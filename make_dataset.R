library(vroom)
library(dplyr)
library(stringr)
library(snakecase)
library(tidyr)
library(purrr)
library(fs)
library(furrr)
library(future)
library(jsonlite)

plan(multisession, workers = 4)

files <-
  dir_ls(paste0(Sys.getenv("BIG_DATA_BOWL"), "data"), regexp = "week")
tracking <- vroom(files)
names(tracking) <- to_snake_case(names(tracking))

pff <-
  vroom(paste0(Sys.getenv("BIG_DATA_BOWL"), "data/pffScoutingData.csv"))
names(pff) <- to_snake_case(names(pff))

players <-
  vroom(paste0(Sys.getenv("BIG_DATA_BOWL"), "data/players.csv"))
names(players) <- to_snake_case(names(players))

games <-
  vroom(paste0(Sys.getenv("BIG_DATA_BOWL"), "data/games.csv"))
names(games) <- to_snake_case(names(games))

# parser guesses wrong for cols 20-21
plays <- vroom("data/plays.csv")
names(plays) <- to_snake_case(names(plays))

distance <- function(coord1, coord2) {
  sqrt((coord2[[1]] - coord1[[1]]) ^ 2 + (coord2[[2]] - coord1[[2]]) ^ 2)
}
vectorized_distance <- Vectorize(distance)

expand_frame <- function(df) {
  expand_grid(player_1 = df$nfl_id, player_2 = df$nfl_id) |>
    filter(player_1 != player_2) |>
    left_join(df, by = c("player_1" = "nfl_id")) |>
    left_join(
      select(df, nfl_id, coords, team),
      by = c("player_2" = "nfl_id"),
      suffix = c("_1", "_2")
    ) |>
    filter(pff_role == "Pass Rush") |>
    mutate(
      object = if_else(
        player_2 == 0,
        "ball",
        if_else(team_1 != team_2, "opponent",
                "teammate")
      ),
      dist = vectorized_distance(coords_1, coords_2)
    ) |>
    group_by(player_1) |>
    arrange(object, dist)
}

consolidate_frame <- function(df) {
  opponent_coords <- df$coords_2[df$object == "opponent"]
  teammate_coords <- df$coords_2[df$object == "teammate"]
  ball_coords <- df$coords_2[df$object == "ball"]
  
  return(
    tibble(
      nfl_id = df$player_1[1],
      opponent_coords = list(opponent_coords),
      teammate_coords = list(teammate_coords),
      ball_coords = ball_coords
    )
  )
}

terminal_events <- c(
  "autoevent_passforward",
  "autoevent_passinterrupted",
  "dropped_pass",
  "fumble",
  "fumble_offense_recovered",
  "pass_arrived",
  "pass_forward",
  "pass_tipped",
  "qb_sack",
  "qb_strip_sack"
)

tracking |>
  select(game_id,
         play_id,
         nfl_id,
         frame_id,
         team,
         play_direction,
         x,
         y,
         event) |>
  group_by(game_id, play_id) |>
  mutate(snap_id = frame_id[str_detect(event, "ball_snap")][1],
         end_id = frame_id[event %in% terminal_events][1]) |>
  filter(frame_id >= snap_id & frame_id <= end_id) |>
  replace_na(list(nfl_id = 0)) |> # the ball
  mutate(
    # should coordinates be relative to each player?
    ball_start_x = x[nfl_id == 0][1],
    ball_start_y = y[nfl_id == 0][1],
    standard_y = if_else(play_direction == "right", -(x - ball_start_x), x - ball_start_x),
    standard_x = if_else(play_direction == "left", -(y - ball_start_y), y - ball_start_y),
    coords = map2(standard_x, standard_y, \(x, y) c(x, y))
  ) |>
  select(-x, -y, -ball_start_x, -ball_start_y, -standard_x, -standard_y) |>
  ungroup() |>
  left_join(select(pff, game_id, play_id, nfl_id, pff_role),
            by = c("game_id", "play_id", "nfl_id")) |>
  filter(pff_role %in% c("Pass Block", "Pass Rush") |
           nfl_id == 0) |>
  group_by(game_id, play_id, frame_id) |>
  # remove plays with no rushers
  filter(any(pff_role == "Pass Rush")) |>
  group_split() |>
  future_map(
    \(x) expand_frame(x) |>
      group_split() |>
      map_dfr(consolidate_frame) |>
      left_join(x, by = "nfl_id"),
    .progress = T
  ) |>
  bind_rows() |>
  left_join(
    select(pff, pff_position_lined_up, game_id, play_id, nfl_id),
    by = c("game_id", "play_id", "nfl_id")
  ) |>
  left_join(select(games, home_team_abbr, game_id), by = c("game_id")) |>
  left_join(
    select(
      plays,
      quarter,
      down,
      yards_to_go,
      # game_clock,
      pre_snap_home_score,
      pre_snap_visitor_score,
      absolute_yardline_number,
      personnel_o,
      defenders_in_box,
      offense_formation,
      game_id,
      play_id
    ),
    by = c("game_id", "play_id")
  ) |>
  mutate(
    score_delta = if_else(
      team == home_team_abbr,
      pre_snap_home_score - pre_snap_visitor_score,
      pre_snap_visitor_score - pre_snap_home_score
    )
  ) |>
  select(
    -team,-play_direction,-event,-snap_id,-end_id,-pff_role,-home_team_abbr,-pre_snap_home_score,-pre_snap_visitor_score
  ) |>
  toJSON() |>
  write(paste0(Sys.getenv("BIG_DATA_BOWL"), "data/dataset.json"))
