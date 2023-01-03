library(vroom)
library(dplyr)
library(stringr)
library(snakecase)
library(tidyr)
library(purrr)
library(fs)
library(purrr)
library(jsonlite)

files <-
  dir_ls(paste0(Sys.getenv("BIG_DATA_BOWL"), "data"), regexp = "week.*.csv")
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
plays <-
  vroom(paste0(Sys.getenv("BIG_DATA_BOWL"), "data/plays.csv"))
names(plays) <- to_snake_case(names(plays))

distance <- function(x1, y1, x2, y2) {
  sqrt((x2 - x1) ^ 2 + (y2 - y1) ^ 2)
}
vectorized_distance <- Vectorize(distance)

closest_opponent <- function(df) {
  expand_grid(player_1 = df$nfl_id, player_2 = df$nfl_id) |>
    filter(player_1 != player_2) |>
    left_join(df, by = c("player_1" = "nfl_id")) |>
    left_join(
      select(df, nfl_id, x, y, team),
      by = c("player_2" = "nfl_id"),
      suffix = c("_1", "_2")
    ) |>
    filter(pff_role == "Pass Route" & team_1 != team_2) |>
    mutate(dist = vectorized_distance(x_1, y_1, x_2, y_2)) |>
    group_by(player_1) |>
    summarise(closest_defender = min(dist)) |>
    arrange(player_1) |>
    mutate(player_1 = paste0("receiver_sep_", 1:length(player_1))) |>
    pivot_wider(names_from = player_1, values_from = closest_defender) |>
    mutate(
      game_id = df$game_id[1],
      play_id = df$play_id[1],
      frame_id = df$frame_id[1]
    )
  
}

# we will consider defensive players as active rushers from the snap up until the earliest of these events
terminal_events <- c(
  "autoevent_passforward",
  "autoevent_passinterrupted",
  "dropped_pass",
  "fumble",
  "fumble_offense_recovered",
  "pass_forward",
  "pass_tipped",
  "qb_sack",
  "qb_strip_sack"
)

by_frame <- tracking |>
  select(game_id,
         play_id,
         nfl_id,
         frame_id,
         team,
         play_direction,
         x,
         y,
         s,
         event) |>
  rename(speed = s) |>
  left_join(
    select(
      pff,
      game_id,
      play_id,
      nfl_id,
      pff_role,
      pff_sack,
      pff_position_lined_up
    ),
    by = c("game_id", "play_id", "nfl_id")
  )  |>
  group_by(game_id, play_id) |>
  # remove plays with no rushers
  filter(any(pff_role == "Pass Rush")) |>
  mutate(snap_id = frame_id[str_detect(event, "snap")][1],
         end_id = frame_id[event %in% terminal_events][1]) |>
  filter(frame_id >= snap_id & frame_id <= end_id) |>
  replace_na(list(nfl_id = 0)) |> # the ball
  mutate(
    ball_start_x = x[nfl_id == 0][1],
    ball_start_y = y[nfl_id == 0][1],
    standard_x = if_else(play_direction == "left", -(x - ball_start_x), x - ball_start_x),
    standard_y = if_else(play_direction == "left", -(y - ball_start_y), y - ball_start_y),
    tackle_box_left = standard_y[pff_position_lined_up == "LT"][1],
    tackle_box_right = standard_y[pff_position_lined_up == "RT"][1]
  ) |>
  select(-ball_start_x, -ball_start_y, -x, -y) |>
  rename(x = standard_x, y = standard_y) |>
  ungroup() |>
  group_by(game_id, play_id, frame_id) |>
  mutate(
    qb_x = x[pff_role == "Pass"][1],
    qb_y = y[pff_role == "Pass"][1],
    dist_from_qb = vectorized_distance(x, y, qb_x, qb_y),
    qb_in_tackle_box = qb_y > tackle_box_right &&
      qb_y < tackle_box_left
  ) |>
  select(
    -qb_x,-qb_y,-event,-snap_id,-end_id,-play_direction,-pff_position_lined_up,-tackle_box_right,-tackle_box_left
  )


blocker_coords <- by_frame |>
  ungroup() |>
  filter(pff_role == "Pass Block") |>
  select(game_id, play_id, nfl_id, frame_id, x, y) |>
  rename(x_blocker = x, y_blocker = y) |>
  mutate(nfl_id = as.numeric(as.factor(nfl_id))) |>
  pivot_wider(names_from = nfl_id,
              values_from = c(x_blocker, y_blocker))

ball_coords <- by_frame |>
  ungroup() |>
  filter(nfl_id == 0) |>
  select(game_id, play_id, frame_id, x, y) |>
  rename(x_ball = x, y_ball = y)

qb <- by_frame |>
  ungroup() |>
  filter(pff_role == "Pass") |>
  select(game_id, play_id, frame_id, speed, x, y) |>
  rename(speed_qb = speed,
         x_qb = x,
         y_qb = y)

receiver_sep <- by_frame |>
  group_split() |>
  map_dfr(closest_opponent, .progress = T)

by_frame |>
  ungroup() |>
  filter(pff_role == "Pass Rush") |>
  left_join(blocker_coords, by = c("game_id", "play_id", "frame_id")) |>
  left_join(qb, by = c("game_id", "play_id", "frame_id")) |>
  left_join(ball_coords, by = c("game_id", "play_id", "frame_id")) |>
  left_join(receiver_sep, by = c("game_id", "play_id", "frame_id")) |>
  left_join(select(games, home_team_abbr, game_id), by = c("game_id")) |>
  left_join(
    select(
      plays,
      quarter,
      down,
      yards_to_go,
      pre_snap_home_score,
      pre_snap_visitor_score,
      absolute_yardline_number,
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
  select(-team,-pff_role,-home_team_abbr,-pre_snap_home_score,-pre_snap_visitor_score)  |>
  toJSON() |>
  write(paste0(Sys.getenv("BIG_DATA_BOWL"), "data/dataset.json"))
