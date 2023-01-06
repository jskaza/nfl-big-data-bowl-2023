library(vroom)
library(dplyr)
library(stringr)
library(snakecase)
library(tidyr)
library(purrr)
library(fs)
library(jsonlite)
# library(data.table)
# library(Rcpp)

# sourceCpp("min_dists.cpp")

files <-
  dir_ls(paste0(Sys.getenv("BIG_DATA_BOWL"), "/data"), regexp = "week.*.csv")
tracking <- vroom(files)
names(tracking) <- to_snake_case(names(tracking))

pff <-
  vroom(paste0(Sys.getenv("BIG_DATA_BOWL"), "/data/pffScoutingData.csv"))
names(pff) <- to_snake_case(names(pff))

players <-
  vroom(paste0(Sys.getenv("BIG_DATA_BOWL"), "/data/players.csv"))
names(players) <- to_snake_case(names(players))

games <-
  vroom(paste0(Sys.getenv("BIG_DATA_BOWL"), "/data/games.csv"))
names(games) <- to_snake_case(names(games))

# parser guesses wrong for cols 20-21
plays <-
  vroom(paste0(Sys.getenv("BIG_DATA_BOWL"), "/data/plays.csv"))
names(plays) <- to_snake_case(names(plays))

distance <- function(x1, y1, x2, y2) {
  sqrt((x2 - x1) ^ 2 + (y2 - y1) ^ 2)
}
# vectorized_distance <- Vectorize(distance)

# closest_opponent <- function(df) {
#   # we know there will be at least one rusher
#   defense <- df$team[df$pff_role == "Pass Rush"][[1]]
#
#   df <- df[order(df$nfl_id), ]
#   x1 <- subset(df, pff_role == "Pass Route")$x
#   y1 <- subset(df, pff_role == "Pass Route")$y
#   x2 <- subset(df, team == defense)$x
#   y2 <- subset(df, team == defense)$y
#
#   gaps <-
#     c(min_dists(x1, y1, x2, y2),
#       df$game_id[[1]],
#       df$play_id[[1]],
#       df$frame_id[[1]])
#   names(gaps) <-
#     c(paste0("receiver_sep_", 1:length(x1)),
#       "game_id",
#       "play_id",
#       "frame_id")
#
#   return(gaps)
# }

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
      pff_hit,
      pff_hurry,
      pff_position_lined_up
    ),
    by = c("game_id", "play_id", "nfl_id")
  )  |>
  mutate(havoc = as.numeric(pff_sack == 1 |
                              pff_hit == 1 | pff_hurry == 1)) |>
  select(-pff_hit, -pff_hurry) |>
  group_by(game_id, play_id) |>
  # remove plays with no rushers
  filter(any(pff_role == "Pass Rush")) |>
  mutate(snap_id = frame_id[str_detect(event, "snap")][1],
         end_id = frame_id[event %in% terminal_events][1]) |>
  filter(frame_id >= snap_id & frame_id <= end_id) |>
  # replace_na(list(nfl_id = 0)) |> # the ball
  mutate(
    ball_start_x = x[team == "football"][1],
    ball_start_y = y[team == "football"][1],
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
    dist_from_qb = distance(x, y, qb_x, qb_y),
    qb_in_tackle_box = if_else(qb_y > tackle_box_right &
                                 qb_y < tackle_box_left, 1, 0),
    n_blockers = sum(pff_role == "Pass Block", na.rm = T)
  ) |>
  select(
    -qb_x,-qb_y,-event,-snap_id,-end_id,-play_direction,-tackle_box_right,-tackle_box_left
  )

tracking <- NULL # free up some mem

blockers <- by_frame |>
  ungroup() |>
  group_by(game_id, play_id) |>
  filter(pff_position_lined_up %in% c("LT", "LG", "C", "RG", "RT")) |>
  select(game_id, play_id, pff_position_lined_up, frame_id, x, y) |>
  pivot_wider(names_from = pff_position_lined_up,
              values_from = c(x, y)) |>
  ungroup()

ball_coords <- by_frame |>
  ungroup() |>
  filter(team == "football") |>
  select(game_id, play_id, frame_id, x, y) |>
  rename(x_ball = x, y_ball = y)

qb <- by_frame |>
  ungroup() |>
  filter(pff_role == "Pass") |>
  select(game_id, play_id, frame_id, speed, x, y) |>
  rename(speed_qb = speed,
         x_qb = x,
         y_qb = y)

# receiver_sep <- by_frame |>
#   group_split() |>
#   map_dfr(closest_opponent, .progress = T)

by_frame |>
  ungroup() |>
  filter(pff_role == "Pass Rush") |>
  left_join(blockers, by = c("game_id", "play_id", "frame_id")) |>
  left_join(qb, by = c("game_id", "play_id", "frame_id")) |>
  left_join(ball_coords, by = c("game_id", "play_id", "frame_id")) |>
  # left_join(receiver_sep, by = c("game_id", "play_id", "frame_id")) |>
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
  select(
    -team,-pff_role,-home_team_abbr,-pre_snap_home_score,-pre_snap_visitor_score,-frame_id,-pff_position_lined_up
  ) |>
  write.csv(paste0(Sys.getenv("BIG_DATA_BOWL"), "/data/dataset.csv"))
