library(vroom)
library(dplyr)
library(stringr)
library(snakecase)
library(tidyr)
library(purrr)
library(fs)
library(furrr)
library(future)
plan(multisession, workers = 4)
# data frame per worker

files <- dir_ls(glob = "data/week*csv", recurse = T)
tracking <- vroom(files)
names(tracking) <- to_snake_case(names(tracking))

pff <- vroom("data/pffScoutingData.csv")
names(pff) <- to_snake_case(names(pff))

players <- vroom("data/players.csv")
names(players) <- to_snake_case(names(players))

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
    # select(a, b, opponent, coords2, dist) |>
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

test <- tracking |>
  select(game_id,
         play_id,
         nfl_id,
         frame_id,
         team,
         play_direction,
         x,
         y,
         event) |>
  mutate(coords = map2(x, y, \(x, y) list(x, y))) |>
  select(-x, -y) |>
  group_by(game_id, play_id) |>
  mutate(snap_id = frame_id[str_detect(event, "ball_snap")][1],
         end_id = frame_id[event %in% terminal_events][1]) |>
  filter(frame_id >= snap_id & frame_id <= end_id) |>
  ungroup() |>
  left_join(
    select(pff, game_id, play_id, nfl_id, pff_role, pff_position_lined_up),
    by = c("game_id", "play_id", "nfl_id")
  ) |>
  replace_na(list(nfl_id = 0)) |> # the ball
  filter(pff_role %in% c("Pass Block", "Pass Rush") |
           nfl_id == 0) |>
  group_by(game_id, play_id, frame_id) |>
  # remove plays with no rushers
  filter(any(pff_role == "Pass Rush")) |>
  group_split()

# filter(game_id == 2021090900, play_id == 97, frame_id == 7)

res <- test |>
  future_map(
    \(x) expand_frame(x) |>
      group_split() |>
      map_dfr(consolidate_frame) |>
      left_join(x, by = "nfl_id"),
    .progress = T
  ) |>
  bind_rows()
# add in play variables after for efficiency?

library(keras)

Y <- res$coords
X <- res$opponent_coords

model <- keras_model_sequential() |> 
  layer_lstm(units = 32, input_shape = c(2)) |> 
  layer_dense(units = 2, activation = 'linear')

# Define the loss function and the optimizer
model |> compile(loss = 'mean_squared_error', optimizer = 'adam')




# add in play variables after for efficiency?
