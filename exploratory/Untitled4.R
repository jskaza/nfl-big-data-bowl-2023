library(vroom)
library(fs)
library(dplyr)
library(stringr)
library(tidyr)
library(fuzzyjoin)
library(purrr)
library(furrr)
# library(progressr)
future::plan(multisession, workers = 4)

distance <- function(x1, y1, x2, y2) {
  sqrt((x2 - x1) ^ 2 + (y2 - y1) ^ 2)
}

terminal_events <-
  c(
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
files <- dir_ls(glob = "data/week*csv", recurse = T)
pff <- vroom("data/pffScoutingData.csv")

# tracking <- vroom(files) |>
#   #micah parsons
#   # filter(nflId == 53441) |>
#   group_by(gameId, playId) |>
#   mutate(snap = frameId[str_detect(event, "ball_snap")][1],
#          end_play = frameId[event %in% terminal_events][1]) |>
#   filter(frameId >= snap & frameId <= end_play) |>
#   ungroup() |>
#   group_by(gameId, playId, frameId) |>
#   mutate()

# function standardize_coord(x, ball){
#   return(x - ball)
# }
#
#
# function transpose_coord(x, ball){
#   return(-x)
# }

tracking <- vroom(files)

tracking_a <- tracking |>
  group_by(gameId, playId) |>
  mutate(snap = frameId[str_detect(event, "ball_snap")][1],
         end_play = frameId[event %in% terminal_events][1]) |>
  filter(frameId >= snap & frameId <= end_play) |>
  group_by(gameId, playId, frameId) |>
  mutate(
    ball_x = x[is.na(nflId)][1],
    ball_y = y[is.na(nflId)][1],
    # need to get coords of blockers
    # maybe distances too
    # teammates_x = map(team, \(z) list(x[team == z]))
  ) |> 
  ungroup()

# mutate(
#   ball_x = x[is.na(nflId)][1],
#   ball_y = y[is.na(nflId)][1],
# relative to player?
# standard_y = if_else(playDirection == "right", -(x - ball_x), x - ball_x),
# standard_x = if_else(playDirection == "left", -(y - ball_y), y - ball_y)

# )
tracking_a$test_ave <- ave(tracking_a$x,  tracking_a$frameId,  tracking_a$gameId,  tracking_a$playId, tracking_a$team != tracking_a$team, FUN = \(x) as.list(x))

#
#
# ungroup() |>
# group_by(gameId, playId, frameId) |>
# mutate()