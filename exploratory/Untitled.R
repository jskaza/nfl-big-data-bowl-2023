library(vroom)
library(fs)
library(dplyr)
library(stringr)

distance <- function(x1, y1, x2, y2) {
  sqrt((x2 - x1) ^ 2 + (y2 - y1) ^ 2)
}

files <- dir_ls(glob = "data/week*csv", recurse = T)
tracking <- vroom(files)

pff <- vroom("data/pffScoutingData.csv")
# table(tracking$event)
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

x = tracking |>
  left_join(pff, by = c("gameId", "playId", "nflId")) |>
  group_by(gameId, playId) |>
  mutate(
    qb_start_x = x[pff_positionLinedUp == "QB" &
                     str_detect(event, "ball_snap")][1],
    qb_start_y = y[pff_positionLinedUp == "QB" &
                     str_detect(event, "ball_snap")][1],
    qb_end_x = x[pff_positionLinedUp == "QB" &
                   event %in% terminal_events][1],
    qb_end_y = y[pff_positionLinedUp == "QB" &
                   event %in% terminal_events][1]
  ) |>
  filter(pff_role == "Pass Rush") |>
  group_by(gameId, playId, nflId) |>
  mutate(
    start_x = x[str_detect(event, "ball_snap")][1],
    start_y = y[str_detect(event, "ball_snap")][1],
    end_x = x[event %in% terminal_events][1],
    end_y = y[event %in% terminal_events][1]
  ) |>
  # filter(event == "qb_sack") |>
  ungroup() |>
  mutate(
    dist_from_qb_start = distance(start_x, start_y, qb_start_x, qb_start_y),
    dist_from_qb_end =  distance(end_x, end_y, qb_end_x, qb_end_y)
  ) |>
  mutate(distance_gained = dist_from_qb_start - dist_from_qb_end) #|> 
  # select(dist_from_qb_end, pff_sack)


x |> 
  group_by(pff_sack) |>
  summarise(mean(dist_from_qb_end, na.rm=T))

x |> 
  group_by(pff_hurry) |>
  summarise(mean(dist_from_qb_end, na.rm=T))

x |> 
  group_by(pff_hit) |>
  summarise(mean(dist_from_qb_end, na.rm=T))




pff |> 
  group_by(gameId, playId) |> 
  count(pff_nflIdBlockedPlayer) |> 
 filter(n == 2)
  ungroup()