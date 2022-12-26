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

files <- dir_ls(glob = "data/week1.csv", recurse = T)
# files <- dir_ls(glob = "data/week*csv", recurse = T)
tracking <- vroom(files)

pff <- vroom("data/pffScoutingData.csv")

players <- vroom("data/players.csv")

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

  
  # which blockers (o/d) can be left alone
  play_groups <-
  tracking |>
  left_join(pff, by = c("gameId", "playId", "nflId")) |>
  # chip blocks?
  filter(pff_role %in% c("Pass Block", "Pass Rush")) |>
  group_by(gameId, playId) |>
  mutate(snap = frameId[str_detect(event, "ball_snap")][1],
         end_play = frameId[event %in% terminal_events][1]) |>
  filter(frameId >= snap & frameId <= end_play) |>
  group_by(gameId, playId, frameId) |>
  nest() |>
  ungroup()



lar <- play_groups |>
  future_pmap(
    \(gameId, playId, frameId, data) fuzzy_join(data, data, by = "team", match_fun = (\(y, z) y != z)) |>
      mutate(
        dist = distance(x.x, y.x, x.y, y.y),
        game_id = gameId,
        play_id = playId,
        frame_id = frameId
      ) |>
      select(nflId.x, dist, game_id, play_id, frame_id),
    # group_by(nflId.x) |>
    # # 2 closest people
    # arrange(dist) |>
    # filter(row_number() <= 2) |>
    # ungroup(),
    .progress = T
  )


test = lar |>
  bind_rows() |>
  left_join(players, by = c("nflId.x" = "nflId")) |>
  filter(officialPosition %in% c("DE", "DT", "NT")) |>
  group_by(game_id, play_id, frame_id) |>
  mutate(total_separation = sum(dist)) |>
  group_by(game_id, play_id, frame_id, nflId.x) |>
  summarise(
    player_separation = sum(dist),
    proportion_of_separation = player_separation / first(total_separation)
  ) |>
  group_by(game_id, play_id, nflId.x) |>
  summarise(delta = first(proportion_of_separation) - nth(proportion_of_separation, 4)) |>
  group_by(nflId.x) |>
  summarise(mean_delta = mean(delta, na.rm = T), n = n()) |>
  filter(n > 15) |>
  arrange(-mean_delta)
# filter(case_when(
#   pff_positionLinedUp.x %in% c("DLT", "DRT", "NLT", "NRT", "NT") ~  pff_positionLinedUp.y %in% c("C", "LG", "RG"),
#   T ~ F
# )) |>
# group_by(nflId.x) |>
# summarise(dist = mean(dist, na.rm = T),
#           frames = n()) |>
# filter(frames > 20) |>
# arrange(dist)

# play_groups |>
#   map()
#   fuzzy_join(
#     select(play_groups, gameId, playId, frameId, team, x, y),
#     by = c(
#       "gameId",
#       "playId",
#       "frameId",
#       "team"
#     ),
#     match_fun = list(
#       "gameId" = (\(x, y) x == y),
#       "playId" = (\(x, y) x == y),
#       "frameId" = (\(x, y) x == y),
#       "team" = (\(x, y) x != y)
#     )
#
#   )


# a = play_groups$data |>
#   map(\(x) fuzzy_join(x, x, by = "team", match_fun = (\(y, z) y != z)), .progress = T)

# with_progress({
#   p <- progressor(steps = nrow(play_groups))
#
#   b <- play_groups$data |>
#     future_map(\(x) fuzzy_join(x, x, by = "team", match_fun = (\(y, z) y != z)),
#                p = p)
# })