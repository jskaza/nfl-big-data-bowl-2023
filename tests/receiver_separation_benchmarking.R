library(microbenchmark)
library(data.table)
library(dplyr)
library(tidyr)
library(Rcpp)
sourceCpp("min_dists.cpp")

frame <- readRDS("data/sample_frame_data.Rds")

distance <- function(x1, y1, x2, y2) {
  sqrt((x2 - x1) ^ 2 + (y2 - y1) ^ 2)
}


# the most expensive part of the preprocessing is extracting the distance
# from each receiver to the nearest defender in each frame
# i tried three methods for this

method_1 <- function(df) {
  expand_grid(player_1 = df$nfl_id, player_2 = df$nfl_id) |>
    filter(player_1 != player_2 & player_2 != 0) |>
    left_join(df, by = c("player_1" = "nfl_id")) |>
    left_join(
      select(df, nfl_id, x, y, team),
      by = c("player_2" = "nfl_id"),
      suffix = c("_1", "_2")
    ) |>
    filter(pff_role == "Pass Route" & team_1 != team_2) |>
    mutate(dist = distance(x_1, y_1, x_2, y_2)) |>
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


method_2 <- function(df) {
  # we know there will be at least one rusher
  defense <- df$team[df$pff_role == "Pass Rush"][[1]]
  
  dt <- as.data.table(df)[order(nfl_id), ]
  dt1 <- dt[pff_role == "Pass Route", list(x, y)]
  dt2 <- dt[team == defense, list(x, y)]
  
  # pre-allocate vector
  gaps <- vector(mode = "numeric", length = nrow(dt1) + 3)
  names(gaps) <-
    c(paste0("receiver_sep_", 1:nrow(dt1)),
      "game_id",
      "play_id",
      "frame_id")
  gaps[(nrow(dt1) + 1):(nrow(dt1) + 3)] = c(df$game_id[[1]], df$play_id[[1]], df$frame_id[[1]])
  for (i in 1:nrow(dt1)) {
    gaps[i] <- min(distance(dt1[i]$x, dt1[i]$y, dt2$x, dt2$y))
  }
  
  return(gaps)
}

method_3 <- function(df) {
  # we know there will be at least one rusher
  defense <- df$team[df$pff_role == "Pass Rush"][[1]]
  
  df <- df[order(df$nfl_id), ]
  x1 <- subset(df, pff_role == "Pass Route")$x
  y1 <- subset(df, pff_role == "Pass Route")$y
  x2 <- subset(df, team == defense)$x
  y2 <- subset(df, team == defense)$y
  
  gaps <-
    c(min_dists(x1, y1, x2, y2),
      df$game_id[[1]],
      df$play_id[[1]],
      df$frame_id[[1]])
  names(gaps) <-
    c(paste0("receiver_sep_", 1:length(x1)),
      "game_id",
      "play_id",
      "frame_id")
  
  return(gaps)
}

# outputs slightly differ, but are numerically equivalent
method_1(frame)
method_2(frame)
method_3(frame)

microbenchmark(
  expand_grid = method_1(frame),
  dt = method_2(frame),
  cpp = method_3(frame),
  times = 1000
)
