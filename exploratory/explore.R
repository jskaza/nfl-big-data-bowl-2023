library(readr)
library(vroom)
library(dplyr)
library(tidymodels)
library(tidyr)
library(fs)

files <- dir_ls(glob = "data/week*csv", recurse=T)
tracking <- vroom(files)

# week1 <- read_csv("data/week1.csv")
plays <- read_csv("data/plays.csv")
pff <- read_csv("data/pffScoutingData.csv")

playTimes <- tracking |>
  group_by(gameId, playId) |>
  summarise(timeElapsed = max(time) - min(time))

vars <- c(
  "prePenaltyPlayResult",
  "yardsToGo",
  "down",
  "offenseFormation",
  "personnelO",
  "defendersInBox",
  "personnelD",
  "dropBackType",
  "pff_playAction",
  "pff_passCoverage",
  "pff_passCoverageType",
  "nRushers",
  "passResult"
)

df <- pff |>
  select(gameId, playId, pff_role) |>
  left_join(playTimes, by = c("gameId", "playId")) |>
  left_join(plays, by = c("gameId", "playId")) |>
  mutate(isPassRusher = pff_role == "Pass Rush") |>
  select(-pff_role) |>
  group_by(gameId, playId) |>
  mutate(nRushers = sum(isPassRusher, na.rm = T)) |>
  select(-isPassRusher) |>
  distinct(gameId, playId, .keep_all = T) |>
  drop_na(vars)


rec <- recipe(df, vars = vars) |> 
  update_role(everything(), new_role = "predictor") |> 
  update_role("prePenaltyPlayResult", new_role = "outcome") |>
  step_dummy(all_nominal_predictors(), one_hot = T) |> 
  prep(training = df)
summary(rec)
rf_defaults <- rand_forest(mode = "regression")

rf <-
  rf_defaults %>%
  set_engine("ranger") %>%
  fit(prePenaltyPlayResult ~ ., data = bake(rec, new_data = NULL))
  # fit_xy(x = df[, preds],
  #        y = df$prePenaltyPlayResult)
# rf_workflow <-
#   workflow() %>% 
#   add_model(lr_mod) %>% 
#   add_recipe(flights_rec)



rf$variable.importance

