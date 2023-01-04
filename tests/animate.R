#Loading pre-installed libraries
library(tidyverse)
library(gganimate)
library(cowplot)
library(repr)

PATH <- "/mnt/chromeos/GoogleDrive/SharedWithMe/11241/nfl-big-data-bowl-2023/data"

#global settings
options(warn=-1)
options(repr.plot.width=15, repr.plot.height = 10)

##reading in non-tracking data

df_plays <- read_csv(paste0(PATH,"/plays.csv"),
                     col_types = cols())

df_players <- read_csv(paste0(PATH,"/players.csv"),
                       col_types = cols())


df_pffScouting <- read_csv(paste0(PATH,"/pffScoutingData.csv"),
                           col_types = cols())

head(df_players)
head(df_plays)
head(df_pffScouting)


##Reading tracking data (needs to be done iteratively)

#weeks of NFL season
weeks <- seq(1, 2)

#blank dataframe to store tracking data
df_tracking <- data.frame()

#iterating through all weeks
for(w in weeks){
  
  #temperory dataframe used for reading season for given iteration
  df_tracking_temp <- read.csv(paste0(PATH,"/week",w,".csv"))
  
  #storing temporary dataframe in full season dataframe
  df_tracking <- bind_rows(df_tracking, df_tracking_temp)                            
  
}


#loading command to make NFL field in ggplot (credit to Marschall Furman)
source("https://raw.githubusercontent.com/mlfurman3/gg_field/main/gg_field.R")


#picking the lowest offset play
set.seed(1)

df_examplePlay <- df_plays %>%
  select(gameId, playId, playDescription) %>%
  sample_n(1)

#merging tracking data to play
df_examplePlayTracking <- inner_join(df_examplePlay,
                                     df_tracking,
                                     by = c("gameId" = "gameId",
                                            "playId" = "playId")) %>%
  
  #Standardizing tracking data so its always in direction of offensive team.
  mutate(x = ifelse(playDirection == "left", 120-x, x),
         y = ifelse(playDirection == "left", 160/3 - y, y))

#attributes used for plot. first is away, second is football, third is home.
cols_fill <- c("dodgerblue1", "#663300", "firebrick1")
cols_col <- c("#000000", "#663300", "#000000")
size_vals <- c(6, 4, 6)
shape_vals <- c(21, 16, 21)
plot_title <- df_examplePlay$playDescription
nFrames <- max(df_examplePlayTracking$frameId)

#plotting
anim <- ggplot() +
  
  
  #creating field underlay
  gg_field(yardmin = 65, yardmax = 122) +
  
  #filling forest green for behind back of endzone
  theme(panel.background = element_rect(fill = "forestgreen",
                                        color = "forestgreen"),
        panel.grid = element_blank()) +
  
  
  #setting size and color parameters
  scale_size_manual(values = size_vals, guide = FALSE) + 
  scale_shape_manual(values = shape_vals, guide = FALSE) +
  scale_fill_manual(values = cols_fill, guide = FALSE) + 
  scale_colour_manual(values = cols_col, guide = FALSE) +
  
  
  #adding players
  geom_point(data = df_examplePlayTracking, aes(x = x,
                                                y = y, 
                                                shape = team,
                                                fill = team,
                                                group = nflId,
                                                size = team,
                                                colour = team), 
             alpha = 0.7) +  
  
  #adding jersey numbers
  geom_text(data = df_examplePlayTracking,
            aes(x = x, y = y, label = jerseyNumber),
            colour = "white", 
            vjust = 0.36, size = 3.5) + 
  
  
  #titling plot with play description
  labs(title = plot_title) +
  
  #setting animation parameters
  transition_time(frameId)  +
  ease_aes("linear") + 
  NULL 


#saving animation to display in markdown cell below:
anim_save("./ExamplePlay.gif",
          animate(anim, width = 720, height = 440,
                  fps = 10, nframe = nFrames))
