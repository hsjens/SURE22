# PURPOSE: to examine NHL data and answer key questions


# QUESTION 1: Is there a difference between left handed and right handed shooters?
# QUESTION 2: Is there a relationship between shot type and the distance of the shot?
# QUESTION 3: Examine home and away shot data, and see if players have a difference of how many shots they can generate at home vs away. We can also see the amount they generate per game and examine who the better offensive players are.

# Load in data
library(tidyverse)
playoff_shot_data <- read_csv("~/R/CMU labs/nhl_playoffs_shots_2022 (1).csv")

# Show NA (if any)
is.na(playoff_shot_data$team)
which(is.na(playoff_shot_data$goalieNameForShot))


# Question 1 --------------------------------------------------------------

# assign left vs right handed shooter
playoff_shot_data$shooterLeftRight <- factor(playoff_shot_data$shooterLeftRight, 
                                             labels = c("Left Handed Shooter", "Right Handed Shooter"))

# show heat map of shots taken over hockey rink
library(sportyR)
playoff_shot_data$shooterLeftRight <- factor(playoff_shot_data$shooterLeftRight, labels = c("Left Handed Shooter", "Right Handed Shooter"))
nhl_shots_filter <- playoff_shot_data %>% filter(arenaAdjustedYCord < 41, arenaAdjustedYCord > -41) %>% mutate(absXCoord = -abs(arenaAdjustedXCord))
geom_hockey(league = "NHL", full_surf = F) + 
  stat_density2d(data = nhl_shots_filter,
                 adjust = 0.5,
                 alpha = 0.5,
                 h = 10,
                 aes(x = absXCoord,
                     y = arenaAdjustedYCord,
                     fill = after_stat(level)),
                 geom = "polygon") +
  scale_fill_gradient(low = "cornflowerblue",
                      high = "darkred") +
  facet_wrap(~ shooterLeftRight, ncol = 2) +
  labs(title = "Differences in Position on the Ice Both Handed Players Shoot",
       caption = "Data Courtesy of MoneyPuck.com") + 
  theme(legend.position = "bottom")

# Question 2 --------------------------------------------------------------

# find average shot distance
mx <- mean(playoff_shot_data$shotDistance)

# histogram showing shot type and distance compared to avg shot distance
playoff_shot_data%>%
  ggplot(aes(x = shotDistance))+
  geom_histogram(bins = 15, 
                 color = "cornflowerblue", 
                 fill = "cornflowerblue",
                 alpha = .22, 
                 size = .65) +
  geom_vline(xintercept= mx,linetype="88", color = "darkred") +
  labs(title = "How Type of Shot Varies by Distance",
       x = "Distance Away from Goal (in feet)",
       y = "Frequency of Type of Shot",
       caption = "Data Courtesy of Moneypuck.com") +
  theme_bw() +
  facet_wrap(~ shotType, ncol = 1)


# Question 3 --------------------------------------------------------------

# Load shooting data for away players
nhl_shots_away_players <- playoff_shot_data %>% 
  group_by(shooterName) %>% 
  filter(isHomeTeam == 0) %>% 
  summarize(away_shots = sum(event %in% c("GOAL", "SHOT", "MISS")),
            away_goals = sum(event == "GOAL"),
            away_games = n_distinct(game_id)) %>%
  mutate(away_shots_per_game = round(away_shots / away_games, 2), 
         away_goals_per_game = round(away_goals / away_games, 2),
         away_shooting_percentage = round(away_goals / away_shots, 4))

# Load shooting data for home players
nhl_shots_home_players <- playoff_shot_data %>% 
  group_by(shooterName) %>%
  filter(isHomeTeam == 1) %>% 
  summarize(home_shots = sum(event %in% c("GOAL", "SHOT", "MISS")),
            home_goals = sum(event == "GOAL"),
            home_games = n_distinct(game_id)) %>%
  mutate(home_shots_per_game = round(home_shots / home_games, 2),
         home_goals_per_game = round(home_goals / home_games, 2),
         home_shooting_percentage = round(home_goals / home_shots, 4))

# Merge the data sets together into one
nhl_player_shooting <- merge(nhl_shots_away_players,
                             nhl_shots_home_players,
                             by = "shooterName")

# K means clustering ------------------------------------------------------

# Make cluster for home and away shots on goal data per game using k means
library(flexclust)
set.seed(12)
init_kmeanspp <- 
  kcca(dplyr::select(nhl_player_shooting,
                     away_shots_per_game, 
                     home_shots_per_game), 
       k = 6,
       control = list(initcent = "kmeanspp"))
nhl_player_shooting %>%
  mutate(nhl_player_clusters = 
           as.factor(init_kmeanspp@cluster)) %>%
  ggplot(aes(x = away_shots_per_game, 
             y = home_shots_per_game,
             color = nhl_player_clusters)) +
  geom_point() + 
  ggthemes::scale_color_colorblind() +
  geom_hline(yintercept = mean(nhl_player_shooting$away_shots_per_game), 
             linetype = "dashed", 
             color = "purple",
             size = 2,
             alpha = 0.3) +
  geom_vline(xintercept = mean(nhl_player_shooting$home_shots_per_game), 
             linetype = "dashed", 
             color = "purple",
             size = 2,
             alpha = 0.3) +
  theme_bw() +
  theme(legend.position = "bottom")

# Doing Hierarchical clustering using home and away shots by player-----------

# Compute the Euclidean distance
player_shot_compare_dist <- dist(dplyr::select(nhl_player_shooting, away_shots_per_game, home_shots_per_game))

# Get the complete linkage information for these variables
nhl_shots_hclust_complete <- hclust(player_shot_compare_dist, method = "complete")

# Determine the number of clusters I should use
library(ggdendro)
ggdendrogram(nhl_shots_hclust_complete, 
             theme_dendro = FALSE, 
             labels = FALSE, 
             leaf_labels = FALSE) +
  labs(y = "Dissimilarity between clusters") +
  theme_bw() +
  theme(axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank())
# From this plot I believe 6 clusters is the best since the drop is not very steep / as much distance away

# Make standard deviations for each of the home and away shots
nhl_player_shooting <- nhl_player_shooting %>%
  mutate(std_away_shots_per_game = 
           as.numeric(scale(away_shots_per_game, 
                            center = TRUE, 
                            scale = TRUE)),
         std_home_shots_per_game = 
           as.numeric(scale(home_shots_per_game, 
                            center = TRUE, 
                            scale = TRUE)))

# Make cluster labels and plot for complete linkage
library(ggrepel)
nhl_player_shooting %>%
  mutate(shooting_clusters =
           as.factor(cutree(nhl_shots_hclust_complete,
                            k = 5))) %>%
  ggplot(aes(x = away_shots_per_game, 
             y = home_shots_per_game, 
             color = shooting_clusters)) +
  geom_label_repel(aes(label = ifelse((std_away_shots_per_game >= 3) | (std_home_shots_per_game >= 3), as.character(shooterName), '')), 
                   #Label all players who are at least 3 standard deviations above the average for either the home and away shots
                   box.padding = 0.35,
                   point.padding = 0.5,
                   segment.color = 'grey50',
                   size = 3,
                   color = "brown") +
  geom_label_repel(aes(label = ifelse((std_away_shots_per_game >= 3) & (std_home_shots_per_game >= 3), as.character(shooterName), '')),
                   #Label all players who are at least 3 standard deviations above the average for either the home or away shots but not both
                   box.padding = 0.35,
                   point.padding = 0.5,
                   segment.color = 'grey50',
                   size = 3,
                   color = "purple") +
  geom_point(alpha = .7) +
  ggthemes::scale_color_colorblind() +
  labs(title = "Grouping players by their offensive shot output in home and away settings",
       x = "Amount of away shots per game",
       y = "Amount of home shots per game",
       color = "Player Shooting Clusters",
       caption = "Data courtesy of Moneypuck.com") +
  theme_bw() +
  theme(legend.position = "bottom")
