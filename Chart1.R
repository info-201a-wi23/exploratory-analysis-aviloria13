music_df <- read.csv("/Users/miczhang/Desktop/final_pro201/exploratory-analysis-aviloria13/mxmh_survey_results.csv", stringsAsFactors = F)
View(music_df)

library(tidyverse)
library(dplyr)
library(ggplot2)

# Different results that came with the effect of music: Improved, No effect, or worsened 
# Getting the ratio of the effect of music on people

#total responses for every listening hours per day
total_responses <- music_df %>% group_by(Hours.per.day) %>% count(Hours.per.day)
colnames(total_responses) <- c("Hours_per_day", "num_responses")

# responses for "improved" music effect
improved <- music_df %>% group_by(Hours.per.day) %>% count(Music.effects == "Improve")
colnames(improved) <- c("Hours_per_day", "true_false", "num_improved")
improved <- improved %>% filter(true_false == "TRUE")
improved <- improved %>% distinct(Hours_per_day, num_improved)
improved <- left_join(total_responses, improved, by = "Hours_per_day")
improved[is.na(improved)] <- 0
improved <- improved %>% mutate(percentage_improved = (num_improved/num_responses) * 100)

# responses for "worsen" music effect
worsen <- music_df %>% group_by(Hours.per.day) %>% count(Music.effects == "Worsen")
colnames(worsen) <- c("Hours_per_day", "true_false", "num_worsened")
worsen <- worsen %>% filter(true_false == "TRUE")
worsen <- worsen %>% distinct(Hours_per_day, num_worsened)
worsen <- left_join(total_responses, worsen, by = "Hours_per_day")
worsen[is.na(worsen)] <- 0
worsen <- worsen %>% mutate(percentage_worsened = (num_worsened/num_responses) * 100)

# responses for no music effect
no_effect <- music_df %>% group_by(Hours.per.day) %>% count(Music.effects == "No effect")
colnames(no_effect) <- c("Hours_per_day", "true_false", "num_no_effect")
no_effect <- no_effect %>% filter(true_false == "TRUE")
no_effect <- no_effect %>% distinct(Hours_per_day, num_no_effect)
no_effect <- left_join(total_responses, no_effect, by = "Hours_per_day")
no_effect[is.na(no_effect)] <- 0
no_effect <- no_effect %>% mutate(percentage_no_effect = (num_no_effect/num_responses) * 100)

#graph
music_effects <- left_join(total_responses, improved, by = "Hours_per_day")
music_effects <- left_join(music_effects, worsen, by = "Hours_per_day")
music_effects <- left_join(music_effects, no_effect, by = "Hours_per_day")

ggplot(music_effects) + 
  geom_point(mapping = aes(x = Hours_per_day, y = percentage_improved, color = "pink")) + 
  geom_line(mapping = aes(x = Hours_per_day, y = percentage_improved, color = "pink")) +
  geom_point(mapping = aes(x = Hours_per_day, y = percentage_worsened, color = "blue")) + 
  geom_line(mapping = aes(x = Hours_per_day, y = percentage_worsened, color = "blue")) +
  geom_point(mapping = aes(x = Hours_per_day, y = percentage_no_effect, color = "green")) + 
  geom_line(mapping = aes(x = Hours_per_day, y = percentage_no_effect, color = "green")) +
  labs(title = "Hours of music and its effects", x = "Hours", y = "Percentage", color = "Music Effects")


