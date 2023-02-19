library("dplyr")
library("stringr")
library("ggplot2")
library("scales")
library("tidyverse")

project_df <- read.csv("/Users/aaliyahviloria/Desktop/INFO201/exploratory-analysis-aviloria13/mxmh_survey_results.csv", stringsAsFactors = FALSE)

# How many responses are in this dataset?
num_responses <- nrow(project_df)

# How many groups of listening hours are in this dataset?
listening_groups <- project_df %>% group_by(Hours.per.day) %>% count(Hours.per.day)
num_listening_groups <- nrow(listening_groups)

# What percentage of respondents listen to music while working/studying?
while_working <- project_df %>% count(true_false = While.working == "Yes")
while_working <- while_working %>% filter(true_false == "TRUE")
num_while_working <- while_working %>% pull(n)

# What is the most common genre of music respondents listen to?
common_genre <- names(which.max(table(project_df$Fav.genre)))

# What is the percentage of improved health? worsened? no effect?
improved_responses <- project_df %>% count(true_false = Music.effects == "Improve")
improved_responses <- improved_responses %>% filter(true_false == "TRUE")
num_improved <- improved_responses %>% pull(n)
percent_improved <- round((num_improved / num_responses) * 100, digits = 2)

worsen_responses <- project_df %>% count(true_false = Music.effects == "Worsen")
worsen_responses <- worsen_responses %>% filter(true_false == "TRUE")
num_worsened <- worsen_responses %>% pull(n)
percent_worsened <- round((num_worsened / num_responses) * 100, digits = 2)

no_effect_responses <- project_df %>% count(true_false = Music.effects == "No effect")
no_effect_responses <- no_effect_responses %>% filter(true_false == "TRUE")
num_no_effect <- no_effect_responses %>% pull(n)
percent_no_effect <- round((num_no_effect / num_responses) * 100, digits = 2)
