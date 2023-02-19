library("tidyverse")
library("dplyr")
library("stringr")

# Load in the data
project_df <- read.csv("/Users/aaliyahviloria/Desktop/INFO201/exploratory-analysis-aviloria13/mxmh_survey_results.csv", stringsAsFactors = FALSE)

## Part 1: Number of responses
aggregate <- project_df %>% group_by(Hours.per.day) %>% count(Hours.per.day)

## Part 2: Majority age
# Function to find mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Data frame to get majority age for every listening hours per day
age <- project_df %>% group_by(Hours.per.day) %>% mutate(getmode(Age))
colnames(age)[34] <- "Majority_Age"
age <- age %>% distinct(Hours.per.day, Majority_Age)

## Part 3: Percentages of worsened, improved, and no music effect 
# Total responses for every listening hours per day
total_responses <- project_df %>% group_by(Hours.per.day) %>% count(Hours.per.day)
colnames(total_responses) <- c("Hours_per_day", "num_responses")

# Responses for "improved" music effect
improved <- project_df %>% group_by(Hours.per.day) %>% count(Music.effects == "Improve")
colnames(improved) <- c("Hours_per_day", "true_false", "num_improved")
improved <- improved %>% filter(true_false == "TRUE")
improved <- improved %>% distinct(Hours_per_day, num_improved)
improved <- left_join(total_responses, improved, by = "Hours_per_day")
improved[is.na(improved)] <- 0
improved <- improved %>% mutate(percentage_improved = (num_improved/num_responses) * 100)
improved <- improved %>% distinct(Hours_per_day, percentage_improved)

# Responses for "worsen" music effect
worsen <- project_df %>% group_by(Hours.per.day) %>% count(Music.effects == "Worsen")
colnames(worsen) <- c("Hours_per_day", "true_false", "num_worsened")
worsen <- worsen %>% filter(true_false == "TRUE")
worsen <- worsen %>% distinct(Hours_per_day, num_worsened)
worsen <- left_join(total_responses, worsen, by = "Hours_per_day")
worsen[is.na(worsen)] <- 0
worsen <- worsen %>% mutate(percentage_worsened = (num_worsened/num_responses) * 100)
worsen <- worsen %>% distinct(Hours_per_day, percentage_worsened)

# Responses for no music effect
no_effect <- project_df %>% group_by(Hours.per.day) %>% count(Music.effects == "No effect")
colnames(no_effect) <- c("Hours_per_day", "true_false", "num_no_effect")
no_effect <- no_effect %>% filter(true_false == "TRUE")
no_effect <- no_effect %>% distinct(Hours_per_day, num_no_effect)
no_effect <- left_join(total_responses, no_effect, by = "Hours_per_day")
no_effect[is.na(no_effect)] <- 0
no_effect <- no_effect %>% mutate(percentage_no_effect = (num_no_effect/num_responses) * 100)
no_effect <- no_effect %>% distinct(Hours_per_day, percentage_no_effect)

# Join all music effects
all_music_effects <- left_join(improved, worsen, by = "Hours_per_day")
all_music_effects <- left_join(all_music_effects, no_effect, by = "Hours_per_day")

## Part 4: Majority genre
# Function to find mode
getmajority <- function(u) {
  names(which.max(table(u)))
}

# Data frame to get majority genre for every listening hours per day
genre <- project_df %>% group_by(Hours.per.day) %>% mutate(getmajority(Fav.genre))
colnames(genre)[34] <- "Majority_Genre"
genre <- genre %>% distinct(Hours.per.day, Majority_Genre)
colnames(genre)[1] <- "Hours_per_day"

## Part 5: Percentage listening while working/studying
when_listening <- project_df %>% group_by(Hours.per.day) %>% count(While.working == "Yes")
colnames(when_listening) <- c("Hours_per_day", "true_false", "num_yes")
when_listening <- when_listening %>% filter(true_false == "TRUE")
when_listening <- when_listening %>% distinct(Hours_per_day, num_yes)
when_listening <- left_join(total_responses, when_listening, by = "Hours_per_day")
when_listening[is.na(when_listening)] <- 0
when_listening <- when_listening %>% mutate(percentage_yes = (num_yes/num_responses) * 100)
when_listening <- when_listening %>% distinct(Hours_per_day, percentage_yes)

# Aggregate table
aggregate <- left_join(aggregate, age, by = "Hours.per.day")
colnames(aggregate) <- c("Hours_per_day", "num_responses", "majority_age")
aggregate <- left_join(aggregate, all_music_effects, by = "Hours_per_day")
aggregate <- left_join(aggregate, genre, by = "Hours_per_day")
aggregate <- left_join(aggregate, when_listening, by = "Hours_per_day")
colnames(aggregate) <- c("Hours per Day", "Number of Reponses", "Majority Age", "Percentage of Improved Health", "Percentage of Worsened Health", "Percentage of No Health Effect", "Majority Genre", "Percentage of Listening While Working/Studying")
aggregate <- aggregate %>% mutate_if(is.numeric, round, digits = 2)
