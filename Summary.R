library("dplyr")
library("stringr")
library("ggplot2")
library("scales")

project_df <- read.csv("/Users/aaliyahviloria/Desktop/INFO201/exploratory-analysis-aviloria13/mxmh_survey_results.csv", stringsAsFactors = FALSE)

total_responses <- nrow(project_df)
improved <- project_df %>% group_by(Hours.per.day) %>% count(Music.effects = "Improve")
improved <- improved %>% mutate(percentage = (n/total_responses) * 100)
worsen <- project_df %>% group_by(Hours.per.day) %>% count(Music.effects = "Worsen")
worsen <- improved %>% mutate(percentage = (n/total_responses) * 100)
no_effect <- project_df %>% group_by(Hours.per.day) %>% count(Music.effects = "No effect")
no_effect <- improved %>% mutate(percentage = (n/total_responses) * 100)

print(sum(improved$percentage))
print(sum(worsen$percentage))
print(sum(no_effect$percentage))


