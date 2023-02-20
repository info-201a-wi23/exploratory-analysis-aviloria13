

library(ggplot2)
library(dplyr)  


df <- read.csv("~/Desktop/INFO201/exploratory-analysis-aviloria13/mxmh_survey_results.csv", stringsAsFactors = FALSE)

#create a new datafram
new_df <- df %>% select(While.working, Music.effects)
num_row_df <- nrow(new_df)

#filter listen to music while working
while_w <- new_df %>% filter(While.working == "Yes")

#filter have positive effects on listening to music while working
working_improve <- while_w %>% filter(Music.effects == "Improve")

#find how many rows with YES and Improve
yes_improved <- nrow(working_improve)

#find the percentage with YES and Improve
percent_working_improve <- (yes_improved / num_row_df) * 100

#filter have negative effects on listening to music while working
working_no_improve <- while_w %>% filter(Music.effects == "No effect")

#find rows with YES and no effect
yes_noeffects <- nrow(working_no_improve)

#find percent
percent_working_no_improve <- (yes_noeffects/ num_row_df) * 100

#filter no music while working
no_while_w <- new_df %>% filter(While.working == "No")

#filter no music while working has positive effect
no_working_improve <- no_while_w %>% filter(Music.effects == "Improve")

#find rows with NO and 'improved'
no_improved <- nrow(no_working_improve)

#find percent
percent_no_working_improve <-(no_improved / num_row_df) * 100

#filter no music has negative effect
no_working_no_improve <- no_while_w %>% filter(Music.effects == "No effect")

#find rows with NO and 'no effects'
no_noeffects <- nrow(no_working_no_improve)

#percent
percent_no_working_no_improve <- (no_noeffects/ num_row_df) * 100

# find else
rest_else <- nrow(new_df) - yes_improved - yes_noeffects - no_improved - no_noeffects 
percent_rest_else <- 100 - percent_working_improve - percent_working_no_improve - percent_no_working_improve - percent_no_working_no_improve 


Group = c("While work and effective", "While work and not effective", "Not while working and effective", "Not while working and not effective", "else")
value = c(yes_improved, yes_noeffects, no_improved, no_noeffects, rest_else)
percentage = c(percent_working_improve, percent_working_no_improve, percent_no_working_improve, percent_no_working_no_improve, percent_rest_else)

chart_df <- data.frame(Group, value, percentage)

round_percent <- round(percentage, 2)
                   
ggplot(chart_df, aes(x = "", y = percentage, fill = Group)) +
  geom_col(color = "white") +
  geom_text(aes(label = round_percent),
            position = position_stack(vjust = 0.5), cex = 2) +
  coord_polar(theta = "y") +
  theme(text = element_text(size = 10)) +
  labs(title = "When listening & Music effects") +
  scale_fill_brewer(palette = "Pastel1")

 






