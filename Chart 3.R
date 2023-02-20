# Load the data
original_df <- read.csv("mxmh_survey_results.csv", stringsAsFactors = FALSE)

# Load the packages
library("dplyr")
library("tidyverse")
library("ggplot2")
library("reshape2")

# Create a data frame with 16 music genres, each corresponding the numbers of responses based on frequency.
music_effect_frequency <- original_df %>% 
  filter(Music.effects == "Improve") %>% 
  select(Frequency..Classical., Frequency..Country., Frequency..EDM., Frequency..Folk., Frequency..Gospel.,
         Frequency..Hip.hop., Frequency..Jazz., Frequency..K.pop., Frequency..Latin., Frequency..Lofi., 
         Frequency..Metal., Frequency..Pop., Frequency..R.B., Frequency..Rap., Frequency..Rock.,
         Frequency..Video.game.music., Music.effects) %>% 
  select(-Music.effects)
mef_df <- data.frame(sapply(X = music_effect_frequency, FUN = table))
colnames(mef_df) <- c("Classical", "Country", "EDM", "Folk", "Gospel", "HipHop", "Jazz", "Kpop", "Latin", "Lofi", "Metal",
                      "Pop", "Rap", "R&B", "Rock", "Video Game")
mef_df <- as.data.frame(t(mef_df))
mef_df <- mef_df %>% 
  mutate(Music_Genre = c("Classical", "Country", "EDM", "Folk", "Gospel", "HipHop", "Jazz", "Kpop", "Latin", "Lofi", "Metal",
           "Pop", "Rap", "R&B", "Rock", "Video Game"))
new_mef_df <- melt(mef_df, id.vars = "Music_Genre")

# Draw a stacked bar chart
ggplot(new_mef_df, aes(x = Music_Genre, y = value, fill = variable, label = value)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Pastel2") +
  scale_y_continuous(breaks = c(50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 550)) +
  labs(title = "Music Genre vs Music Effects",
       x = "Music Genre",
       y = "Number of People Who Report 'Improve'",
       fill = "Frequency") 

