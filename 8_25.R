library(tidyverse)
library(wordcloud2)
library(devtools)

devtools::install_github("lchiffon/wordcloud2")

tuesdata <- tidytuesdayR::tt_load(2020, week = 35)
chopped <- tuesdata$chopped


# Word Cloud --------------------------------------------------------------
df <- chopped %>%
  select(appetizer, entree, dessert) 

df <- gather(df, "meal", "ingredients", 1:3)

freq <- data.frame(table(unlist(strsplit(tolower(df$ingredients), ","))))

names(freq)[1] <- "word"
names(freq)[2] <- "frequency"

freq <- freq %>% arrange(desc(frequency))

wordcloud2(freq, figPath = "cleaver4.jpg", size = .75, backgroundColor = "#163459", shuffle = FALSE, color = rep(c("#FF8C00","#FFA500","#FF4500","#FFAF42", "#FEDEBE", "#e68825", "#ff8400"),nrow(freq)))


# Lollipop Plot -----------------------------------------------------------
scores <- 
  chopped %>% 
  filter(episode_rating != "NA") %>%
  select(season, episode_rating) %>% 
  group_by(season) %>% 
  mutate(season_avg = mean(episode_rating))
  

ggplot(scores, aes(season, season_avg)) +
  geom_segment(aes(x = season, xend = season, y = 0, yend = 10), color = "white", lty = "dotted") +
  geom_segment(aes(x = season, xend = season, y = 0, yend = season_avg), color = "#a4d1ed") +
  geom_point(shape = 19, size = 2.5, fill = "#e68825", color = "#e68825")+
  scale_y_continuous(breaks = seq(0, 10, by = 2))+
  scale_x_continuous(breaks = seq(0, 43, by = 5))+
  coord_flip()+
  theme(
    plot.background = element_rect(fill = "#163459"),
    panel.background = element_rect(fill = "#163459"),
    axis.text = element_text(color = "white"),
    axis.text.x = element_text(margin = margin(t=-10)),
    axis.text.y = element_text(margin = margin(r=-15)),
    axis.title = element_text(color = "white"),
    plot.title = element_text(color = "white", hjust = .5, vjust = -.2, size = 20, face ="bold"),
    plot.subtitle = element_text(color = "white", hjust = .5, vjust = -.2, size = 15),
    panel.grid = element_blank()
  )+
  labs(
    y = "Average Episode Rating",
    x = "",
    title = "Chopped: Seasons 1-43",
    subtitle = "Average Episode Rating by Season"
  )




