# TidyTuesday Week 12, Tessa Eagle, 3-16-21

library(tidyverse)
library(ggpubr)
library(extraFont)

tuesdata <- tidytuesdayR::tt_load(2021, week = 12)
games <- tuesdata$games



df <- games %>%
  group_by(year, month) %>%
  slice_max(peak) 
  #summarise(maxPlayers = max(peak)) 

df$gamename <-  str_to_title(df$gamename) 
df$month <- strtrim(df$month, 3)
df$month <- factor(df$month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

colors <- RColorBrewer::brewer.pal(11, "Spectral")[c(11,10,9,6,2)]
df$gamename <- as.factor(df$gamename)
summary(df$gamename)


# Tile plot ---------------------------------------------------------------
p <- df %>%
  filter(year < 2021) %>%
  ggplot(aes(x= month, y = year, fill = gamename))+
  geom_tile(color = "black", size = 1.5)+
  scale_fill_manual(values = colors)+
 # scale_fill_brewer(palette = "RdYlBu")+
  scale_x_discrete(position = "top")+
    theme(
      plot.background = element_rect(fill = "black", color = NA),
      panel.background = element_rect(fill = "black", color = NA),
      plot.title = element_text(color = "white", size = 35, hjust = .5, face = "bold", family = "Bauhaus 93"),
      plot.subtitle = element_text(color = "white", size = 14, hjust = .5, family = "Arial"),
      panel.grid = element_blank(),
      plot.margin = unit(c(.5,.5,6,0), "cm"),
      legend.background = element_rect(fill = "black"),
      legend.text = element_text(color = "white", size = 12),
      legend.title = element_text(color = "white", size = 12),
      axis.text = element_text(color = "white", size = 13.5, face = "bold"),
      legend.key = element_blank(),
      legend.position = "top"
    )+
  labs(
    fill = "Game",
    title = "Simultaneous Gaming",
    subtitle = "A breakdown of the video games that had the highest peak player numbers for each month and year, from July 2012 to \nDecember 2020. Peak player numbers detail the highest number of people playing a game at the same time. The highest \nsimultaneous player count was in January 2018, when PlayerUnknown's Battlegrounds was played by 3,236,027 people \nconcurrently. Dota 2 had the highest peak count for 68 different months."
  )



# Bar and ball bottom plot ------------------------------------------------
p2 <- ggplot() + 
  geom_point(aes(x = 2.25, y = 8.5), color = "#c84848", size = 8)+
  geom_rect(aes(xmin=1, xmax=2.5, ymin=0, ymax=1), color="#c84848", fill = "#c84848")+
  geom_rect()+
  ylim(0,10)+
  xlim(0,5)+
  theme(
    plot.background = element_rect(fill = "black", color = NA),
    panel.background = element_rect(fill = "black"),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    plot.caption = element_text(color = "white", size = 13)
  )+
  labs(
    caption = "Plot: @tessueagle | Data: Steam"
  )

ggarrange(p, p2, ncol = 1,  heights = c(5, 1.5))



# Controller test ---------------------------------------------------------
ggplot() + 
  ylim(0,30)+
  geom_rect(aes(xmin=0, xmax=20, ymin=0, ymax=20), color="#d1cfcf", fill = "#d1cfcf")+#background
  geom_rect(aes(xmin=1, xmax=19, ymin=1, ymax=16), color="#000000", fill = "#000000")+#black rect
  geom_rect(aes(xmin=3, xmax=4, ymin=4, ymax=12), color="#d1cfcf", fill = "#000000")+#plus pt.1
  geom_rect(aes(xmin=1.5, xmax=5.5, ymin=7, ymax=8.5), color="#adadad", fill = "#000000")+#plus pt.2
  geom_rect(aes(xmin=8, xmax=12, ymin=14.5, ymax=16), color="#adadad", fill = "#adadad")+#middle first rect
  geom_rect(aes(xmin=8, xmax=12, ymin=12, ymax=13.5), color="#adadad", fill = "#adadad")+#middle second rect
  geom_rect(aes(xmin=8, xmax=12, ymin=9.5, ymax=11), color="#adadad", fill = "#adadad")+#middle third rect
  geom_rect(aes(xmin=8, xmax=12, ymin=4, ymax=8), color="#d1cfcf", fill = "#d1cfcf")+#middle bottom rect
  geom_rect(aes(xmin=8, xmax=12, ymin=1, ymax=2.5), color="#adadad", fill = "#adadad")+#middle main rect
  geom_rect(aes(xmin=14, xmax=15.5, ymin=2, ymax=5), color="#d1cfcf", fill = "#d1cfcf")+#grey squares
  geom_rect(aes(xmin=16, xmax=17.5, ymin=2, ymax=5), color="#d1cfcf", fill = "#d1cfcf")+#grey square 2
  geom_point(aes(x = 14.7, y = 3.5), size = 10, color = "red")+
  geom_point(aes(x = 16.7, y = 3.5), size = 10, color = "red")+
  theme(
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black"),
    panel.grid = element_blank(),
    axis.text = element_blank()
  )
