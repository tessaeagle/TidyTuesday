install.packages("githubinstall")
install.packages("devtools")
install.packages("ggimage")
install.packages("png")
install.packages("raster")
install.packages("mgsub")
library(devtools)
library(tidyverse)
library(RColorBrewer)
library(ggimage)
library(png)
library(raster)
library(ggplot2)
library(grid)
library(mgsub)


character_visualization <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-30/character_visualization.csv')


selected <- dplyr::select(character_visualization, character, depicted, issue) %>%
  filter(depicted > 0)

agg <- aggregate(selected$depicted, by=list(character=selected$character), FUN=sum)


agg$character = gsub("\\=.*", "", agg$character)#remove aliases
agg$character = gsub("\\(.*", "", agg$character)

agg_new <- agg %>%
  arrange(x) %>%
  mutate(mean_x = mean(x),
         bool = ifelse(x - mean_x > 0, TRUE, FALSE),
         character = factor(character, levels = .$character))

ggplot(agg_new, aes(x = x, y = character, color = bool)) +
  geom_segment(aes(x = mean_x, y = character, xend = x, yend = character), color="skyblue") +
  geom_point(size=4,alpha=.6)+
  annotate("text", x = 650, y = 20, label = "Above Average", color = "darkslategray2", size = 4) +
  annotate("text", x = 1700, y = 9, label = "Below Average", color = "tomato", size = 4) +
  geom_segment(aes(x = 1000, xend = 1000, y = 17, yend = 23),
               arrow = arrow(length = unit(0.2,"cm")), color = "darkslategray2") +
  geom_segment(aes(x = 1350, xend = 1350, y = 16, yend = 1),
               arrow = arrow(length = unit(0.2,"cm")), color = "tomato") +
  scale_x_continuous(breaks = round(seq(0, max(agg$x), by = 500)))+#tick mark adjustment
  theme(axis.line = element_line(color = "yellow",size=1),
        axis.ticks.x = element_line(color="white",size=1),
        axis.ticks.y = element_blank(),
        plot.background = element_rect(fill="black"),
        panel.background = element_rect(fill="black"),
        panel.grid = element_line(colour = "black"),
        axis.text = element_text(colour = "white", size=12),
        axis.title.x = element_text(colour = "darkslategray2", size=14),
        plot.title = element_text(colour="yellow", size=14, hjust=0.5),
        legend.position = "none")+
  ylab(NULL)+
  xlab("Total Depictions")+
  ggtitle("Claremont Run Character Occurrences: Uncanny X-Men #97-278")

#followed https://www.statology.org/lollipop-chart-r/ for arrow tutorial



#old
agg$character <- as.character(agg$character)#reorder characters by count 
agg$character <- factor(agg$character, levels=unique(agg$character))

#older version, not split by averages
ggplot(agg, aes(x = x, y = character)) +
  geom_segment(aes(x = 0, y = character, xend = x, yend = character),color="skyblue") +
  geom_point(color="red",size=4,alpha=.5)+
  scale_x_continuous(breaks = round(seq(0, max(agg$x), by = 500),1))+
  xlab("Total Depictions")+
  ylab(NULL)+
  ggtitle("Claremont Run Character Occurrences: Uncanny X-Men #97-278")+
  theme(axis.line.x = element_line(color = "yellow",size=1),
        axis.line.y = element_line(color = "yellow", size=1),
        axis.ticks.x = element_line(color="white",size=1),
        axis.ticks.y = element_blank(),
        plot.background = element_rect(fill="black"),
        panel.background = element_rect(fill="black"),
        panel.grid.major = element_line(colour = "black"),
        panel.grid.minor = element_line(colour = "black"),
        axis.text = element_text(colour = "white", size=12),
        axis.title.x = element_text(colour = "skyblue", size=14),
        axis.title.y = element_text(colour = "white", size=14),
        plot.title = element_text(colour="yellow", size=14))





