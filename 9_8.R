library(tidyverse)
library(waffle)
library(png)
library(grid)
library(cowplot)

tuesdata <- tidytuesdayR::tt_load(2020, week = 37)
friends <- tuesdata$friends

img <- png::readPNG("frame_long2.png")


df2 <- friends %>%
  group_by(speaker, season) %>% 
  summarise(freq = n()) %>%
  filter(speaker != "Scene Directions"  & freq > 630)


# Frame Version -----------------------------------------------------------
p <- ggplot(df2, aes(fill = speaker, values = freq))+
  geom_waffle(n_rows = 10, size = .5, flip = TRUE, make_proportional = TRUE, color = "#9c8cd4") +
  facet_wrap(~season, nrow = 2, strip.position = "bottom") +
  guides(fill = guide_legend(nrow = 2))+
  scale_fill_manual(name = "Speaker", values = c("#FF9AA2", "#FFDAC1", "#FBF7D5", "#B5EAD7", "#C5EBFE", "#DCCAEC"))+
  coord_equal()+
  theme_minimal()+
  theme_enhance_waffle()+
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    plot.background = element_rect(fill = "#9c8cd4", color = NA),
    legend.text = element_text(size = 10),
    strip.text.x = element_text(size = 13, colour = "black"),
    legend.margin=margin(t = -.5, unit='cm'),
    panel.spacing = unit(-.1, "lines"),
    plot.margin = unit(c(6,6,6,6), "lines"),
    plot.title = element_text(size = 13, face = "bold", vjust = -.1)
    )+
  labs(
    title = "Which Friend Talked the Most?"
  )


  
plot <- ggdraw(p) + annotation_custom(rasterGrob(img, width = unit(1,"npc"), height = unit(1,"npc")),  -Inf, Inf, -Inf, Inf)

plot <- cowplot::ggdraw(plot) + 
  theme(plot.background = element_rect(fill="#9c8cd4", color = NA))


# Frameless Version -------------------------------------------------------
p2 <- ggplot(df2, aes(fill = speaker, values = freq))+
  geom_waffle(n_rows = 10, size = .5, flip = TRUE, make_proportional = TRUE, color = "#9c8cd4") +
  facet_wrap(~season, nrow = 2, strip.position = "bottom") +
  guides(fill = guide_legend(nrow = 1))+
  scale_fill_manual(name = "Speaker", values = c("#FF9AA2", "#FFDAC1", "#FBF7D5", "#B5EAD7", "#C5EBFE", "#DCCAEC"))+
  coord_equal()+
  theme_minimal()+
  theme_enhance_waffle()+
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    plot.background = element_rect(fill = "#9c8cd4", color = NA),
    legend.text = element_text(size = 11),
    strip.text.x = element_text(size = 12, color = "black"),
    legend.margin=margin(t = .25, unit='cm'),
    panel.spacing = unit(1, "lines"),
    plot.title = element_text(size = 25, face = "bold", hjust = .5, color = "#e8d697", vjust = 3),
    plot.subtitle = element_text(size = 16, hjust = .5, color = "#e8d697", vjust = 4)
  )+
  labs(
    title = "Which Friend Talked the Most?",
    subtitle = "Number of Lines per Season of Friends, Broken Down by Speaker"
  )

plot <- cowplot::ggdraw(p2) + 
  theme(plot.background = element_rect(fill="#9c8cd4", color = NA))
