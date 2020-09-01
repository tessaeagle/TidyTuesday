library(tidyverse)
library(ggpubr)
library(viridis)

tuesdata <- tidytuesdayR::tt_load(2020, week = 36)
key_crop_yields <- tuesdata$key_crop_yields

df <- key_crop_yields %>%
  filter(Entity == "United States") %>%
  gather(Crop, Tonnes, 4:14)

df$Crop <- gsub("\\(.*","",df$Crop)

p1 <- df %>%
  filter(Tonnes != "NA") %>%
  ggplot(aes(x= Crop, y = Year, fill = Tonnes))+
  geom_tile(color = "grey")+
  coord_flip()+
  scale_y_continuous(breaks = seq(1960, 2020, by = 10))+
  scale_fill_viridis(discrete=FALSE, breaks = c(5,15,25,35,45)) +
  theme(
    panel.background = element_rect(fill = "#4f5d75"),
    plot.background = element_rect(fill = "#4f5d75", color = NA),
    panel.grid = element_blank(),
    axis.text = element_text(color = "white", size = 12, face = "bold"),
    axis.text.y = element_text(margin = margin(r=-15)),
    axis.title = element_text(color = "white", size = 15),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white"),
    legend.background = element_blank(),
    legend.margin=margin(t = -.7, r = 1.5, unit='cm')
  )+
  labs(
    x = "",
    y = ""
  )


p2 <- df %>%
  filter(Tonnes != "NA") %>%
  ggplot(aes(x= Year, y = Tonnes, color = Crop))+
  geom_line(size = 1)+
  scale_x_continuous(breaks = seq(1960, 2020, by = 10))+
  scale_color_viridis(discrete=TRUE, option = "plasma") +
  theme(
    panel.background = element_rect(fill = "#4f5d75"),
    plot.background = element_rect(fill = "#4f5d75", color = NA),
    panel.grid = element_blank(),
    axis.text = element_text(color = "white", size = 10, face = "bold"),
    axis.title.y = element_text(margin = margin(r=10)),
    axis.title = element_text(color = "white", size = 14),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(color = "white", face = "bold"),
    legend.title = element_text(color = "white", face = "bold"),
    legend.key = element_rect(fill = "#4f5d75"),
    legend.background = element_rect(fill = "#4f5d75"),
    legend.margin=margin(t = -.5, unit='cm'),
    plot.title = element_text(color = "white", size = 20, hjust = .5, face = "bold"),
    plot.subtitle = element_text(color = "white", size = 14, hjust = .5),
    plot.margin = unit(c(.5,.5,.5,.5), "lines")
  )+
  guides(color = guide_legend(nrow = 1))+
  labs(
    x = "",
    y = "Tonnes",
    title = "United Spuds of America",
    subtitle = "US crop yields over time (1961-2018)"
  )

p <- ggarrange(p2, p1, ncol = 1, nrow = 2, heights = c(1.5, 1.6))
