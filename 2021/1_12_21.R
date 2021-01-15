library(tidyverse)

artwork <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-12/artwork.csv')
artists <- readr::read_csv("https://github.com/tategallery/collection/raw/master/artist_data.csv")


df <- left_join(artists, artwork, by = c("id" = "artistId"))

yearCreated <- df %>%
  count(year) %>%
  na.omit()


yearCreated %>%
  ggplot(aes(x=year, y=n, size=n, color = n)) +
  geom_point(alpha=0.5) +
  geom_hline(yintercept = -75, color = "white")+
  scale_x_continuous(breaks = seq(from = 1550, to = 2020, by = 50))+
  scale_size(range = c(.05,10), breaks = c(0,50,250,500,1000,1250, 1500,2500,3000,3500)) +
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 12),
    axis.title.x = element_text(size = 13, color = "#363636", margin = margin(t = 10, r = 0, b = 0, l = 0)),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "#9cd0db", color = NULL),
    panel.background = element_rect(fill = "#9cd0db", color = NULL),
    legend.background = element_rect(fill = "#9cd0db", color = NULL),
    legend.key = element_rect(fill = "#9cd0db", color = NULL),
    legend.position = "bottom",
    plot.title = element_text(size = 20, hjust = -0),
    plot.subtitle = element_text(size = 11, hjust = 0, vjust = 3)
  )+
  coord_cartesian(clip = "off")+
  guides(size = guide_legend(nrow = 1, override.aes = list(color = c("#3a1c71", "#5b3573","#7c4d74", "#9d6576","#d76d77", "#bd7e78","#de9779", "#ffaf7b"))))+
  scale_color_gradient(guide = FALSE, low = "#3a1c71", high = "#ffaf7b")+
  labs(
    y = "",
    x = "Year",
    size = "Count",
    title = "Tate Modern Artworks",
    subtitle = "Counts of the number of pieces of artwork acquired by the Tate Modern art museum by year \nof artwork creation, with the earliest created in 1545 and the most recent in 2012 (datset \nfrom 2014). The ten years with the most pieces acquired were between 1800 and 1830."
  )


