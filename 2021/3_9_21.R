#Tessa Eagle
#TidyTuesday Week 11, 2021
#Bechdel Test

library(tidyverse)
library(tidytuesdayR)
library(ggimage)

tuesdata <- tidytuesdayR::tt_load('2021-03-09')
bechdel <- tuesdata$raw_bechdel
movies <- tuesdata$movies

bg <- "https://upload.wikimedia.org/wikipedia/commons/thumb/4/4d/Film_strip.svg/1200px-Film_strip.svg.png"


df <- left_join(bechdel, movies, by = c("imdb_id" = "imdb_id", "title" = "title", "year" = "year"))


df3 <- df %>%
  select(year, rating) %>%
  filter(rating == 3) %>%
  group_by(year, rating) %>%
  summarise(n = n()) %>%
  mutate(decade = case_when(
          year == 1899 ~ "1890s",
          year %in% c(1910:1919)  ~ "1910s",
          year %in% c(1920:1929)  ~ "1920s",
          year %in% c(1930:1939)  ~ "1930s",
          year %in% c(1940:1949)  ~ "1940s",
          year %in% c(1950:1959)  ~ "1950s",
          year %in% c(1960:1969)  ~ "1960s",
          year %in% c(1970:1979)  ~ "1970s",
          year %in% c(1980:1989)  ~ "1980s",
          year %in% c(1990:1999)  ~ "1990s",
          year %in% c(2000:2009)  ~ "2000s",
          year %in% c(2010:2019)  ~ "2010s",
          year %in% c(2020:2021)  ~ "2020s"
          ))

df3$year <- as.factor(df3$year)
df3$decade <- as.factor(df3$decade)

labs <- df3 %>%
  filter(n >= 90)

p <- ggplot(df3, aes(x = year, y = 1))+
  geom_segment(aes(x = year, y = 0, xend = year, yend = 1), color = "grey")+
  geom_point(aes(size = n, color = decade))+
  scale_color_manual(values = c("#a50026",
                                 "#d73027",
                                 "#f46d43",
                                 "#fdae61",
                                 "#fee090",
                                 "#ffffbf",
                                 "#d8d8d8",
                                 "#e0f3f8",
                                 "#abd9e9",
                                 "#74add1",
                                 "#4575b4",
                                 "#313695",
                                 "#ffffff"))+
  scale_size(range = c(1,10))+
  scale_x_discrete(breaks=c(1910,1920, 1930, 1940, 1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020))+
  geom_text(data = labs, aes(label = n), color = "#ffffff", size = 3.5)+
  theme(
    plot.background = element_rect(fill = "black", color = NA),
    panel.background = element_rect(fill = "black", color = NA),
    legend.position = "bottom",
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = .5, size = 45, face = "bold", color = "white"),
    plot.subtitle = element_text(hjust = .5, size = 18, color = "white"),
    plot.caption = element_text(size = 18, face = "bold", color = "white", hjust = 1.2),
    legend.key = element_blank(),
    legend.background = element_rect(fill = "black"),
    legend.text = element_text(color = "white", size = 20),
    legend.title = element_text(color = "white", size = 20),
    axis.text.x = element_text(color = "white", size = 18)
  )+
  guides(color = FALSE, size = guide_legend(override.aes=list(color = "white")))+
  labs(
    x = "",
    y = "",
    title = "The Bechdel Test in Films",
    subtitle = "This plot displays the count of the number of films produced each year (1899-2021) that pass the Bechdel Test with a \nrating of 3. The Bechdel Test measures female representation based on three measures: 1. There are at least two \nnamed women 2. They have a conversation with each other, and 3, The conversation isn't about a male character.\n Each decade is shown in a different color and the count shown in points that were large enough.",
    caption = "Plot: @tessuheagle | Data: FiveThirtyEight",
    size = "Count"
  )+
  coord_polar()



plot <- cowplot::ggdraw(p) + 
  theme(plot.background = element_rect(fill="black", color = NA))

ggsave("3_9_21.png", plot, width = 14, height = 14)


