library(tidyverse)
library(ggstream)
library(RColorBrewer)
library(scales)

youtube <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-02/youtube.csv')

df <- youtube %>%
  select(1,5:11) %>%
  rename(sex = use_sex, "quick product placement" = show_product_quickly) %>%
  gather(key = "description", value = "presence", -year) %>%
  filter(presence == TRUE) %>%
  group_by(year, description) %>%
  summarise(count = n()) %>%
  mutate(prop = count / sum(count))

#subset for vlines
lines <- df %>%
  filter(year %in% c(2000,2005,2010,2015,2020))


p <- ggplot(df, aes(year, prop, fill = description))+
  geom_stream(color = "#c6c9cf", extra_span = .15, true_range = "none")+
  geom_vline(data = lines, aes(xintercept = year), linetype = "dotted", color = "#c6c9cf", size = .7)+
  geom_stream_label(aes(label = tolower(description)), size = 4.5, type = "mirror", extra_span = .15)+
  scale_fill_brewer(palette = "YlOrRd")+
  scale_x_continuous(position = "top")+
  theme(
    panel.background = element_rect(fill = "#122c57"),
    plot.background = element_rect(fill = "#122c57"),
    plot.title = element_text(color = "white", face = "bold", size = 22, hjust = .5),
    plot.subtitle = element_text(color = "white", size = 11),
    plot.caption = element_text(color = "white", face = "bold", size = 9),
    axis.text.x = element_text(color = "white", face = "bold", size = 13),
    axis.text.y = element_blank(),
    panel.grid  = element_blank(),
    legend.position = "none"
  )+
  labs(
    y = "",
    x = "",
    title = "Superbowl Ad Characteristics",
    subtitle = "Proportion of characteristics featured in the commercials from the last 21 Superbowls \n(2000-2020). The 10 brands that aired the mosts ad spots were included in the dataset.\nShowing the product quickly and including humor are fairly consistently utilized.",
    caption = "Plot: @tessuheagle | Data: FiveThirtyEight"
  )

ggsave("3_2_21.png", p, width = 7, height = 5)
