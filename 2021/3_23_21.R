#Tessa Eagle
#Week 13 TT

library(tidyverse)
library(treemapify)
library(gganimate)
library(RColorBrewer)
library(extrafont)

unvotes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/unvotes.csv')
roll_calls <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/roll_calls.csv')
issues <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/issues.csv')

df <- left_join(roll_calls, issues, by = "rcid")
df <- separate(df, date, into = c("year", "month", "day"), sep = "-")

df2 <- df %>%
  select(rcid, issue, year) %>%
  na.omit(issue) %>%
  group_by(year, issue) %>%
  summarise(count = n())

df2$year <- as.integer(as.character(df2$year))
df2$issue <- as.factor(df2$issue)
df2$issue <- plyr::revalue(df2$issue, c("Arms control and disarmament" ="Arms Control", "Nuclear weapons and nuclear material" = "Nuclear Weapons/Material", "Economic development" = "Economic Dev"))

levels(df2$issue)


p <- ggplot(df2, aes(area = count, fill = issue, label = issue)) +
  geom_treemap()+
  geom_treemap_text(colour = "black", place = "centre",grow = TRUE)+
  scale_fill_brewer(palette = "RdYlBu")+
  theme(
    legend.position = "none",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.key = element_blank(),
    plot.subtitle = element_text(size = 20, hjust = .05, margin = margin(b = .5, unit = "cm")),
    plot.caption = element_text(size = 16, hjust = 1),
    plot.title = element_text(size = 36, hjust = .8, family = "Arial", face = "bold"),
    axis.text = element_blank(),
    plot.margin = margin(l = 2, r = 2, t = .5, b = .5, unit = "cm")
  )+
  labs(
    fill = "Issue",
    subtitle = 'Year: {frame_time}',
    title = "United Nations Issues Over Time",
    caption = "Plot: @tessuheagle | Data: Harvard Dataverse",
    y = "",
    x =""
  )+
  transition_time(year)


animate(p, height = 500, width = 700, fps = 2)

anim_save("3_23_21.gif", animation = last_animation())

