library(tidyverse)
library(tidytuesdayR)
library(ngram)
library(extrafont)
library(tvthemes)


tuesdata <- tidytuesdayR::tt_load(2020, week = 33)
avatar <- tuesdata$avatar

import_avatar()
loadfonts(device = "win")
windowsFonts(Slayer = windowsFont("Slayer"))
font_import()

avatar$total <- sapply(avatar$character_words, function(x) length(unlist(strsplit(as.character(x), "\\W+"))))
agg <- aggregate(avatar$total, by=list(book=avatar$book_num, chapter=avatar$chapter_num, character=avatar$character), FUN=sum)
agg <- agg[order(agg$book, agg$chapter),] 
  
agg <- agg %>% 
  filter(character %in%  c("Aang", "Katara", "Iroh", "Zuko", "Sokka", "Toph")) %>%
  mutate(chapter = chapter / 100) %>%
  mutate(episode = book + chapter) %>%
  mutate(line_num = row_number())


ggplot(agg, aes(x=character, y=line_num, size=x, color=character)) +
  geom_point(alpha=0.6)+
  coord_flip() +
  scale_size(range = c(2,8), name="Word Count")+
  scale_color_avatar(palette = "FireNation", guide = FALSE) +
  theme_avatar(
    title.font = "Slayer",
    text.font = "Slayer",
  )+
  theme(
    plot.title = element_text(size = 20, hjust = .5, color = "#52a8c4", face = "bold", family = "Slayer"),
    plot.subtitle = element_text(size = 16, hjust = .5, color = "#52a8c4", family = "Slayer"),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 12, face = "bold", color = "#52a8c4"),
    legend.position = "bottom",
    legend.justification="center",
    legend.margin=margin(t = -.5, unit='cm'),
    legend.background = element_rect(color = NA),
    legend.text=element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.key = element_rect(color = NA, fill = NA)
  )+
  labs(
    title = "Avatar: Words Spoken by Main Characters",
    subtitle = "Total Wordcount by Episode",
    x = "",
    y = ""
  )+
  annotate(geom="text", x=.5, y=5, label="Book 1",
           color="maroon")+
  annotate(geom="text", x=.5, y=96, label="Book 2",
           color="maroon")+
  annotate(geom="text", x=.5, y=200, label="Book 3",
           color="maroon")


