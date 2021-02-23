library(tidyverse)

beyonce_lyrics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/beyonce_lyrics.csv')
taylor_swift_lyrics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/taylor_swift_lyrics.csv')

taylor_swift_lyrics$Lyrics <- tolower(taylor_swift_lyrics$Lyrics)

df <- taylor_swift_lyrics %>%
  group_by(Album) %>% 
  summarize(love_count = sum(str_count(Lyrics, "love")))


df$Album <- as.factor(df$Album)
df$Album <- factor(df$Album, ordered = TRUE, levels = c("Taylor Swift", "Fearless", "Speak Now", "Red", "1989", "reputation", "Lover", "folklore"))


ggplot(df, aes(x = Album, y = love_count)) +
  geom_segment(aes(x = Album, xend = Album, y = 0, yend = love_count), size = 1.5, color = "#faf2be", linetype="dotted")+
  geom_point(size = 7.5, color = "#bb91e3", shape="\U2665")+ 
  scale_y_continuous(breaks = seq(0, 90, by = 10))+
  coord_flip()+
  theme(
    plot.background = element_rect(color = NA, fill = "#FFB4B4"),
    panel.background = element_rect(color = NA, fill = "#FFB4B4"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 20, hjust = .4, color = "#ad3b70", face = "bold"),
    plot.subtitle = element_text(size = 13.5, hjust = .4, color = "#ad3b70"),
    axis.text.y = element_text(size = 12, face = "bold", color = "#519bc9", margin = margin(r=-15)),
    axis.title = element_text(size = 14, color = "#ad3b70"),
    axis.text.x = element_text(size = 12, face = "bold", color = "#ad3b70")
  )+
  labs(
    x = "Album",
    y = "",
    title = "Taylor Swift's Use of Love",
    subtitle = "Lyrical occurences of 'love' across albums"
  )



