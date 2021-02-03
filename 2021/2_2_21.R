library(tidyverse)
library(tidytuesdayR)
library(ggalt) 
library(ggtext)

tuesdata <- tidytuesdayR::tt_load('2021-02-02')
hbcu_all <- tuesdata$hbcu_all

df <- hbcu_all %>%
  select(1:4)

df$percFem = (100*(df$Females / df$`Total enrollment`))


p <- ggplot(df, aes(y = Year, x = Males, xend = Females))+
  geom_segment(data = df, aes(y = Year, yend = Year, x = min(Males), xend = max(Females) * 1.1), color = "#949494", alpha = .75)+
  geom_dumbbell(size= 1, size_x = 3, size_xend = 3, colour_x = "#006E90", colour_xend = "#F18F01", colour = "white")+
  geom_text(data = filter(df, Year == 2015), aes(x = Males, y = Year, label="Males"), color="#006E90", size=3.2, vjust=-1, fontface="bold")+
  geom_text(data = filter(df, Year == 2015), aes(x = Females, y = Year, label="Females"), color="#F18F01", size=3.2, vjust=-1, fontface="bold")+
  labs(
    y = "",
    x = "Total Enrollment",
    title = "Historical Enrollment at HBCUs",
    subtitle = "Enrollment over time (1976-2015) at Historically Black Colleges and Universities. <br>Total enrollment of 
<span style = 'color:#F18F01'><b>Females</b></span> has been higher than <span style = 'color:#006E90'><b>Males</b></span> 
every year since 1976."
  )+
  scale_x_continuous(breaks = seq(from = 100000, to = 200000, by = 20000))+
  scale_y_continuous(labels = seq(from = 1975, to = 2015, by = 5), breaks = seq(from = 1975, to = 2015, by = 5))+
  theme(
    panel.background = element_rect(fill = "#ADCAD6", color = NA),
    plot.background = element_rect(fill = "#ADCAD6", color = NA),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_text(size = 12, color = "black"),
    axis.title.x = element_text(size = 13, margin = margin(t = 10, r = 0, b = 0, l = 0)),
    plot.title = element_text(size = 20, hjust = .5, face = "bold"),
    plot.subtitle = element_markdown()
  )+
  geom_rect(data = df, aes(xmin = max(Females) * 1.09, xmax = max(Females) * 1.13, ymin = -Inf, ymax = Inf), fill = "#c5d6e3")+
  geom_text(data = df, aes(label = paste0(round(percFem), "%"), y = Year, x = max(Females) * 1.11), fontface = "bold", size = 2.5, color = "#F18F01")+
  geom_text(data = filter(df, Year == 2015), aes(x = max(Females) * 1.244, y = Year, label = "% F"), color = "black", size = 2.5, vjust = -1.3, fontface = "bold") 


ggsave('2_2_21.png', p, height = 7, width = 7, limitsize = F)



