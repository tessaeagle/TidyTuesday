library(tidyverse)
library(tidytuesdayR)

mobile <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-10/mobile.csv')
landline <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-10/landline.csv')

options(scipen = 999)

df <- inner_join(mobile, landline, by=c("code", "year", "entity", "continent", "gdp_per_cap"))

df2 <- df %>%
  filter(entity == "United States") %>%
  gather(key = "phoneType", value = "subs", -entity, -code, -total_pop.x, -total_pop.y, -gdp_per_cap, -continent, -year)

#find the inverse to plot divergent bars
df2$inv <-  ifelse(df2$phoneType =="landline_subs",df2$subs+1,df2$subs)
df2 <- df2 %>% mutate(inv = ifelse(phoneType =="mobile_subs",subs,subs*-1))


 df2 %>%
  ggplot(aes(x = year, y = inv, fill = phoneType))+
  geom_bar(stat = "identity", position = "identity")+
  geom_hline(yintercept = 0, color = "white")+
  scale_y_continuous(breaks = seq(from = -75, to = 125, by = 15), labels=abs(seq(from = -75, to = 125, by = 15)))+
  scale_x_continuous(breaks = seq(from = 1990, to = 2017, by = 5))+
  scale_fill_manual(name = "Phone Type", labels = c("Landline", "Mobile"), values = c("#FFD166", "#00C49A"))+
  labs(
     x = "Year",
     y = "Mobile/Landline Phone Subs per 100 People",
     fill = "Phone Type",
     title ="US Phone Subscriptions over Time",
     subtitle = "Comparison of Landline vs. Mobile Phone Subscriptions \n (per 100 people) in the United States from 1990 to 2017"
   )+
   theme(
     plot.background = element_rect(fill = "#26547C"),
     panel.background = element_rect(fill = "#26547C"),
     axis.text = element_text(color = "white", size = 11),
     axis.title = element_text(color = "white", size = 12),
     axis.ticks.y = element_blank(),
     axis.ticks.x = element_line(color = "white"),
     panel.grid = element_blank(),
     axis.line.x = element_line(color="white"),
     axis.line.y = element_line(color="white"),
     plot.title = element_text(color = "white", size = 20, hjust = .5, face = "bold"),
     plot.subtitle = element_text(color = "white", size = 13, hjust = .5),
     legend.background = element_blank(),
     legend.text = element_text(color = "white", size = 11),
     legend.title = element_text(color = "white", size = 11),
     legend.key = element_blank()
   )
