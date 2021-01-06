library(tidyverse)
library(countrycode)
library(ggpubr)
library(cowplot)

transit_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-05/transit_cost.csv')

transit_cost$real_cost <- as.numeric(transit_cost$real_cost)

transit_cost$fullcountry <- countrycode(transit_cost$country, "iso2c", "country.name")#convert country codes to full name

transit_cost$continent <- countrycode(transit_cost$fullcountry, origin = "country.name", destination = "continent")#convert country to continent


p1 <- transit_cost %>%
  filter(length <= 200 & length > 0 & continent != "NA" & real_cost < 50000) %>% 
  #filtered outliers for cleaner plotting, removed 1ish datapoint
  ggplot(aes(x = length, y = real_cost))+
    geom_point(aes(color = continent))+
    geom_smooth(method = "lm", color = "yellow")+
    labs(
      y = "",
      x = "Proposed Length (KM)",
      title = "Transit Spending by Length of Transit Line"
    )+
  theme(
    plot.background = element_rect(fill = "#132a45", color = NA),
    panel.background = element_rect(fill = "#132a45"),
    axis.text = element_text(color = "white", size = 20),
    axis.title = element_text(color = "white", size = 25),
    legend.position = "none",
    plot.title = element_text(color = "#62bfbc", hjust = .5, size = 28),
    plot.margin = unit(c(2,1.5,1,1), "cm"),
    panel.grid = element_line(color = "#4d4d4d")
)


p2 <- transit_cost %>%
  filter(continent != "NA" & real_cost < 50000) %>% 
  ggplot(aes(x = stations, y = real_cost))+
  geom_point(aes(color = continent))+
  scale_x_continuous(breaks = c(0,20,40,60,80,100,120,140))+
  geom_smooth(method = "lm", color = "yellow")+
  labs(
    y = "",
    x = "Number of Stations",
    title = "Transit Spending by Number of Stations"
  )+
  theme(
    plot.background = element_rect(fill = "#132a45", color = NA),
    panel.background = element_rect(fill = "#132a45"),
    axis.text = element_text(color = "white", size = 20),
    axis.title = element_text(color = "white", size = 25),
    plot.title = element_text(color = "#62bfbc", hjust = .5, size = 28),
    panel.grid = element_line(color = "#4d4d4d"),
    legend.background = element_rect(fill = "#132a45"),
    legend.position = "none",
    plot.margin = unit(c(1,1.5,1,1), "cm")
  )


p3 <- transit_cost %>%
  filter(continent != "NA" & real_cost < 50000) %>% 
  ggplot(aes(x = continent, y = real_cost))+
  geom_point(aes(color = continent))+
  geom_smooth(method = "lm", color = "yellow")+
  labs(
    y = "",
    x = "Continent",
    title = "Transit Spending by Continent"
  )+
  theme(
    plot.background = element_rect(fill = "#132a45", color = NA),
    panel.background = element_rect(fill = "#132a45"),
    axis.text = element_text(color = "white", size = 20),
    axis.title = element_text(color = "white", size = 25),
    plot.title = element_text(color = "#62bfbc", hjust = .5, size = 28),
    panel.grid = element_line(color = "#4d4d4d"),
    legend.background = element_rect(fill = "#132a45"),
    legend.text = element_text(color = "white", size = 20),
    legend.title = element_text(color = "white", size =20),
    legend.key = element_rect(fill = NA),
    legend.position = "bottom",
    plot.margin = unit(c(1,1.5,1,1), "cm")
  )


plot <- ggarrange(p1, p2, p3, ncol = 1)


plot2 <- ggdraw(plot) + draw_label("Real Cost in Millions (USD)", color = "white", size = 26, angle = 90, vjust = -32.5)+
  draw_label("Exploring Factors Related to Transit Costs", color = "white", size = 40, vjust = -23.5)

ggsave('1_5.png', plot2, height = 20, width = 18, limitsize = F)


