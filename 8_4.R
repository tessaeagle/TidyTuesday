library(tidyverse)
library(RColorBrewer)

options(scipen=999)

energy_types <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-04/energy_types.csv')

energy_types$type <- as.factor(energy_types$type)
energy_types$country_name <- as.factor(energy_types$country_name)

energy_types <- na.omit(energy_types)
energy_types$yearsMean <- rowMeans(energy_types[,5:7], na.rm=TRUE)


#gather energy by year into two columns
df <- energy_types %>%
  gather(year, energy, 5:7)

#calc percentage of each energy type by year and country
df2 <- df %>% 
  group_by(country, year) %>% 
  mutate(percent = 100*energy/sum(energy)) 


# Stacked Bar - percentages -----------------------------------------------

p <- ggplot(df2, aes(x = country_name, y = percent, group = desc(type))) + 
  geom_bar(aes(fill=type), stat="identity", position = "stack")+
  facet_wrap(~year)+
  coord_flip()+ 
  scale_fill_brewer(palette = "RdYlBu", name = "Energy Type")+
  theme(
    panel.background = element_rect(fill="#abbac4"),
    plot.background = element_rect(fill="#abbac4"),
    axis.text = element_text(color="black"),
    legend.background = element_rect(fill="#abbac4"),
    plot.title = element_text(size=16, hjust=.3, face="bold"),
    plot.subtitle = element_text(size=14, hjust=.4),
    legend.position = "top",
    legend.justification="left",
    plot.margin = unit(c(.25,1,.5,0), "cm"),
    axis.text.y = element_text(size=8.5),
    panel.grid.major.x = element_blank(),
    strip.text = element_text(size=11, face="bold")
  )+
  labs(
    title = "Where Europe's Power Came From, 2016-2018",
    subtitle ="Percentage by Energy Type",
    x = "",
    y = "Percent"
  )


ggsave("C:/users/eagle/Desktop/R/TidyTuesday/8_4.png", p)



# Stacked Bar - total energy with breakdown -------------------------------
ggplot(energy_types, aes(x = reorder(country_name, -yearsMean), y = yearsMean)) + 
  geom_bar(aes(fill=type), stat="identity", position = "stack")+
  coord_flip()+ 
  scale_fill_brewer(palette = "RdYlBu", name = "Energy Type")+
  theme(
    panel.background = element_rect(fill="grey"),
    plot.background = element_rect(fill="grey"),
    axis.text = element_text(color="black"),
    legend.background = element_rect(fill="grey"),
    plot.title = element_text(size=15, hjust=.5),
    plot.subtitle = element_text(size=13, hjust=.5),
  )+
  labs(
    title = "Energy (GWh) Use by Country, 2016-2018",
    subtitle ="Average use by Energy Source",
    x = "",
    y = "Energy (GWh)"
  )


