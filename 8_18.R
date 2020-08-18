library(tidyverse)
library(maps)
library(mapproj)
library(ggpubr)


# Load Plant Data -----------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2020, week = 34)

plants <- tuesdata$plants
threats <- tuesdata$threats

#convert to factors
plants$country <- as.factor(plants$country)
plants$continent <- as.factor(plants$continent)
plants$red_list_category <- as.factor(plants$red_list_category)

#count number of extinct species by country
df <- plants %>%
  select(2:3, 24) %>%
  group_by(country, continent, red_list_category) %>%
  summarise(count=n())

#keep only Extinct values  
df <- df %>% 
  filter(red_list_category != "Extinct in the Wild")

#rename countries to match world map names
df <- df %>% mutate(country = case_when(country=="United States" ~ "USA",
                                  country=="Viet Nam" ~ "Vietnam",
                                  country=="Trinidad and Tobago" ~ "Trinidad",
                                  country=="Saint Helena, Ascension and Tristan da Cunha" ~ "Saint Helena",
                                  country=="Côte d'Ivoire" ~ "Ivory Coast",
                                  country=="Cabo Verde" ~ "Cape Verde",
                                  country=="Congo" ~ "Democratic Republic of the Congo",
                                  TRUE ~ as.character(country)))



# Map data ----------------------------------------------------------------
#load world map  
world <- map_data("world") %>% 
    filter(region != "Antarctica")

#join plant and world data
test <- left_join(df,world, by=c("country" = "region")) 

us <-subset(test, country == "USA" & subregion == "California")#subset to fix plotting issues, very messy...

test <- test %>%
  filter(country != "USA")#remove USA    

test1 <- rbind(us, test)#rejoin USA with World

#collapse to one set of long/lat points for each country
cnames <-aggregate(cbind(long, lat) ~ country, data=test1, 
                    FUN=function(x)mean(range(x)))

df2 <- left_join(test1, cnames, by=c("country"))


# Map plot------------------------------------------------------------------
p1 <- ggplot() +
    geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="#dbcca0", alpha=1) +
    geom_point(data=df2, aes(x=long.y, y=lat.y, size = count, color = count), alpha = .75) +
    scale_size(range = c(1,10), name="Extinct Species", breaks = seq(0, 100, by = 15))+
    theme_void() +
    coord_map(xlim=c(-180,180))+
    labs(
      title = "Where are plants going extinct?",
      subtitle = "Counts of extinct species by country in the past century"
    )+
    theme(
      plot.background = element_rect(fill = "#3d4e70", color = NA),
      plot.title = element_text(color = "white", size = 20, hjust = .5, vjust = 6, margin=margin(.1,0,0,0), face = "bold"),
      plot.subtitle = element_text(color = "white", size = 14, hjust = .5, vjust = 6, margin=margin(-.5,0,0,0)),#where is this??
      plot.margin = unit(c(1,0,0,0), "cm"),
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.text = element_text(color = "white"),
      legend.title = element_text(color = "white"),
      legend.margin=margin(t = -1, unit='cm')
    )+
    scale_color_gradient(guide = FALSE, low = "#9ef0d4", high = "#147856")+
    guides(size = guide_legend(nrow = 1, override.aes = list(color = c("#9ef0d4", "#82D8BB", "#67C0A2","#4BA888", "#30906F","#147856"))))




# Top threats plot ----------------------------------------------------------
agg <- aggregate(threats$threatened, by=list(type = threats$threat_type), FUN = sum)#aggregate types of threats

#dot plot
p2 <- agg %>%
  filter(x > 80) %>%
  ggplot(aes(reorder(type,-x),x, size = x))+
  geom_point(color="#67C0A2")+
  scale_size(range = c(2,6), limits = c(75,250), guide = FALSE)+
  coord_flip()+
  theme_minimal()+
  theme(
    plot.background = element_rect(fill = "#3d4e70", color = NA),
    axis.text = element_text(color="#dbcca0", size = 10.5),
    axis.title = element_text(color="#93bd91", size = 12, face = "bold"),
    legend.title = element_text(color="#dbcca0"),
    legend.text = element_text(color="#dbcca0"),
    plot.title = element_text(color = "white", size = 15, hjust=.15, face = "bold"),
    plot.margin = unit(c(.5,0,0,0), "cm")
  )+
  labs(
    title = "Top Threats to Endangered Plants",
    y = "",
    x = ""
  )

# Combine Plots -----------------------------------------------------------
p <- ggarrange(p1 , p2, ncol = 1, nrow = 2,
               heights = c(1.9, .75)) 

plot <- cowplot::ggdraw(p) + 
  theme(plot.background = element_rect(fill="#3d4e70", color = NA))

ggsave("8_18.png", plot)

