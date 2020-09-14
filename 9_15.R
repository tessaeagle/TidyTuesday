library(tidyverse)
library(geojsonio)
library(rgdal)
library(broom)
library(rgeos)
library(viridis)

# Load TT Data ------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2020, week = 38)
kids <- tuesdata$kids


# Load Map Data -----------------------------------------------------------
# Instructions from: https://www.r-graph-gallery.com/328-hexbin-map-of-the-usa.html
spdf <- geojson_read("us_states_hexgrid.geojson",  what = "sp")

# Bit of reformating
spdf@data = spdf@data %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))

plot(spdf)

spdf@data = spdf@data %>% mutate(google_name = gsub(" \\(United States\\)", "", google_name))
spdf_fortified <- broom::tidy(spdf, region = "google_name")

#Determine centers for labels
centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2))

#filter for SNAP
df <- kids %>%
  filter(variable == "SNAP")

#Join map data with kids data
test <- spdf_fortified %>%
  left_join(. , df, by=c("id"="state")) 


# Map Plotting ------------------------------------------------------------
p <- test %>%
  filter(year == "1997") %>%
ggplot() +
  geom_polygon(aes(fill = inf_adj_perchild, x = long, y = lat, group = group)) +
  geom_text(data = centers, aes(x = x, y = y, label = id), color = "grey") +
  scale_fill_viridis()+
  #scale_fill_brewer(palette = "RdBu")+
  theme_void() +
  coord_map()+
  theme(
    plot.title = element_text(hjust = .6, vjust = 5, size = 20, face = "bold"),
    plot.subtitle = element_text(hjust = .75, vjust = 6, size = 13),
  )+
  labs(
    title = "SNAP Spending per Child",
    subtitle = "Public spending by state on the Supplemental Nutrition Assistance Program \n per child (0-18) between 1997 and 2016, with adjustments for inflation  ",
    fill = "Money per Child \n (Thousands)"
  )

cowplot::ggdraw(p) + 
  theme(plot.background = element_rect(fill="#95b0cc", color = NA))+
  cowplot::draw_figure_label(label = "1997", position = "top.right", size = 20)
