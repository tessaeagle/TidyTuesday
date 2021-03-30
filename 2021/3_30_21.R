#Tessa Eagle
#TT 3-30-21

library(tidyverse)
library(extrafont)
library(ggtext)

allShades <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-30/allShades.csv')


# All shades --------------------------------------------------------------
df <- allShades %>%
  group_by(hex) %>%
  summarise(count = n()) %>%
  mutate(line = row_number()) %>%
  #there is probably a better way to split the groups than by hand
  mutate(row = 
    case_when(
      line < 1564 ~ 1,
      line > 1563 & line < 3128 ~ 2,
      line > 3127 & line < 4691 ~ 3,
      line > 4690 ~ 4
    )
  )%>%
  group_by(row) %>%
  mutate(x_val = row_number())

p2<-df %>%
  ggplot()+
  geom_point(aes(x = x_val, y = row, color = hex, size = count), position = "jitter")+
  scale_colour_identity()+
  ylim(0,5.4)+#increase range due to jitter from 5 to 5.4
  scale_size(range = c(1,5))+
  coord_polar()+
  theme_minimal()+
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "#eb3156", color = NA),
    panel.background  = element_rect(fill = "#eb3156", color = NA),
    plot.title = element_text(size = 32, face = "bold", hjust = .5, family = "Gill Sans MT"),
    plot.subtitle = element_markdown(size = 16, hjust = .5, family = "Gill Sans MT"),
    plot.caption = element_text(size = 12, hjust = 1.6),
    legend.position = "none"
  )+
  labs(
    title = "Makeup Shade Range",
    subtitle = "Shade colors for 6,254 foundations from 107 different brands. Larger points indicate shades<br> that occur in multiple instances. <span style = 'color: #F9F9F9;'>Two</span> <span style = 'color: #FDFCFB;'>shades</span> appeared most frequently at 6 times each.",
    caption = "Plot: @tessuheagle | Data: The Pudding"
  )


plot2 <- cowplot::ggdraw(p2) + 
  theme(plot.background = element_rect(fill="#eb3156", color = NA))

ggsave("3_30_21_all_shades.png", plot2, height = 7, width = 9)



  



# Top Ten Brands ----------------------------------------------------------

#find top ten brands
allShades %>%
  group_by(brand) %>%
  distinct(hex) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  top_n(10)

#probably a way to combine this with the first step
df2 <- allShades %>%
  select(brand, hex) %>%
  filter(brand %in% c("FENTY BEAUTY by Rihanna", "Too Faced","Estée Lauder","MAKE UP FOR EVER","Laura Mercier","MAC","SEPHORA COLLECTION","Clinique","Tarte","bareMinerals")) %>%
  group_by(brand) %>% 
  mutate(row = cur_group_id()) %>%
  group_by(row) %>%
  mutate(x_val = row_number())
  
levels(df2$brand)[levels(df2$brand)=="FENTY BEAUTY by Rihanna"] <- "FENTY BEAUTY"
  

labs <- df2 %>%
  select(brand, row) %>%
  group_by(brand) %>% 
  slice(1)
  
p<-df2 %>%
  ggplot()+
  geom_point(aes(x = hex, y = row, color = hex), size = 2.5)+
  geom_text(data = labs, aes(x = 0, y = row, label = brand), nudge_y = .4, size = 3)+
  scale_colour_identity()+
  ylim(0,10.5)+
  coord_polar()+
  theme_minimal()+
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "#57a9fa", color = NA),
    panel.background  = element_rect(fill = "#57a9fa", color = NA),
    plot.title = element_text(size = 32, face = "bold", hjust = .5, family = "Gill Sans MT"),
    plot.subtitle = element_text(size = 16, hjust = .5, family = "Gill Sans MT"),
    plot.caption = element_text(size = 12, hjust = 1.6),
    legend.position = "none"
  )+
  labs(
    title = "Makeup Shades",
    subtitle = "Foundation colors for the ten brands with the highest number of unique \nshade values. bareMinerals has the highest number at 284 different shades.",
    caption = "Plot: @tessuheagle | Data: The Pudding"
  )

plot <- cowplot::ggdraw(p) + 
  theme(plot.background = element_rect(fill="#57a9fa", color = NA))



ggsave("3_30_21.png", plot, height = 7, width = 9)

