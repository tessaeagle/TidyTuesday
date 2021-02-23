library(tidyverse)
library(reshape)
library(waffle)
library(magrittr)
library(hrbrthemes)


tuesdata <- tidytuesdayR::tt_load(2020, week = 43)

beer_awards <- tuesdata$beer_awards


df <- beer_awards %>%
  select(1,5,7)

df$medal <- as.factor(df$medal)
df$state <- as.factor(df$state)

levels(df$state)[levels(df$state)=="wa"] = "WA"
levels(df$state)[levels(df$state)=="Ak"] = "AK"

freq <- df %>%  
  group_by(medal, state, year) %>% 
  summarise(count = n(), .groups = 'drop')

freq %>%
  ggplot(aes(x=state,y=year))+
  geom_point(aes(color=medal), shape = 1)+
  #geom_bar(position = "dodge", stat = "identity")+
  coord_flip()+
  scale_color_manual(
    name = NULL,
    values = c("#FFD700", "#C0C0C0", "#b08d57"),
    labels = c("Gold", "Silver", "Bronze")
  )+
  theme(
    panel.background = element_rect(fill = NA),
    panel.grid.major.y = element_line(color="grey"),
    axis.ticks = element_blank()
  )+
  labs(
    x = "State",
    y = "Year"
  )



# OLD ---------------------------------------------------------------------


freq %>%
  ggplot(aes(fill = medal, values = count)) +
  geom_waffle(n_rows = 5, size = .5, flip = FALSE, make_proportional = TRUE, color = "gray50") +
  facet_wrap(~state, nrow = 10, strip.position = "bottom") +
  #guides(fill = guide_legend(nrow = 1))+
  coord_equal()+
  theme_minimal()+
  theme_enhance_waffle()+
  theme(
    axis.text = element_text(face = "bold")
  )+
  scale_fill_manual(
    name = NULL,
    values = c("#FFD700", "#C0C0C0", "#b08d57"),
    labels = c("Gold", "Silver", "Bronze")
  )

freq %>%
  filter(count > 36) %>%
  ggplot(aes(x=reorder(state,count),y=count,fill=medal))+
  geom_point(aes(color=medal, size = count))+
  #geom_bar(position = "dodge", stat = "identity")+
  coord_flip()+
  scale_color_manual(
    name = NULL,
    values = c("#FFD700", "#C0C0C0", "#b08d57"),
    labels = c("Gold", "Silver", "Bronze")
  )+
  labs(
    x = "State",
    y = "Count"
  )
