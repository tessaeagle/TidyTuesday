library(tidytuesdayR)
library(tidyverse)
library(extrafont)
library(ggimage)
library(ggpubr)

tuesdata <- tidytuesdayR::tt_load(2021, week = 8)

conjugal <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/conjugal.csv')

bg <- "https://w7.pngwing.com/pngs/982/58/png-transparent-torn-brown-paper-illustration-paper-parchment-scape-house-lukkoye-paper-sheet-miscellaneous-material-file-folders.png"

df <- conjugal %>% 
  rename("Widowed and Divorced" = "Divorced and Widowed") %>%
  gather(key = "MaritalStatus", value = "Percent", -Population, -Age) 


df$Population <- factor(df$Population, levels=c("Negroes", "Germany"))
df$Age <- factor(df$Age, levels=c("15-40", "40-60", "60 and over"))
levels(df$MaritalStatus)

labs <- data.frame(
  Age = c("15-40", "40-60", "60 and over"),
  label = c("AGE 15-40", "40-60", "60 AND OVER")
)


p<-df %>%
ggplot(aes(x = Population, y = Percent))+
  geom_bar(aes(fill = as.factor(MaritalStatus)), stat = "identity", position = "stack", color = "black", width = .5, key_glyph = "point")+
  geom_text(aes(x = Population, y = Percent, label = paste0(Percent, "%")), position = position_stack(vjust = 0.52), size = 3.2, family = "mono")+ 
  coord_flip(clip = "off", ylim = c(0,100))+
  facet_wrap(~Age, ncol = 1)+
  scale_fill_manual(values = c("#de2745", "#e6ac3d", "#50715e"), breaks = c("Single", "Married", "Widowed and Divorced"))+
  guides(fill = guide_legend(nrow = 2, override.aes = list(size = 8, colour = c("#de2745", "#e6ac3d", "#50715e"))), guide = guide_legend(reverse = TRUE))+
  labs(
    title = "CONJUGAL CONDITION"
  )+
  theme(
    panel.background = element_rect(fill = NA),
    plot.background = element_rect(fill = NA),
    strip.text.x = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "top",
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.spacing.x = unit(.5, 'cm'),
    legend.text = element_text(family = "mono", size = 14),
    legend.background = element_rect(fill = NA),
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(family = "mono", size = 14, margin = margin(l = 3, unit = "cm")),
    plot.title = element_text(color = "#443933", hjust = .4, family = "mono", face = "bold", size = 16, margin = margin(t = .5, unit = "cm"))
    )+
  geom_text(x = 1.6, y = -37, aes(label = label), data = labs, color = "black", family = "mono", size = 3.1)+
  annotate("text", x = 1.6, y = -27, label = '{', size = 22, color = '#443933')



p2<-ggbackground(p, bg)


ggsave("2_16_21.png", p2, width = 7, heigh = 8)

