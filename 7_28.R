library(tidyverse)
library(ggridges)
library(ggpubr)
library(cowplot)


penguins <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-28/penguins.csv')
penguins_raw <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-28/penguins_raw.csv')


penguins <- na.omit(penguins)


# Flipper Length ----------------------------------------------------------

p1 <- ggplot(penguins, aes(x = flipper_length_mm, y = species, fill = sex)) +
  geom_density_ridges(
    scale = 1, rel_min_height = .01, alpha=.75
  )+
  theme_ridges(center = TRUE, grid = TRUE)+
  theme(
    plot.background = element_rect(fill="#2D4654"),
    panel.background = element_rect(fill="#2D4654"),
    strip.background = element_blank(),
    plot.title = element_text(hjust=-.7, size=18, face="bold",color="#E1CDB5"),
    axis.title = element_text(color="#E1CDB5",size=14,face="bold"),
    axis.text.x = element_text(color="white", size=14),
    axis.text.y = element_text(color="#E1CDB5",size=14),
    strip.text.x = element_text(size = 14,color="white",face="bold"),
    legend.text = element_text(color="white"),
    legend.title = element_text(color="white"),
    legend.background = element_rect(fill = "#2D4654"),
    legend.key = element_rect(fill="#2D4654")
  )+
  scale_fill_manual(values = c("#6C9A8B", "#c6e6fb"), labels = c("Female", "Male")) +
  scale_y_discrete(expand = c(0, 0))+
  labs(
    x="Flipper Length",
    y="",
    title="Antarctic Penguin Species Features",
    fill="Sex"
  )
  

# Bill Length -------------------------------------------------------------

p2 <- ggplot(penguins, aes(x = bill_length_mm, y = species, fill = sex)) +
  geom_density_ridges(
    scale = 1, rel_min_height = .01, alpha=.75
  )+
  theme_ridges(center = TRUE, grid = TRUE)+
  theme(
    plot.background = element_rect(fill="#2D4654"),
    panel.background = element_rect(fill="#2D4654"),
    strip.background = element_blank(),
    plot.title = element_text(hjust=-.5, size=16, face="bold",color="white"),
    axis.title = element_text(color="#E1CDB5",size=14,face="bold"),
    axis.text.x = element_text(color="white", size=14),
    axis.text.y = element_text(color="#E1CDB5",size=14),
    strip.text.x = element_text(size = 14,color="white",face="bold"),
    legend.text = element_text(color="white"),
    legend.title = element_text(color="white"),
    legend.background = element_rect(fill = "#2D4654"),
    legend.key = element_rect(fill="#2D4654")
  )+
  scale_fill_manual(values = c("#6C9A8B", "#c6e6fb"), labels = c("Female", "Male")) +
  scale_y_discrete(expand = c(0, 0))+
  labs(
    x="Bill Length",
    y=""
  )



# Combine Plots -----------------------------------------------------------
p <- ggarrange(p1 , p2,
          common.legend = TRUE, legend = "right",
          ncol = 1) 

plot <- cowplot::ggdraw(p) + 
  theme(plot.background = element_rect(fill="#2D4654", color = NA))+
 draw_label(label = "Species", x = 0, y = .5, hjust = .5, vjust = 1.3, angle = 90,
                  size = 16, fontface = "bold", color = "white")

ggsave("C:/users/eagle/Desktop/R/TidyTuesday/7_28.png", plot)

