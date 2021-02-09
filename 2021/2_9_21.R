library(tidyverse)
library(RColorBrewer)

student_debt <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/student_debt.csv')

df <- student_debt %>%
  group_by(year) %>%
  mutate(yearSum = sum(loan_debt)) %>%
  mutate(yearAv = mean(loan_debt)) %>%
  group_by(race) %>%
  mutate(proportion=paste0(round(100*loan_debt/yearSum,2)))

df$proportion <- as.numeric(as.character(df$proportion))  

df <- df %>%
  mutate(lab = paste0(round(proportion,0),"%"))
  #mutate(lab = replace(lab, lab <= 35, "")) 
  

p<-ggplot(df,aes(x=2,y=proportion, fill=race))+
  geom_bar(stat="identity",color="#042a38")+
  #geom_text(data = df, aes(label = lab), color="black", size = 1.6, position = position_stack(vjust=.7))+ 
  coord_polar(theta="y", start=0)+
  facet_wrap(~year,ncol=5)+
  xlim(.2,2.5)+
  scale_fill_brewer(palette = "YlGnBu")+
  theme(
    axis.text = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(hjust = .5, vjust = 6, size = 20, face = "bold",color = "white", margin = margin(b = -5)),
    plot.subtitle = element_text(hjust =.5, vjust = 7, size = 7, color = "white", margin = margin(b = 6)),
    plot.caption = element_text(size = 6, color = "white"),
    panel.grid = element_blank(),
    plot.background = element_rect(fill="#042a38",color=NA),
    panel.background = element_rect(fill="#042a38"),
    strip.background = element_blank(),
    strip.text.x = element_text(size = 12, color="white",face="bold"),
    legend.text = element_text(color="white"),
    legend.title = element_text(color="white"),
    legend.background = element_rect(fill = "#042a38"),
    legend.key = element_rect(fill="#042a38"),
    legend.position = "bottom"
  )+
  labs(
    title = "Student Loan Debt Over Time",
    subtitle = "Proportion of student loan debt (normalized to 2016 dollars) held by Black, Hispanic, and White\n families broken down every three years from 1989 to 2016. The average student loan debt\n across these groups in 1989 was $1,052, increasing to $10,942 in 2016.",
    fill = "Race",
    caption = "Plot by @tessuheagle \n Source: Urban Institute"
  )


ggsave('2_9_21_no_labels.jpg', p, height = 5, width = 5, bg = "#042a38", limitsize = F)



