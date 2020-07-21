library(tidyverse)
library(formattable)
library(RColorBrewer)

options(scipen = 999)

animal_outcomes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-21/animal_outcomes.csv')

df2 <- animal_outcomes %>%
  gather(state, n, 4:11) 

df2 <- aggregate(df2$n, by=list(outcome=df2$outcome, state=df2$state), FUN=sum)#sum outcomes across years and state
df2 <- na.omit(df2)  
df2$sum <- ave(df2$x, df2$state, FUN=sum)#get total number of outcomes by state
df2$perc <- (df2$x / df2$sum)#calculate percent of each outcome by state

df2 <- df2 %>% 
  mutate(perc2 = perc)#duplicate column to filter for labels an avoid gaps in plot

df2$perc2 <- as.numeric(df2$perc2)

df3 <- df2
df3 <- filter(df3, perc2 > .07)
#df3$lab <- percent(df3$perc2,d=0)

p <- ggplot(df2,aes(x=2,y=perc, fill=outcome))+
  geom_bar(stat="identity",color="black")+
  geom_text(data = df3, aes(label = lab), color="black", size=3.2, position = position_stack(vjust=.8))+ 
  coord_polar(theta="y", start=0)+
  facet_wrap(~state,ncol=4)+
  xlim(.2,2.5)+
  theme(
    axis.text = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(hjust=.5, vjust=6, size=20, face="bold",color="white"),
    plot.subtitle = element_text(hjust=.5, vjust=7, size=16, color="white"),
    panel.grid = element_blank(),
    plot.background = element_rect(fill="black",color=NA),
    panel.background = element_rect(fill="black"),
    #panel.border = element_rect(fill="black"),
    strip.background = element_blank(),
    strip.text.x = element_text(size = 14,color="white",face="bold"),
    legend.text = element_text(color="white"),
    legend.title = element_text(color="white"),
    legend.background = element_rect(fill = "black"),
    legend.key = element_rect(fill="black"),
    #panel.grid = element_line(color="black")
  )+
  scale_fill_brewer(palette = "RdBu")+
  labs(
      fill = "Outcomes",
      title = "Australian Animal Outcomes 1999-2018",
      subtitle = "Percentage of Total Outcomes by State/Territory"
  )

p<-ggsave("7_21.png", bg = "black")



  





