library(tidyverse)
library(ggimage)

tuesdata <- tidytuesdayR::tt_load('2020-07-14')
astronauts <- tuesdata$astronauts

img ="https://i.dlpng.com/static/png/375720_preview.png"
img2 = "https://img.freepik.com/free-photo/universe-space-sky-stars-night-time-milky-way_117930-55.jpg?size=626&ext=jpg"
img3 = "https://images.unsplash.com/photo-1520034475321-cbe63696469a?ixlib=rb-1.2.1&w=1000&q=80"

astronauts$nationality <- as.factor(astronauts$nationality)

#select relevant data
subset <- select(astronauts, name, nationality) %>%
  distinct() 

#rename levels
levels(subset$nationality)[levels(subset$nationality)=="Republic of South Africa"] <- "South Africa"
levels(subset$nationality)[levels(subset$nationality)=="U.S.S.R/Ukraine"] <- "USSR/Ukraine"
levels(subset$nationality)[levels(subset$nationality)=="U.S.S.R/Russia"] <- "USSR/Russia"
levels(subset$nationality)[levels(subset$nationality)=="Hungry"] <- "Hungary"

#group by nationality and sum
counts <- subset %>%
  group_by(nationality) %>%
  summarize(count = n())

counts <- arrange(counts, count) 

#grid for plotting
grid <- expand.grid(x = 8:1, y = 1:5)

#combine grid positioning with counts
data <- cbind(counts, grid)

#rocket plot
p <- ggplot(data, aes(x, y * 2)) +
  geom_text(aes(label = count), color="white", nudge_x = .17, nudge_y = .11, size=3.5, fontface="bold") +
  geom_text(aes(label = nationality), color="#2eb8b8", nudge_y = -1.25, size=3.75, fontface="bold") +
  geom_text(aes(label = "\U1F680"), color="#1d6fc2", size = 21, nudge_y = -0.075) +
  ggtitle("Number of Astronauts by Country, 1961-2019")+
  theme_void()+
  theme(
    plot.title = element_text(color="white",hjust = 0.5, size=20, margin=margin(0,0,8,0)),
    plot.margin = unit(c(.5,.25,.5,.5), "cm")
  )

ggbackground(p, img3)#add background img to plot


#bar chart of same data as above, different formatting
p2 <- ggplot(count, aes(x=reorder(nationality,-count), y=count))+
  geom_bar(stat="identity", fill="#33cccc")+
  coord_flip()+
  labs(
    y="",
    x="",
    title = "Number of Astronauts by Country"
  )+
  scale_y_continuous(breaks = seq(from = 0, to = 350, by = 25))+
  theme(
    axis.text = element_text(colour = "white",size=10),
    plot.title = element_text(color="white", hjust=.5, size=20),
    axis.title = element_text(colour = "white", size=15),
    panel.grid = element_blank(),
    panel.background = element_rect(fill="black"),
    plot.background = element_rect(fill="black"),
    axis.line = element_line(colour = "white"),
    legend.position = "none",
    plot.margin = unit(c(.1,.1,.1,.1), "cm")
    )

ggbackground(p2, img3)#add background img to plot





#older attempts, ignore
subset1 <- select(astronauts, name, nationality, year_of_mission, 20)

sum1 <- aggregate(subset1$hours_mission, by=list(year=subset1$year_of_mission, nation=subset1$nationality), FUN=sum)


ggplot(sum, aes(color=nation))+
  geom_point(subset(sum, nation %in% c("U.S.")), mapping = aes(x = year, y = x))+
  geom_line(subset(sum, nation %in% c("U.S.")), mapping = aes(x = year, y = x))+
  geom_point(subset(sum, nation %in% c("U.S.S.R/Russia")), mapping = aes(x = year, y = x))+
  geom_line(subset(sum, nation %in% c("U.S.S.R/Russia")), mapping = aes(x = year, y = x))+
  scale_x_continuous(name="Year", limits=c(1961, 2019))+
  ylab("Total Mission Hours")+
  theme(
    axis.text = element_text(colour = "white"),
    axis.title = element_text(colour = "white"),
    panel.grid = element_blank(),
    panel.background = element_rect(fill="black"),
    plot.background = element_rect(fill="black"),
    axis.line = element_line(colour = "white"),
    legend.position = "none")
 


