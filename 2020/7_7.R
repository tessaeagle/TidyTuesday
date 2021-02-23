install.packages("tidytuesdayR")
install.packages("forcats")
install.packages("ggpubr")
install.packages("ggthemes")


library(plyr)
library(tidyverse)
library(reshape2)
library(scales)
library(tibble)
library(forcats)
library(ggpubr)
library(ggthemes)


tuesdata <- tidytuesdayR::tt_load('2020-07-07')
coffee_ratings <- tuesdata$coffee_ratings


subset <- dplyr::select(coffee_ratings, country_of_origin, number_of_bags, 21:31)
subset <- melt(subset, id=c("country_of_origin"))

#rename variables
levels(subset$variable)[levels(subset$variable)=="clean_cup"] ="clean cup"
levels(subset$variable)[levels(subset$variable)=="cupper_points"] ="cupper points"


t <- count(coffee_ratings, 'country_of_origin')#occurrences of each country
names(t) <- c("country_of_origin", "freq")
w <- subset %>% inner_join(t)#join frequency table with subset


#group by country & average scores
stats <- w %>% 
  group_by(country_of_origin, variable, freq) %>% 
  dplyr::summarise(avg = mean(value))

#filter for top 10 countries of origin to fit on combined plot
most_freq <-  t %>%
  filter(freq >= 40)

most_freq$country_of_origin[most_freq$country_of_origin =="Tanzania, United Republic Of"] <- "Tanzania"
most_freq$country_of_origin[most_freq$country_of_origin =="United States (Puerto Rico)"] <-"Puerto Rico"
most_freq$country_of_origin[most_freq$country_of_origin =="United States (Hawaii)"] <-"Hawaii"
most_freq$country_of_origin[most_freq$country_of_origin =="Cote d?Ivoire)"] <-"Cote d'Ivoire"


#bar chart for top 10 countries of origin
bar <- ggplot(most_freq, aes(x=reorder(country_of_origin,-freq), y=freq))+
    geom_bar(stat="identity", fill="darkcyan")+
    coord_flip()+
    xlab("Country")+
    ylab("Count")+
    theme_economist()+
    ggtitle("Most Frequent Countries of Origin")+
    theme(
      axis.text = element_text(size=12),
      axis.title = element_text(size=14),
      axis.title.y = element_text(vjust=4),
      axis.title.x = element_text(vjust=-1),
      plot.title = element_text(size=14,vjust=2,hjust=-.25),
      plot.margin=unit(c(.5,.5,.2,.5), "cm")
    )
    
  
#scores for top 5 countries
scores <- stats %>%
  filter(country_of_origin == "Colombia" | country_of_origin == "Mexico" 
         | country_of_origin == "Guatemala" | country_of_origin == "Brazil" | country_of_origin == "Taiwan")


#dot plot for grades of top 5 countries of origin
dot <- ggplot(scores, aes(x = variable,
                     y = avg,
                     color = country_of_origin)) +
    geom_point(size=3)+
    coord_flip()+
    xlab("Grade Variable")+
    ylab("Average Score")+
    scale_colour_discrete("Country of Origin")+
    theme_economist()+
    ggtitle("Coffee Grades for Top 5 Countries of Origin")+
    theme(
      axis.text = element_text(size=12),
      axis.title = element_text(size=14),
      axis.title.y = element_text(vjust=4),
      axis.title.x = element_text(vjust=-1),
      plot.title = element_text(size=14, hjust=-.4),
      plot.margin=unit(c(0,.5,.5,.5), "cm")
    )
  
#combine plots
ggarrange(bar, dot, 
          ncol = 1, nrow = 2,
          heights = c(1.2, 1.5))

