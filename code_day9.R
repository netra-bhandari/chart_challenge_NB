## day 9 
## statistics

#IPL stats
library(dplyr)
library(tidyverse)
library(ggplot2)
library(png)
library(grid)

ipl_bat <- read.csv("all_season_batting_card.csv")
ipl_bowl <- read.csv("all_season_bowling_card.csv")
ipl_summary <- read.csv("all_season_summary.csv")

#matches won per team each year
won <- ipl_summary %>% select(season,winner,home_captain, away_captain)
won_team <- won %>% group_by(season, winner) %>% summarise(n = table(winner))
won_team <- won_team %>% group_by(winner) %>% summarise(sum = sum(n))
won_team$id <- seq(1,13)
label_data <- won_team

# calculate the ANGLE of the labels
number_of_bar <- nrow(label_data)
angle <-  90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)


label_data$hjust<-ifelse( angle < -90, 1, 0)

label_data$angle<-ifelse(angle < -90, angle+180, angle)
label_data$names <- paste(won_team$winner, "(", label_data$sum, ")")

csk <- "#FFFF3C"
dc <-  "#00008B"
gl <- "#E04F16"
kkr <- "#2E0854"
kochi <- "#632B72"
kxip <- "#A7A9AC"
mi <- "#004BA0"
pwi <-  "#2F9BE3"
no_result <- "grey90"
rcb <- "#EC1C24"
rps <- "#D11D9B"
rr <- "#D1AB3E"
srh <- "#FF822A"



a <- ggplot(won_team,aes(x=winner, y= sum) )+
  geom_bar(stat="identity", fill = c(csk, dc, gl , kkr,kochi,kxip,mi,no_result, pwi,      
                                      rcb, rps,rr,srh ), show.legend = F)+
  ylim(-100,120) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.caption = element_text(hjust = 0.5),
    plot.title = element_text(hjust = 0.7)
  ) +
  coord_polar(start = 0)+
  geom_text(data=label_data, aes(x=id, y=sum+5, label=names, hjust=hjust),
            color="black", fontface="bold",alpha=1, size=3, angle= label_data$angle, inherit.aes = T ) +
  labs(caption = fullname, title = "Total matches won per team in all seasons")
  
a

## per season
won <- ipl_summary %>% select(season,winner,home_captain, away_captain)
won_team_2020 <- won %>% group_by(season, winner) %>% summarise(n = table(winner))
fullname <- c("CSK = Chennai Super Kings, DC = Delhi Capitals,GL = Gujrat Lions,KKR = Kolkata Knight Riders,Kochi = Kochi Tuskers Kerala,
       KXIP = Kings XI Punjab,MI = Mumbai Indians, PWI = Pune Warrios India,RCB = Royal Challengers Bangalore,RPS = Rising Pune Supergiant,
       RR = Rajasthan Royals,SH = Sunrisers Hyderabad")

won_team_2020 <- won_team_2020 %>% filter(season == "2020")
cols <- c(csk,dc,kkr,kxip,mi,rcb,rr,srh)

b <- ggplot(won_team_2020,aes(x= fct_reorder(winner,-n), y= n) )+
  geom_bar(stat="identity",show.legend = F, fill =  cols)+
  theme_classic()+
  ylab("Number of matches won in Season - 2020")+
  xlab("")+
  scale_y_continuous(limits = c(0,12), expand = c(0, 0))+
  theme(axis.text.y = element_text( size =10, face = "bold" ),
        axis.text.x =  element_text( size =10, face = "bold" ))
  

b

library(ggpubr)
figure <- ggarrange(a, b + font("x.text", size = 10),
                    ncol = 1, nrow = 2,heights = c(3, 2))
c <-annotate_figure(figure,
                top = text_grob("Indian Premier League - Statistics", color = "darkblue", face = "bold", size = 20, hjust = 1.5),
                bottom = text_grob("Data source : Kaggle| Design : @netra1128 | #30DayChartChallenge", color = "darkblue",
                                   hjust = 1, x = 1, face = "italic", size = 10),
                left = text_grob("Lets see who will win this season!!", color = "darkblue", rot = 90),
                right = ""
)

ggsave(c, filename = "c.png", dpi = 300, height =10, width = 15)
