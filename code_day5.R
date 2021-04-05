library(tidyverse)
library(showtext)
library(ggrepel)
library(jpeg)
library(png)
library(grid)

vg <- read.csv("vgsales.csv")
head(vg)
vg_count <- vg %>% group_by(Name,Year) %>% tally()
vg_fifa <- vg %>% filter(str_detect(Name, "FIFA"))
vg_fifa_17 <- vg_fifa %>% filter(Name == c("FIFA 17"))
vg_fifa_16 <- vg_fifa %>% filter(Name == c("FIFA 16"))
fifa <- rbind(vg_fifa_16, vg_fifa_17)
colnames(fifa) <- c("Rank"   ,      "Name" ,        "Platform" ,    "Year"   ,
                    "Genre"   ,     "Publisher"  ,"North America" ,"EU","Japan","Other Sales"  ,"Global Sales")

font_add_google("Montserrat", "Montserrat")
showtext_auto()

fifa <- fifa %>% gather(Sales, Value, 7:11)
fifa$Sales <- factor(fifa$Sales, levels = c( "Global Sales","North America","EU",
                                                "Japan","Other Sales"))
                                                
background <- jpeg::readJPEG("football-isolated-black-background_118019-1293.jpg")


g <- ggplot(fifa, aes(x = Year, y = Value, group = Platform )) +
  geom_line(aes(color = Platform), size = 1, alpha = 0.8) +
  geom_point(aes(color = Platform), size = 3, alpha = 0.8) +
  scale_colour_manual(values = c("darkgreen", "#F9ADA0", "#F9627D", "#C65B7C", "#5B3758")) +
  scale_x_discrete(position = "bottom", labels = c("FIFA 16", "FIFA 17")) +
  facet_wrap(~Sales, ncol = 5)+
  labs(title = "FIFA Sales between 2015 and 2016",
       subtitle = "Sales (in millions) across different platforms",
       caption = "Data source: Kaggle(Video Games Sales Dataset),
       Design: @netra1128 #30DaysChartChallenge)")+
  theme_classic() +
  theme(plot.background = element_rect("gray90"),
        panel.background = element_rect("gray90"),
        strip.background = element_rect(colour="#2F4858",
                                        fill="gray90", linetype="solid"),
        strip.text = element_text(face = "bold", colour = "#2F4858", size = 17),
    text = element_text(family = "Montserrat"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(hjust = 2.5, size = 14, face = "bold", colour = "#2F4858"),
        plot.subtitle = element_text(size = 16, colour = "#2F4858", face = "bold"),
        plot.caption = element_text(size = 10, colour = "#2F4858", face = "bold"),
        plot.title = element_text(size = 22,face = "bold", colour = "#2F4858"),
        axis.text.x = element_text(size = 14, face = "bold", colour = "#2F4858"),
        legend.position = "bottom",legend.title=element_text(size=17), 
    legend.text=element_text(size=16))

ggsave("day5.png",g)
