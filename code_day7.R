### day 7 
#ridges

library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggridges)


data <- read.csv("indian_food.csv")

west <- filter(data, region == "West")
north <- filter(data, region == "North")
south <- filter(data, region == "South")
east <- filter(data, region == "East")
north_east <- filter(data, region == "North East")
central <- filter(data, region == "Central")

data <- rbind(west, north, north_east, east,south, central)

data <- data %>% gather(Time, value , 4:5)
names(data)[7] <- "Region"

a <- ggplot(data, aes(x = value, y = Region, color = Time, point_color = Time, fill = Time)) +
  geom_density_ridges(
    jittered_points = TRUE, scale = .95, rel_min_height = .01,
    point_shape = "|", point_size = 3, size = 0.25,
    position = position_points_jitter(height = 0),
    alpha = 0.7
  ) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0), name = "Time (minutes)") +
  scale_fill_manual(values = c("darkgreen", "purple"), labels = c("Preparation time", "Cooking time")) +
  scale_color_manual(values = c("darkgreen", "purple"), guide = "none") +
  scale_discrete_manual("point_color", values = c("darkgreen", "purple"), guide = "none") +
  coord_cartesian(clip = "off") +
  guides(fill = guide_legend(
    override.aes = list(
      fill = c("darkgreen", "purple"),
      color = NA, point_color = NA)
  )
  ) +
  theme_ridges(center = TRUE)+theme_classic()+
  theme(plot.background = element_rect(fill = "grey90"),
        panel.background = element_rect(fill = "grey90"),
        legend.background = element_rect(fill = "grey90"),
        axis.title.x = element_text(color = "black", size = 10),
        axis.title.y = element_text(color = "black", size = 10),
        axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color = "black",face = "bold", hjust = 0.5, size = 15),
        plot.caption = element_text(color = "black", size = 10,face = "bold"))+
  labs(title = "Exploring the Indian food dataset",
                     subtitle = "Preparation and cooking time for regional meals",
                    caption = "#30DayChartChallenge Data source: Kaggle Design: @netra1128")


ggsave(a, filename ="day7.png", height = 7, width = 9)


