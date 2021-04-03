#load data
library(dplyr)
remotes::install_github("davidsjoberg/ggstream")
library(ggstream)
library(ggplot2)
library(wesanderson)

#Data source
#https://ourworldindata.org/economic-inequality-by-gender

data <- read.csv("gender-inequality-index-from-the-human-development-report.csv")
neighbors <- c("India","Pakistan","China","Indonesia",
              "Afghanistan","Nepal","Bhutan","Sri Lanka", 
              "Myanmar","Bangladesh")
data <- data[data$Entity %in% neighbors ,]
data <- data %>% select(-"Code")
data$Entity <- as.factor(data$Entity)
names(data) <- c("Country", "Year", "Index")
#streamline plots

plot_1 <- ggplot(data, aes(x = Year,y = Index, fill = Country)) +
  geom_stream(position = "identity", type = "proportional") + # (type = ridge)
  scale_fill_manual(values = c("#F3D2B3", "#F2B8A2", "#F38C8D", "#5E9DB8", "#ADA296", "#2C5F72",
                               "#F5CDB4", "#F8AFA8", "#F8AFA8", "#FDDDA0")) +
  theme_classic() +
  theme(legend.position="bottom") +
  ylab("Gender Inequality Index") +
  xlab("Year") +
  theme(legend.position = "right",#plot.background = element_rect(fill = "white"),
        text = element_text(color ="black"),
        axis.title.x = element_text(color = "black", size = 17),
        axis.title.y = element_text(color = "black", size = 17),
        axis.text.x = element_text(color = "black", size = 15),
        axis.text.y = element_text(color = "black", size = 15),
        plot.title = element_text(size = 19, face = "bold", hjust = 0.5),
        plot.caption = element_text(color = "black", size = 7),
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 15),
        plot.background = element_rect(fill = "white"))+
  labs(title = "Gender Inequality index for India and its neighboring countries (1995-2015)", 
       subtitle = "This index covers three dimensions: reproductive health, empowerment, and economic status. \nHigher values indicate higher inequalities",
       caption = "#30DayChartChallenge 
               Data source: Our world in data
               Design: @netra1128")

ggsave(plot_1, filename = "day3.png", width = 15, height = 7,unit = "in" , dpi = 300)

