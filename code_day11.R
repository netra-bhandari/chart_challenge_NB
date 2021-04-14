library(tidyverse)
library(ggplot2)
library(viridisLite)
library(extrafont)
library(grid)

data <- read.csv("amazon.csv")

data <- data %>% filter(!number == 0)
data_sum <- data %>% group_by(state, month) %>% summarise(sum = round(sum(number)))
data_sum$state <- as.factor(data_sum$state)
data_sum$month <- factor(data_sum$month, levels = c("Janeiro" ,"Fevereiro","Março","Abril",
                                                    "Junho","Julho","Agosto","Setembro",
                                                    "Outubro","Novembro","Dezembro"))
data_sum <- drop_na(data_sum)
data_sum$id <- 1:253

label_data <- data_sum

# calculate the ANGLE of the labels
number_of_bar <- nrow(label_data)
angle <-  90 - 360 * (label_data$id-0.5) /number_of_bar     
label_data$hjust<-ifelse( angle < -90, 1, 0)

# flip angle BY to make them readable
label_data$angle<-ifelse(angle < -90, angle+180, angle)

background <- jpeg::readJPEG("1052x736_cmsv2_4e4f6b11-b44c-5adb-9055-d94566971e55-4889670.jpg")

(data_plot <- ggplot(data_sum, aes(x = state, y = sum, fill = month, na.rm = TRUE))+
            geom_bar(position = "stack", stat="identity",na.rm = TRUE)+
    scale_fill_viridis(discrete=TRUE, option = "B", direction = -1) +
    theme_classic()+
    coord_polar()+
  theme(axis.line=element_blank(),
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_text(size =15, color = "black",family= "Garamond"),
    panel.background = element_rect(fill = "grey90"),
    plot.background = element_rect(fill = "grey90"),
    legend.text = element_text(color = "black", size =12),
    legend.background = element_rect(fill = "grey90"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(color = "black", size = 13),
    plot.title = element_text(size = 30,family= "Garamond"),
    plot.subtitle = element_text(size = 22,family= "Garamond"),
    plot.caption =  element_text(size = 15,family= "Garamond")) +
    labs(title = "Fires across different states of Brazil ", fill = "Month",
          subtitle = "(Represented as sum of fires in each month between 1998-2017)",
          caption = "Data source: Kaggle| Design:@netra1128|#30DayChartChallenge"))
            
ggsave(data_plot, filename = "data_plot.png", width = 17, height = 10)
