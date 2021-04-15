
## day 12 

library(ggplot2)
library(tidyverse)
library(extrafont)
library(viridis)

## 
data_75 <- read.csv("archive/finch_beaks_1975.csv")
data_12 <- read.csv("archive/finch_beaks_2012.csv")
heredity <- read.csv("archive/fortis_beak_depth_heredity.csv")
colnames(data_12) 
colnames(data_75) <- colnames(data_12)
data_75$year <- "1975"
data_12$year <- "2012"
beak_data <- rbind(data_75, data_12)

pal <- wes_palette("Royal2",2, type = "discrete")
dput(pal)
beak_data$species <- gsub("fortis", "G.fortis", beak_data$species)
beak_data$species <- gsub("scandens", "G.scandens", beak_data$species)

length <- beak_data %>%
  ggplot( aes(x = year, y = blength,color = year)) +
  geom_jitter(position=position_jitter(0.2), size = 3) +
  facet_wrap(~species, nrow = 2)+
  coord_flip()+
  labs(fill = "",
       y = "Beak Length",
       x = "Year",
       title = "Evolution of Beak lengths in Darwin finches",
       subtitle = expression(italic("Geospiza fortis and Geospiza scandens")),
       caption = "Data source : Kaggle| Design : @netra1128| #30DayChartChallenge")


  
length <- length+scale_color_brewer(palette="Set1")+theme_classic()+
  theme(plot.background = element_rect(fill = "#003152"),
        panel.background = element_rect(fill = "#003152"),
        strip.background = element_rect(fill = "#003152"),
        strip.text = element_text(color = "white"),
        strip.text.x = element_text(size = 22),
        axis.title.x = element_text(color = "white", size = 20),
        axis.title.y = element_text(color = "white", size = 20),
        axis.text.x = element_text(color = "white", size = 20),
        axis.text.y = element_text(color = "white", size = 20),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5, color = "white"),
        plot.subtitle = element_text(color = "white", size = 20, hjust = 0.5),
        plot.caption = element_text(color = "white", size = 10),
        legend.position = "none",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10))

length
ggsave(length, filename = "fig12.png", height = 9, width = 13)          
## day 13 
Correlation
library("ggpubr")


cor <-ggscatter(beak_data, x = "blength", y = "bdepth",color = "year",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Beak Length", ylab = "Beak Depth")+
  facet_wrap(~species+year, ncol = 2)+
  scale_color_brewer(palette="Set1")+
  theme_classic()+
  theme(plot.background = element_rect(fill = "#003152"),
        panel.background = element_rect(fill = "#003152"),
        strip.background = element_rect(fill = "#003152"),
        strip.text = element_text(color = "white"),
        strip.text.x = element_text(size = 22),
        axis.title.x = element_text(color = "white", size = 20),
        axis.title.y = element_text(color = "white", size = 20),
        axis.text.x = element_text(color = "white", size = 20),
        axis.text.y = element_text(color = "white", size = 20),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5, color = "white"),
        plot.caption = element_text(color = "white", size = 10),
        plot.subtitle = element_text(color = "white", size = 20, hjust = 0.5),
        legend.position = "none",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10))+
  labs(fill = "",
       x = "Beak Length",
       y = "Beak Depth",
       title = "Exploring correlation between beak length and beak depth in Darwin finches:",
       subtitle = expression(italic("Geospiza fortis and Geospiza scandens")),
       caption = "Data source : Kaggle| Design : @netra1128| #30DayChartChallenge")

cor
ggsave(cor, filename = "fig13.png", height = 9, width = 13)  
