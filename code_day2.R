library(tidyverse)
library(echarts4r)
library(echarts4r.assets)

## download data from Our world in data 

d <- read.csv("hiv-death-rates-by-age.csv", header = T, sep = ",")

## Select India and latest year

d <- d %>% filter(Entity == "India")
d <- d %>% filter(Year == "2017")

d <- d[,c(4:9)]
colnames(d) <- c("0-5 years old","70+ years old","5-14 years old","15-49 years old","50-69 years old","All ages")

d <- d %>% gather(Age_group, Deaths, 1:6) 

d <- d %>% arrange(factor(Age_group,levels= c("0-5 years old","5-14 years old","15-49 years old","50-69 years old","70+ years old","All ages")))

## plot using echarts

 d %>% 
  e_charts(Age_group) %>% 
  e_pictorial(Deaths, symbol = ea_icons("user"), 
              symbolRepeat = TRUE, z = -1,
              symbolSize = c(20, 20)) %>% 
  e_theme("roma") %>%
  e_title("Which population groups are most at risk from HIV/AIDS in India (2017)?", subtext =  
          "Death rates from HIV/AIDS, measured as the number of deaths per 100,000 individuals across various age categories") %>% 
  e_flip_coords() %>%
  e_color( color = "black", background = "beige") %>% 
  # Hide Legend
  e_legend(show = FALSE) %>%
  # Remove Gridlines
  e_x_axis(splitLine=list(show = FALSE)) %>%
  e_y_axis(splitLine=list(show = FALSE)) %>%
  # Format Label
  e_labels(fontSize = 20, fontWeight ='bold', position = "right", offset=c(10, 0))


#export the plot to add source and caption in image editor
# I couldnt find the option to save the plot through echarts
