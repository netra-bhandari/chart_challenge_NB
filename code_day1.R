
# 30 day chart challenge - Day 1 ------------------------------------------
# A fun graph  to start with the challenege

# What my dog does in a day


#libraries
library(ggplot2)

data <- read.csv("day1.csv")

head(data)
#lets calculate the percentage 

data <- data %>% 
        mutate(Percentage = Hours/sum(Hours)*100)
               
#make an interactive pie chart=============================================================

library(highcharter) 
options(highcharter.theme = hc_theme_smpl(tooltip = list(valueDecimals = 2),backgroundColor = "#15C0DE"))

hc <- data %>%
  hchart("pie", hcaes(x = Activity, y = Percentage),
    name = "Activities") %>% 
  hc_title(text = "What my dog does in a day",align = "center") %>% 
  hc_legend(align = "left",
            verticalAlign = "top",
            layout = "vertical",
            x = 0,
            y = 100) %>% 
  hc_credits(enabled = TRUE, text = c("Source: Selfmade database Design : @netra1128 #30DayChartChallenge"),
                               style = list(fontSize = "14px"))
hc

library(htmlwidgets)

saveWidget(hc, file=paste0( getwd(), "/day1.html"),selfcontained=TRUE,
           knitrOptions=list())

#to save the chart as .png use the export function in the viewer
