## day 8 
## animal distributions
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyverse)


### world seafood production
data <- read.csv("capture-and-aquaculture-production.csv")
data <- data %>% filter(data$Entity == "World")
data_aq <- data %>% group_by(Year, Entity) %>% 
  summarise("Aquaculture production" = round(Aquaculture.production..metric.tons./1000000))
data_cap <-  data %>% group_by(Year, Entity) %>% 
  summarise( "Capture fisheries " = round(Capture.fisheries.production..metric.tons./1000000))


# area chart
fig <- plot_ly(x = ~data_aq$Year, y = ~data_aq$`Aquaculture production`, type = 'scatter', mode = 'lines', name = 'Aquaculture production', fill = 'tozeroy')
fig <- fig %>% add_trace(x = ~data_cap$Year, y = ~data_cap$`Capture fisheries `, name = 'Capture fisheries', fill = 'tozeroy')
fig <- fig %>% layout(title = 'Caught in the wild? 
                    Aquaculture production has now surpassed wild catch.
                    While global wild fish catch has not increased since the early 1990s 
                     and instead remained relatively constant at around 90 to 95 million tonnes per year. 
                     On the other hand Fish farming is growing very rapidly,
                    from 1960 until 2015 it has increased 50-fold to over 100 million per year.',
                      xaxis = list(title = "",
                                   showgrid = FALSE),
                      yaxis = list(title = "Million tons",
                                   showgrid = FALSE),
                      annotations = list(x = 1, y = 5, text = "Source: Our World in Data, Design:@netra1128, #30DayChartChallenge", 
showarrow = F, xref='paper', yref='paper', 
xanchor='centre', yanchor='auto', xshift=0, yshift=5,
font=list(size=15, color="black")))

fig

