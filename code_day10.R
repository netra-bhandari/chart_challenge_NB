###day10 abstract

library(ggplot2)
library(dplyr)
library(tidyverse)
library(extrafont)
library(gtrendsR) # for getting youtube trends 
library(ggtext)
library(lubridate)
library(scico) #getting new pallete
library(here)

#search the term for abstract art
search <- c("Marbling","Geometric Art", "Paint Splatter")

#select 5 year trends 
trends <- gtrends(keyword = search,
                  time = "today+5-y", gprop = "youtube")

#select the date and number of hits per search term to visualise later

trends <- trends %>%
  .$interest_over_time %>%
  mutate_at("hits", ~ifelse(. == "<1", 0.5, .)) %>% 
  mutate_at("hits", ~as.numeric(.)) %>%
  select(date, hits, keyword)

#ploting segments over time to see trend

a <-ggplot(trends, aes(x = date, xend = date, y = 0, yend = 1)) +
  geom_segment(aes(colour = hits), size = 2) +
  theme_void() +
  facet_wrap(~keyword)+
  scale_colour_scico(palette = 'lajolla', direction = -1)+
  theme(panel.background = element_blank(),
        plot.background = element_rect(fill = "black"),
    plot.title = element_markdown(size = 32, family= "Garamond", color = "white", hjust = 0.5),
        plot.caption = element_text(size = 22, family = "Garamond",color = "white"),
        strip.text = element_text(size = 12),
        legend.position = "right",
    strip.text.x = element_text(color = "white", size = 20,margin = margin(2,0,2,0, "cm"),family= "Garamond"),
    aspect.ratio = 1,
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.switch.pad.grid = unit(0.4, "in"),
    strip.text.y = element_text(color = "white", size = 20,family= "Garamond"),
    legend.text = element_text(color = "white"), 
    plot.subtitle = element_text(color = "white", hjust = 0.5, size = 22,family= "Garamond")
    )+
  labs(title = "Abstract art type trends on Youtube",
       subtitle = "One can speak poetry just by arranging colors well - Van Gogh",
       caption = "Source:Google trends| @netra1128| #30DayChartChallenge")
  

ggsave(a, filename = "a.png", width = 10, height = 8)
