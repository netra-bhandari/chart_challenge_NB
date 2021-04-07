library(tidyverse) 
library(imager) 
library(scales)
library(packcircles)
library(grid)
library(gridExtra)

## Please checkout https://chichacha.netlify.app/2018/12/22/bubble-packed-chart-with-r-using-packcircles-package/
## for more inspiration

## load images
im_sunflower <- load.image("download.png") 
im_pumpkin <- load.image("pumpkin.png")
im_coffee <- load.image("97-978198_green-coffee-beans-are-soaked-in-hot-water.png")
im_sugarcane <- load.image("sugarcane.jpg")
im_tea <- load.image("68-689066_green-tea-png-free-images-green-tea-leaf.png")


## plot to check background
plot(im_sunflower)
plot(im_pumpkin)
plot(im_coffee)
plot(im_sugarcane)
plot(im_tea)


#### change Image into Data Frame
im.df.colour_sunflower <- im_sunflower %>%
  as.data.frame(wide="c") %>% 
  rename(im_x=x,im_y=y) %>%
  mutate(hex=rgb(c.1,c.2,c.3))



im.df.colour_pump <- im_pumpkin %>%
  as.data.frame(wide="c") %>% 
  rename(im_x=x,im_y=y) %>%
  mutate(hex=rgb(c.1,c.2,c.3))

im.df.colour_coffee <- im_coffee %>%
  as.data.frame(wide="c") %>% 
  rename(im_x=x,im_y=y) %>%
  mutate(hex=rgb(c.1,c.2,c.3))

im.df.colour_sugar <- im_sugarcane %>%
  as.data.frame(wide="c") %>% 
  rename(im_x=x,im_y=y) %>%
  mutate(hex=rgb(c.1,c.2,c.3))

im.df.colour_tea <- im_tea %>%
  as.data.frame(wide="c") %>% 
  rename(im_x=x,im_y=y) %>%
  mutate(hex=rgb(c.1,c.2,c.3))


## Circular packaging of the images

pack_layout_sunflower <- circleProgressiveLayout(rbeta(2000,1,2), sizetype='area') %>% 
  ## Step 3 - I want to figure out what colour to use, so I want layout & image df to have same scaling. 
  mutate(im_x=floor(rescale(x,to=range(im.df.colour_sunflower$im_x))),  
         im_y=floor(rescale(y,to=range(im.df.colour_sunflower$im_y))),
         ## also generate id, so i can join the data frame easily later!
         id=row_number()) %>% 
  inner_join(im.df.colour_sunflower %>% select(im_x,im_y,hex), by=c("im_x","im_y"))


pack_layout_pump <- circleProgressiveLayout(rbeta(2000,1,2), sizetype='area') %>% 
  ## Step 3 - I want to figure out what colour to use, so I want layout & image df to have same scaling. 
  mutate(im_x=floor(rescale(x,to=range(im.df.colour_pump$im_x))),  
         im_y=floor(rescale(y,to=range(im.df.colour_pump$im_y))),
         ## also generate id, so i can join the data frame easily later!
         id=row_number()) %>% 
  inner_join(im.df.colour_pump %>% select(im_x,im_y,hex), by=c("im_x","im_y"))


pack_layout_coffee <- circleProgressiveLayout(rbeta(2000,1,2), sizetype='area') %>% 
  ## Step 3 - I want to figure out what colour to use, so I want layout & image df to have same scaling. 
  mutate(im_x=floor(rescale(x,to=range(im.df.colour_coffee$im_x))),  
         im_y=floor(rescale(y,to=range(im.df.colour_coffee$im_y))),
         ## also generate id, so i can join the data frame easily later!
         id=row_number()) %>% 
  inner_join(im.df.colour_coffee %>% select(im_x,im_y,hex), by=c("im_x","im_y"))

pack_layout_sugar <- circleProgressiveLayout(rbeta(2000,1,2), sizetype='area') %>% 
  ## Step 3 - I want to figure out what colour to use, so I want layout & image df to have same scaling. 
  mutate(im_x=floor(rescale(x,to=range(im.df.colour_sugar$im_x))),  
         im_y=floor(rescale(y,to=range(im.df.colour_sugar$im_y))),
         ## also generate id, so i can join the data frame easily later!
         id=row_number()) %>% 
  inner_join(im.df.colour_sugar %>% select(im_x,im_y,hex), by=c("im_x","im_y"))

pack_layout_tea <- circleProgressiveLayout(rbeta(2000,1,2), sizetype='area') %>% 
  ## Step 3 - I want to figure out what colour to use, so I want layout & image df to have same scaling. 
  mutate(im_x=floor(rescale(x,to=range(im.df.colour_tea$im_x))),  
         im_y=floor(rescale(y,to=range(im.df.colour_tea$im_y))),
         ## also generate id, so i can join the data frame easily later!
         id=row_number()) %>% 
  inner_join(im.df.colour_tea %>% select(im_x,im_y,hex), by=c("im_x","im_y"))


## circleLayoutVertices to plots circle using ggplot2

data_gg_sunflower <- circleLayoutVertices(pack_layout_sunflower) %>% 
  inner_join(pack_layout_sunflower %>% select(id,hex), by=c("id"))



data_gg_pump <- circleLayoutVertices(pack_layout_pump) %>% 
  inner_join(pack_layout_pump %>% select(id,hex), by=c("id"))

data_gg_coffee <- circleLayoutVertices(pack_layout_coffee) %>% 
  inner_join(pack_layout_coffee %>% select(id,hex), by=c("id"))

data_gg_sugar <- circleLayoutVertices(pack_layout_sugar) %>% 
  inner_join(pack_layout_sugar %>% select(id,hex), by=c("id"))

data_gg_tea <- circleLayoutVertices(pack_layout_tea) %>% 
  inner_join(pack_layout_tea %>% select(id,hex), by=c("id"))


## Step 5
sunflower <- data_gg_sunflower %>% 
  ggplot(aes(x=x,y=y,group=id)) +
  geom_polygon(aes(fill=hex)) +  
  scale_fill_identity() + 
  coord_equal() +
  scale_y_reverse() +  ## you need to reverse y-axis
  theme_void()

pumpkin <- data_gg_pump %>% 
  ggplot(aes(x=x,y=y,group=id)) +
  geom_polygon(aes(fill=hex)) +  
  scale_fill_identity() + 
  coord_equal() +
  scale_y_reverse() +  ## you need to reverse y-axis
  theme_void() 

coffee <- data_gg_coffee %>% 
  ggplot(aes(x=x,y=y,group=id)) +
  geom_polygon(aes(fill=hex)) +  
  scale_fill_identity() + 
  coord_equal() +
  scale_y_reverse() +  ## you need to reverse y-axis
  theme_void() 

sugarcane <- data_gg_sugar %>% 
  ggplot(aes(x=x,y=y,group=id)) +
  geom_polygon(aes(fill=hex)) +  
  scale_fill_identity() + 
  coord_equal() +
  scale_y_reverse() +  ## you need to reverse y-axis
  theme_void() 

tea <- data_gg_tea %>% 
  ggplot(aes(x=x,y=y,group=id)) +
  geom_polygon(aes(fill=hex)) +  
  scale_fill_identity() + 
  coord_equal() +
  scale_y_reverse() +  ## you need to reverse y-axis
  theme_void() 

## export images

grid.arrange(sunflower,
             pumpkin,
             tea,
             coffee,
             sugarcane, nrow = 1) #you can change the heights and widhts here

