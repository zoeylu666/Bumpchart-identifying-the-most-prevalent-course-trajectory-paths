
library(wesanderson)
library(tidyverse)
library(ggbump)
library(dplyr)
library(plotly)
library(cowplot)

if(!require(pacman)) install.packages("pacman")
if(!require(ggbump)) devtools::install_github("davidsjoberg/ggbump")

pacman::p_load(padr, hablar, jsonlite, ggbump, 
               httr, xml2, lubridate, tidyverse)


dat <- read_csv("./R/DataViz1/Bumpcharts/bumpchart_fake6.csv")

dat$Year <- as.numeric(dat$Year)
is.numeric(dat$Year)
####Create a bump chart

math_path <- dat %>%
  ggplot(aes(Year, n, color = Order)) +
  geom_point(size =7)+
  geom_bump(size = 2, smooth = 8) +
  geom_text(data = dat  %>% filter(Year == min(Year)), aes(Year, n, label = n),size = 5, color = "brown", 
            hjust = 0.5, vjust = -1)+
  geom_text(data = dat  %>% filter(Year == max(Year)), aes(Year, n, label = n),size = 5, color = "brown", 
            hjust =-1.5, vjust =.5, nudge_x = 0.1)+
  geom_text(data = dat %>% filter(Year == min(Year)),
            aes(x= Year -.2, label = Order), size = 5, hjust = 1, vjust = -1,
            nudge_x = 0.1,  color = "brown") +
            #  nudge_y = .5,) 
  geom_text(data = dat %>% filter(Year == max(Year)),
            aes(x= Year + .2, label = Order), size = 5, hjust =1.2, vjust =.5, nudge_x = 0.1,
            nudge_y = .5, color = "brown") +
   theme_minimal()+
  theme(legend.position = "none")+
  scale_color_manual(values = wes_palette(n = 5, name = "Rushmore1"))

math_path_2 <- math_path +labs(title = "ABC High School Math Model Trajectory",
       x= "Academic Years",
       y="The Count (n)",
       caption= "From 2017 (G9) to 2020 (G12)",
       subtitle = "The Count of Courses By Rigor")+
       theme(
             plot.title = element_text( size = 18, face = "bold"),
              plot.subtitle = element_text(size = 15),
             plot.caption = element_text(hjust = 0.5, face = "italic")
  )


math_path_2

math_path <- geom_text(data = math_path, aes(x=Year, y = n, label = Order)) 
  # geom_label()+
  annotate("text", y = Order, yend=Order, label= "Order")+ 
  # scale_color_manual(values = wes_palette(n = 5, name = "Rushmore1"))



math_path

ggplotly(dupont_math_path)

