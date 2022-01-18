# ACRUISE SUMMARY PLOTS #

################################################################################
### Loading packages ###

library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate) 
library(viridis)
library(stringr)
library(forcats)

################################################################################

#histograms

#gross tonnage
ggplot(dm, aes(Gross.tonnage/1000))+
    geom_bar(fill="#440154FF", colour="black")+
    scale_x_binned(n.breaks = 9)+
    labs(x="Gross tonnage (thousands)", y="Ship count", title = "ACRUISE-2 ship size distribution")+
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, colour = "white"),  text = element_text(size=14, colour="white"), axis.text = element_text(colour = "white"))


#year built
ggplot(dm, aes(Year))+
  geom_bar(fill="#95D840FF", colour="black")+
  scale_x_binned(n.breaks = 8)+
  labs(x="Year built", y="Ship count", title = "ACRUISE-2 ship age distribution")+
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, colour = "white"),  text = element_text(size=14, colour="white"), axis.text = element_text(colour = "white"))


#column charts

#type
st <- dm %>%
  dplyr::count(Type, sort=TRUE) %>%
  dplyr::mutate(Type=forcats::fct_rev(forcats::fct_inorder(Type)))

ggplot(st, aes(x=n,y=Type))+
  geom_col(fill="#fde725ff", colour="black")+
  #scale_x_binned(n.breaks = 8)+
  labs(x="Ship count", y="", title = "ACRUISE-2 ship type distribution")+
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, colour = "white"),  text = element_text(size=16, colour="white"), axis.text = element_text(colour = "white"))

