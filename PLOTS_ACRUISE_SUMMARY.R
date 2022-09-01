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

# read in file
dm <- read.csv("G:/My Drive/ACRUISE/all_ships_basic_data.csv", header=T, stringsAsFactors = F)


#histograms

#gross tonnage
dm %>%
  filter(Campaign != 1) %>%
  ggplot()+
    geom_bar(aes(Gross.tonnage/1000),
             fill="#440154FF", 
             colour="black")+
    scale_x_binned(n.breaks = 10)+
    labs(x="Gross tonnage (thousands)", y="Ship count")+
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, colour = "black"),  text = element_text(size=12, colour="black"), axis.text = element_text(colour = "black"))


#year built
dm %>%
  filter(Campaign != 1) %>%
  ggplot()+
  geom_bar(aes(Year.built),
           fill="#95D840FF", 
           colour="black")+
  scale_x_binned(n.breaks = 8)+
  labs(x="Year built", y="Ship count")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, colour = "black"),  text = element_text(size=12, colour="black"), axis.text = element_text(colour = "black"))

#year built
dm %>%
  filter(Campaign != 1) %>%
  ggplot()+
  geom_bar(aes(Scrubber),
           fill="#3b528b", 
           colour="black")+
  labs(x="Scrubber", y="Ship count")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, colour = "black"),  text = element_text(size=12, colour="black"), axis.text = element_text(colour = "black"))




#column charts

#type
st <- dm %>%
  filter(Campaign != 1) %>%
  dplyr::count(Type, sort=TRUE) %>%
  dplyr::mutate(Type=forcats::fct_rev(forcats::fct_inorder(Type)))

ggplot(st, aes(x=n,y=Type))+
  geom_col(fill="#fde725ff", 
           colour="black")+
  labs(x="Ship count", y="")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, colour = "black"),  text = element_text(size=11, colour="black"), axis.text = element_text(colour = "black"))

