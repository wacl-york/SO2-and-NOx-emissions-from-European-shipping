library(dplyr)
library(ggmap)
library(ggplot2)
library(tidyverse)
library(lubridate) 
library(viridis)
library(data.table)
library(openair)
library(reshape2)



### Merge ###

#set working directory
setwd("G:/My Drive/ACRUISE/ACRUISE2/SWAS_ACRUISE2/swas_CO2_CH4")

file_list <- list.files("./", pattern = "gga")
file <- file_list[3]

ugga <- read.table(file, header=T,sep=",") 
ugga$Time <- dmy_hms(ugga$Time)

ugga2 <- ugga %>% filter(between(Time, 
                                 ymd_hms("2022-02-16 09:50:00"),
                                 ymd_hms("2022-02-16 16:40:00")))


dt.df <- melt(ugga2, measure.vars = c("CO2_ppm", "CH4_ppm","H2O_ppm"))

ggplot(dt.df, aes(x = Time, y = value)) +
  geom_line(aes(color = variable),size=1) +
  facet_grid(variable ~ ., scales = "free_y") +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none", axis.title.x=element_blank(), axis.title.y=element_blank())

file3 <- ugga2







