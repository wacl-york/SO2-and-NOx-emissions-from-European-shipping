library(dplyr)
library(ggmap)
library(ggplot2)
library(tidyverse)
library(lubridate) 
library(viridis)
library(data.table)
library(openair)
library(reshape2)


# -------------------------------------------------------------------------
### Merge ###

setwd("G:/My Drive/ACRUISE/ACRUISE2/SWAS_ACRUISE2/swas_CO2_CH4")

source("G:/My Drive/GitHub/ACRUISE/ugga_swas_avg.R")

file_list <- list.files("./", pattern = "gga")
file <- file_list[3]

ugga <- read.table(file, header=T,sep=",") 
ugga$Time <- dmy_hms(ugga$Time)

ugga2 <- ugga %>% filter(between(Time, 
                                 ymd_hms("2022-02-16 09:50:00"),
                                 ymd_hms("2022-02-16 16:40:00")))

# -------------------------------------------------------------------------

file1Avg = file1 %>% 
  tidy_ugga_canisters()


#file2
#caseNum = c(rep(5,16), rep(105,8), rep(6,16), rep(2,15), rep(101,8), rep(102,5)),
#bottleNum = c(1:16,1:8,1:16,1:7,9:16,1:8,1:5)

#file3
#caseNum = c(rep(102,3), rep(3,16), rep(104,7), rep(7,14), rep(1,14), rep(4,16), rep(103,8)),
#bottleNum = c(6:8,1:16,1:4,6:8,1:14,1:8,11:16,1:16,1:8)


ggplot()+
  geom_line(data = file3, aes(x=Time, y=CO2_ppm))+
  geom_vline(data=canisterTimes, 
             aes(xintercept=date_start), 
             colour="darkgreen")+
  geom_vline(data=canisterTimes, 
             aes(xintercept=date_end),
             colour="firebrick")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "none", 
        axis.title.x=element_blank(), 
        axis.title.y=element_blank())


avgs1 <- data.frame(file1Avg$avgData)

write.csv(avgs1, "./avgs1.csv")
