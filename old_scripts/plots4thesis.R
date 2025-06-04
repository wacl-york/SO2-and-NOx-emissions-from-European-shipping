library(tidyverse)
library(gridtext)
library(dplyr)
library(openair)
library(tidyverse)
library(lubridate) 
library(viridis)
library(ggpmisc)
library(reshape2)
library(grid)
library(gridExtra)
library(rgeos)
library(ggnewscale)
library(measurements)
library(ggplot2)
library(scales)


dm <- read.csv("G:/My Drive/ACRUISE/Stuarts_integration/ACRUISE-2_integration/ACRUISE-2_integration_prelim_with_zeros.csv",
         stringsAsFactors = F,
         header=T)

dm$Mean.SFC <- dm$Mean.SFC*100
dm$Sd <- dm$Sd*100
dm$Uncertainty <- dm$Uncertainty*100
dm$Min.SFC <- dm$Min.SFC*100
dm$Max.SFC <- dm$Max.SFC*100



#dm$SFC <- label_percent()(dm$SFC)


dm$newx = str_wrap(dm$Ship, width = 12)
dm$pass_disc = as.factor(dm$Passes)



ggplot(data=dm)+
  geom_errorbar(aes(x=newx,
                      y=Mean.SFC,
                      #colour=Ship,
                      ymin=Min.SFC, 
                      ymax=Max.SFC),
                alpha=0.7,
                width=.2,
                position=position_dodge(0.05))+
  geom_point(aes(x=newx, 
                 y=Mean.SFC,
                 colour=pass_disc
                 ),
                 #shape=21,
                 #stroke=2,
                 size=4) +
  facet_wrap(~Flight, scales = "free")+
  theme_bw()+
  theme(text = element_text(size=13), 
        #legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
        )+
  viridis::scale_fill_viridis(option="magma", discrete=T) +
  viridis::scale_colour_viridis(option="magma", discrete=T) +
  labs(x= "Flight", y="SFC (%)", colour="Passes")#+
  #guides(colour="none")




#########

#origin vs arrrr

dm <- read.csv("G:/My Drive/ACRUISE/Stuarts_integration/ACRUISE-1_integration/origin_comparison/comparison.csv", stringsAsFactors = F)

dm <- dm[-c(7:16)]


ggplot(data=dm,
       aes(x=SO2_origin/CO2_origin, 
           y=SO2_r/CO2_r))+
  stat_poly_line(colour="cornflowerblue", 
                 size=1) + #2 large, 1 standard
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label),
                                 sep = "*\", \"*"))) +
  geom_point(size=4,
             shape=21) +
  theme_bw()+
  theme(text = element_text(size=13), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
  )+
  labs(x= "Origin Pro", y="R")

