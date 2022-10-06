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


dm <- read.csv("G:/My Drive/ACRUISE/Stuarts_integration/SFC_ACRUISE_2_preliminary.csv",
         stringsAsFactors = F,
         header=T)

dm$SFC <- dm$SFC*100


#dm$SFC <- label_percent()(dm$SFC)


ggplot(data=dm)+
  geom_point(aes(x=Number, 
                 y=SFC),
             shape=21,
             size=5) + 
  geom_errorbar(aes(x=Number,
                      y=SFC,
                      #colour=Ship,
                      ymin=SFC-SFC*0.49, 
                      ymax=SFC+SFC*0.49),
                alpha=0.7,
                width=.2,
                position=position_dodge(0.05))+
  theme_bw()+
  theme(text = element_text(size=14), legend.title = element_blank())+
  viridis::scale_fill_viridis(option="magma", discrete=T) +
  viridis::scale_colour_viridis(option="magma", discrete=T) +
  labs(x= "Flight", y="SFC (%)")+
  guides(colour="none")





