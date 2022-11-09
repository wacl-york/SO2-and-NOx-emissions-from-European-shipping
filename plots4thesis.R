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





