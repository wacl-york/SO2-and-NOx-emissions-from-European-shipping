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


dm <- read.csv("G:/My Drive/ACRUISE/Stuarts_integration/ACRUISE-1_integration/ACRUISE-1_integration_uncert.csv",
               stringsAsFactors = F,
               header=T)

dm <- dm %>% select(-c("X"))

dm$SFC <- dm$SFC*100
dm$Relative_unc <- dm$Relative_unc*100
dm$Absolute_unc <- dm$Absolute_unc*100



#dm$SFC <- label_percent()(dm$SFC)


dm$newx = str_wrap(dm$Ship, width = 12)
dm$pass_disc = as.factor(dm$Sea)



ggplot(data=dm)+
  geom_errorbar(aes(x=newx,
                    y=SFC,
                    #colour=Ship,
                    ymin=SFC-Absolute_unc, 
                    ymax=SFC+Absolute_unc),
                alpha=0.7,
                width=.2,
                position=position_dodge(0.05))+
  geom_point(aes(x=newx, 
                 y=SFC
                 ,colour=pass_disc
  ),
  #shape=21,
  #stroke=2,
  size=4) +
  #facet_wrap(~Flight, scales = "free")+
  theme_bw()+
  theme(text = element_text(size=13), 
        #legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
  )+
  viridis::scale_fill_viridis(option="viridis", discrete=T) +
  viridis::scale_colour_viridis(option="viridis", discrete=T) +
  labs(x= "Flight", y="SFC (%)", colour="Sea")#+
#guides(colour="none")















