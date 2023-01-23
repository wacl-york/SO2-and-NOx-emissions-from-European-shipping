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



##### ACRUISE-2






















#### ACRUISE-1
dm <- read.csv("G:/My Drive/ACRUISE/Stuarts_integration/ACRUISE-1_integration/ACRUISE-1_integration_uncert.csv",
               stringsAsFactors = F,
               header=T)

#dm <- dm %>% select(-c("X"))

dm$SFC <- dm$SFC*100
dm$Relative.unc <- dm$Relative.unc*100
dm$Absolute.unc <- dm$Absolute.unc*100



#dm$SFC <- label_percent()(dm$SFC)


dm$shipw = str_wrap(dm$Ship, width = 12)

dm$typef = as.factor(dm$Type)
dm$typef = str_wrap(dm$typef, width = 12)

dm$seaf = as.factor(dm$Sea)



ggplot(data=dm)+
  geom_point(aes(x=typef,
                 y=SFC,
                 colour=Tonnage),
             size=4)+
  geom_errorbar(aes(x=typef,
                    y=SFC,
                    ymin=SFC-Absolute.unc,
                    ymax=SFC+Absolute.unc),
                width=0.1,
                position = position_dodge(0.05))+
  #facet_wrap(~Flight, scales = "free")+
  theme_bw()+
  theme(text = element_text(size=13), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  viridis::scale_colour_viridis(option="viridis", discrete=F) +
  labs(x= "Type", y="SFC (%)", colour="Tonnage")#+
  #guides(colour="none")



ggplot(data=dm)+
  geom_point(aes(x=Year,
                 y=SFC),
  size=4,
  colour="grey")+
  geom_errorbar(aes(x=Year,
                    y=SFC,
                    ymin=SFC-Absolute.unc,
                    ymax=SFC+Absolute.unc),
                width=0.3,
                position = position_dodge(0.05))+
  theme_bw()+
  theme(text = element_text(size=13), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  viridis::scale_colour_viridis(option="viridis", discrete=T) +
  labs(x= "Year", y="SFC (%)")











