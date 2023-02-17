library(tidyverse)
library(gridtext)
library(openair)
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
dm <- read.csv("G:/My Drive/ACRUISE/Stuarts_integration/ACRUISE-2_integration/ACRUISE-2_integration_just_anthem.csv",
               stringsAsFactors = F,
               header=T)
#dm <- dm %>% select(-c("X"))


dm$SFC <- dm$SFC*100
dm$Relative.unc <- dm$Relative.unc*100
dm$Absolute.unc <- dm$Absolute.unc*100



dm$shipw = str_wrap(dm$Ship, width = 12)

dm$typef = as.factor(dm$Type)
dm$typef = str_wrap(dm$typef, width = 12)

dm$seaf = as.factor(dm$Sea)

dm$limit <- 0.5
dm$limit[dm$Sea == "EC"] <- 0.1


#dm$date <- dmy_hms(dm$date) 

#dm$date <- dm$date - hours(1)


#SHIP AREA LIMIT
ggplot(data=dm)+
  geom_hline(aes(yintercept = limit))+
  geom_point(aes(x=Ship,
                 y=SFC,
                 colour=seaf),
             size=4)+
  geom_errorbar(aes(x=Ship,
                    y=SFC,
                    ymin=SFC-Absolute.unc,
                    ymax=SFC*1.06+Absolute.unc),
                width=0.1,
                position = position_dodge(0.05))+
  facet_grid(~Sea, scales = "free", space="free_x")+
  theme_bw()+
  theme(text = element_text(size=14),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  viridis::scale_colour_viridis(option="viridis", discrete=T) +
  labs(x= "Ship", y="SFC (%)", colour="Sea")+
  guides(colour="none")



#multiple encounters
dm %>%
  filter(revisit == "yes") %>%
  ggplot()+
  geom_hline(aes(yintercept = limit))+
  geom_point(aes(x=Flight,
                 y=SFC,
                 colour=seaf),
             size=4)+
  geom_errorbar(aes(x=Flight,
                    y=SFC,
                    ymin=SFC-Absolute.unc,
                    ymax=SFC*1.06+Absolute.unc),
                width=0.1,
                position = position_dodge(0.05),
                colour="grey")+
  facet_wrap(~shipw, scales = "free_x", ncol=5)+
  theme_bw()+
  theme(text = element_text(size=14),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_color_manual(values = c("EC" = "#21918c",
                                "BB"="#440154",
                                "SW"="#fde725",
                                "SW2"="#a0da39"))+
  labs(x= "Flight", y="SFC (%)", colour="Sea")#+
#guides(colour="none")


#multiple peaks
dm %>%
  filter(revisit == "no" | Ship == "Anthem of the Seas") %>%
  filter(Ship != "") %>%
  group_by(Ship) %>%
  filter(n()>1) %>%
  ggplot()+
  geom_hline(aes(yintercept = limit))+
  geom_point(aes(x=Ship,
                 y=SFC,
                 colour=seaf),
             size=4)+
  geom_errorbar(aes(x=Ship,
                    y=SFC,
                    ymin=SFC-Absolute.unc,
                    ymax=SFC*1.06+Absolute.unc),
                width=0.1,
                position = position_dodge(0.05),
                colour="grey")+
  facet_grid(~Flight,
             scales = "free_x",
             space = "free_x")+
  theme_bw()+
  theme(text = element_text(size=16),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_color_manual(values = c("EC" = "#21918c",
                                "BB"="#440154",
                                "SW"="#fde725"))+
  labs(x= "Ship", y="SFC (%)", colour="Sea")#+
#guides(colour="none")



#scrubber year
dm %>%
  filter(Scrubber != "") %>%
  ggplot()+
  geom_point(aes(x=Year,
                 y=SFC,
                 colour=Scrubber),
             size=4)+
  geom_errorbar(aes(x=Year,
                    y=SFC,
                    ymin=SFC-Absolute.unc,
                    ymax=SFC*1.06+Absolute.unc),
                width=0.1,
                position = position_dodge(0.05))+
  #facet_wrap(~Flight, scales = "free")+
  theme_bw()+
  theme(text = element_text(size=14),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  viridis::scale_colour_viridis(option="viridis", discrete=T) +
  labs(x= "Year", y="SFC (%)", colour="Scrubber")#+
#guides(colour="none")


#type tonnage
dm %>%
  ggplot()+
  geom_point(aes(x=typef,
                 y=SFC,
                 colour=Tonnage),
             size=4)+
  geom_errorbar(aes(x=typef,
                    y=SFC,
                    ymin=SFC-Absolute.unc,
                    ymax=SFC*1.06+Absolute.unc),
                width=0.1,
                position = position_dodge(0.05))+
  #facet_wrap(~Flight, scales = "free")+
  theme_bw()+
  theme(text = element_text(size=14),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  viridis::scale_colour_viridis(option="viridis", discrete=F) +
  labs(x= "Type", y="SFC (%)", colour="Tonnage")#+
#guides(colour="none")





#merge with lat lon
latlon <- readRDS("G:/My Drive/ACRUISE/ACRUISE2/data_raw/core_for_stats/ACRUISE-2_merged_r0.RDS")
tz(latlon$date) <- "UTC"
dm_map <- left_join(dm, latlon, by="date", all.x=F)

saveRDS(dm_map, "G:/My Drive/ACRUISE/Stuarts_integration/ACRUISE-2_integration/acruise2_sfcs_latlon.RDS")
write.csv(dm_map, "G:/My Drive/ACRUISE/Stuarts_integration/ACRUISE-2_integration/acruise2_sfcs_latlon_2.csv")








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

dm$limit <- 3.5
dm$limit[dm$Sea == "EC"] <- 0.1




#scrubber year
dm %>%
  filter(Scrubber != "") %>%
  ggplot()+
  geom_point(aes(x=Year,
                 y=SFC,
                 colour=Scrubber),
             size=4)+
  geom_errorbar(aes(x=Year,
                    y=SFC,
                    ymin=SFC-Absolute.unc,
                    ymax=SFC*1.06+Absolute.unc),
                width=0.1,
                position = position_dodge(0.05))+
  #facet_wrap(~Flight, scales = "free")+
  theme_bw()+
  theme(text = element_text(size=14),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  viridis::scale_colour_viridis(option="viridis", discrete=T) +
  labs(x= "Year", y="SFC (%)", colour="Scrubber")#+
#guides(colour="none")


#type tonnage
dm %>%
  ggplot()+
  geom_point(aes(x=typef,
                 y=SFC,
                 colour=Tonnage),
             size=4)+
  geom_errorbar(aes(x=typef,
                    y=SFC,
                    ymin=SFC-Absolute.unc,
                    ymax=SFC*1.06+Absolute.unc),
                width=0.1,
                position = position_dodge(0.05))+
  #facet_wrap(~Flight, scales = "free")+
  theme_bw()+
  theme(text = element_text(size=14),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  viridis::scale_colour_viridis(option="viridis", discrete=F) +
  labs(x= "Type", y="SFC (%)", colour="Tonnage")#+
#guides(colour="none")



#SHIP AREA LIMIT
ggplot(data=dm)+
  geom_hline(aes(yintercept = limit))+
  geom_point(aes(x=Ship,
                 y=SFC,
                 colour=seaf),
             size=4)+
  geom_errorbar(aes(x=Ship,
                    y=SFC,
                    ymin=SFC-Absolute.unc,
                    ymax=SFC*1.06+Absolute.unc),
                width=0.1,
                position = position_dodge(0.05))+
  facet_grid(~Sea, scales = "free", space="free_x")+
  theme_bw()+
  theme(text = element_text(size=12),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  viridis::scale_colour_viridis(option="viridis", discrete=T) +
  labs(x= "Ship", y="SFC (%)", colour="Sea")+
  guides(colour="none")






############################


# YORK REGRESSION

#format data
york_data <- tibble(
  X = dm$so2_ph,
  Y = dm$SFC,
  Xstd = dm$so2_ph_unc,
  Ystd = dm$Absolute.unc) %>%
  na.omit()

#york regression magic
results <- openair:::YorkFit(york_data)
print(results)



ggplot(york_data, aes(x = X, y = Y)) +
  geom_point() +
  geom_errorbarh(aes(xmax = X + Xstd, xmin = X - Xstd)) +
  geom_errorbar(aes(ymax = Y + Ystd, ymin = Y - Ystd)) +
  geom_abline(slope = results$Slope, intercept = results$Intercept,
              lty = 5, colour = "red") +
  geom_smooth(method = "lm", lty = 3, colour = "blue") 


# nice plot

ggplot(data=york_data, aes(x = X, y = Y))+
  geom_abline(slope = results$Slope, 
              intercept = results$Intercept, 
              colour = "#a0da39",
              size=3) +
  geom_point(size=4,
             colour="grey")+
  geom_errorbarh(aes(xmax = X + Xstd, xmin = X - Xstd)) +
  geom_errorbar(aes(ymax = Y + Ystd, ymin = Y - Ystd)) + 
  annotate("text", x = 4, y = 1.15, label = "y = 0.1150x - 0.2040", size=5)+
  annotate("text", x = 4, y = 1.09, label = "slope sd = 0.0174",size=5)+
  theme_bw()+
  theme(text = element_text(size=14),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  viridis::scale_colour_viridis(option="viridis", discrete=T) +
  labs(x=bquote(''~SO[2]~peak~height~(ppb)*''), y="SFC (%)")

# normal boring regression


ggplot(data = dm,
       aes(x=so2_ph, 
           y=SFC))+
  geom_point(size=2, 
             shape=21)+
  stat_poly_line(colour="#440154", 
                 size=1) + 
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label),
                                 sep = "*\", \"*"))) +
  theme_bw()+
  labs(x="CO2 peak height", 
       y=" SFC (%)") +
  theme(plot.title = element_text(hjust = 0.5),  
        text = element_text(size=12)) #14 large, 12 standard







####################################################


# so2/co2 against lon - read data from integration script
dm2 %>%
  filter(HGT_RADR < 500) %>%
  filter(flight == 263) %>%
  ggplot()+
  geom_point(aes(x=LON_GIN,
                 y=CO2_ppm))+
  theme_bw()+
  theme(text = element_text(size=14),
        axis.text.x = element_text(vjust = 0.5, hjust=1))+
  labs(y=bquote(''~CO[2]~(ppb)*''), x="Longitude")












































#######################

#colour variable
ggplot(data=dm)+
  geom_point(aes(x=typef,
                 y=SFC,
                 colour=Tonnage),
             size=4)+
  geom_errorbar(aes(x=typef,
                    y=SFC,
                    ymin=SFC-Absolute.unc,
                    ymax=SFC*1.06+Absolute.unc),
                width=0.1,
                position = position_dodge(0.05))+
  #facet_wrap(~Flight, scales = "free")+
  theme_bw()+
  theme(text = element_text(size=13),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  viridis::scale_colour_viridis(option="viridis", discrete=F) +
  labs(x= "Type", y="SFC (%)", colour="Tonnage")#+
#guides(colour="none")


# no colour variable
ggplot(data=dm)+
  geom_point(aes(x=Year,
                 y=SFC),
             size=4,
             colour="grey")+
  geom_errorbar(aes(x=Year,
                    y=SFC,
                    ymin=SFC-Absolute.unc,
                    ymax=SFC*1.06+Absolute.unc),
                width=0.3,
                position = position_dodge(0.05))+
  theme_bw()+
  theme(text = element_text(size=13),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  viridis::scale_colour_viridis(option="viridis", discrete=T) +
  labs(x= "Year", y="SFC (%)")
