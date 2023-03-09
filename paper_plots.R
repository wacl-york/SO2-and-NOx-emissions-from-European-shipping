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
library(praise)




##### ACRUISE-3
dm <- read.csv("G:/My Drive/ACRUISE/Stuarts_integration/ACRUISE-3_integration/ACRUISE-3_integration_uncert_freqs.csv",
               stringsAsFactors = F,
               header=T)

dm <-  dm[1:38,]

dm$SFC <- dm$SFC*100
dm$Relative.unc <- dm$Relative.unc*100
dm$Absolute.unc <- dm$Absolute.unc*100



dm$shipw = str_wrap(dm$Ship, width = 12)

dm$typef = as.factor(dm$Type)
dm$typef = str_wrap(dm$typef, width = 12)

dm$seaf = as.factor(dm$Sea)

dm$limit <- 0.5
dm$limit[dm$Sea == "EC"] <- 0.1



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




#multiple peaks
dm %>%
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
  scale_color_manual(values = c("AO" = "#3b528b",
                                "BB"="#440154",
                                "SW"="#fde725"))+
  labs(x= "Ship", y="SFC (%)", colour="Sea")#+
#guides(colour="none")



#scrubber year
dm %>%
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
latlon <- readRDS("G:/My Drive/ACRUISE/ACRUISE3/data/prelim_core/ACRUISE-3_merged_r0.RDS")

tz(latlon$date) <- "UTC"

dm$date <- dmy_hms(dm$co2_start)
dm$date <- dm$date - lubridate::hours(1)
dm_map <- left_join(dm, latlon, by="date", all.x=F)

saveRDS(dm_map, "G:/My Drive/ACRUISE/Stuarts_integration/ACRUISE-3_integration/acruise3_sfcs_latlon_1Hz.RDS")
write.csv(dm_map, "G:/My Drive/ACRUISE/Stuarts_integration/ACRUISE-3_integration/A3_sfcs_latlon_1Hz.csv")




#frequency comparison

dm$num <- 1:38

 

dm %>%
  filter(!is.na(SFC_5hz))%>%
  ggplot()+
  geom_point(aes(x=num,
                 y=SFC_1hz*100),
             size=4,
             colour="maroon",
             alpha=0.5)+
  geom_errorbar(aes(x=num,
                    y=SFC_1hz*100,
                    ymin=SFC_1hz*100-SFC_1hz_abs*100,
                    ymax=SFC_1hz*100*1.06+SFC_1hz_abs*100),
                width=0.1,
                position = position_dodge(0.05))+
  geom_point(aes(x=num,
                 y=SFC_5hz*100),
             size=4,
             colour="blue",
             alpha=0.5)+
  geom_errorbar(aes(x=num,
                    y=SFC_5hz*100,
                    ymin=SFC_5hz*100-SFC_5hz_abs*100,
                    ymax=SFC_5hz*100*1.06+SFC_5hz_abs*100),
                width=0.1,
                position = position_dodge(0.05))+
  #facet_wrap(~Flight, scales = "free")+
  theme_bw()+
  theme(text = element_text(size=14),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  viridis::scale_colour_viridis(option="viridis", discrete=F) +
  labs(x= "Number ", y="SFC (%)")#+
#guides(colour="none")


# 1hz teco vs 5hz lif
dm %>%
  filter(!is.na(SFC_5hz))%>%
  ggplot(aes(x=SFC_1hz*100,
             y=SFC_5hz*100))+
  stat_poly_line(colour="#3b528b", 
                 size=1) + 
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label),
                                 sep = "*\", \"*"))) +
  geom_point(size=4,
             colour="#fde725",
             alpha=0.5)+
  geom_errorbar(aes(xmin=SFC_1hz*100-SFC_1hz_abs*100,
                    xmax=SFC_1hz*100*1.06+SFC_1hz_abs*100),
                width=0.05)+
  geom_errorbar(aes(ymin=SFC_5hz*100-SFC_5hz_abs*100,
                    ymax=SFC_5hz*100*1.06+SFC_5hz_abs*100),
                width=0.05)+
  #facet_wrap(~Flight, scales = "free")+
  theme_bw()+
  theme(text = element_text(size=14),
        #axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
        )+
  viridis::scale_colour_viridis(option="viridis", discrete=F) +
  labs(x= "SFC 1Hz (%) ", y="SFC 5Hz (%)")#+
#guides(colour="none")



# 1 hz teco vs lif
dm %>%
  filter(!is.na(SFC_5hz))%>%
  ggplot(aes(x=SFC_1hz*100,
             y=SFC_1hz_lif*100))+
  stat_poly_line(colour="#3b528b", 
                 size=1) + 
  stat_poly_eq(aes(label = paste(after_stat(eq.label),
                                 after_stat(rr.label),
                                 sep = "*\", \"*"))) +
  geom_point(size=4,
             colour="#fde725",
             alpha=0.5)+
  geom_errorbar(aes(xmin=SFC_1hz*100-SFC_1hz_abs*100,
                    xmax=SFC_1hz*100*1.06+SFC_1hz_abs*100),
                width=0.05)+
  geom_errorbar(aes(ymin=SFC_1hz_lif*100-SFC_1hz_lif_abs*100,
                    ymax=SFC_1hz_lif*100*1.06+SFC_1hz_lif_abs*100),
                width=0.05)+
  #facet_wrap(~Flight, scales = "free")+
  theme_bw()+
  theme(text = element_text(size=14),
        #axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
  )+
  viridis::scale_colour_viridis(option="viridis", discrete=F) +
  labs(x= "SFC 1Hz TECO (%) ", y="SFC 1Hz LIF (%)")#+
#guides(colour="none")


# same but with york regression

#format data
york_data <- tibble(
  X = dm$SFC_1hz*100,
  Y = dm$SFC_1hz_lif*100,
  Xstd = dm$SFC_1hz_abs*100,
  Ystd = dm$SFC_1hz_lif_abs*100) %>%
  na.omit()

#york regression magic
results <- openair:::YorkFit(york_data)
print(results)



ggplot(york_data, aes(x = X, y = Y)) +
  geom_point(colour="grey") +
  geom_errorbarh(aes(xmax = X + Xstd, xmin = X - Xstd)) +
  geom_errorbar(aes(ymax = Y + Ystd, ymin = Y - Ystd)) +
  geom_smooth(method = "lm", lty = 3, colour = "blue") +
  annotate("text", x = 1, y = 2, label = eq_lab, size=5)+
  annotate("text", x = 1, y = 1.8, label = sd_lab,size=5)+
  theme_bw()+
  theme(text = element_text(size=14)  )+
  labs(x= "SFC 1Hz TECO (%) ", y="SFC 1Hz LIF (%)")#+





eq_lab <- paste0("y = ", round(results$Slope, digits=2), "x ", round(results$Intercept, digits=3))

sd_lab <- paste0("slope sd = ", round(results$Slope_error, digits=3))











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

def.pol.cart <- function(cart){
  az <- (atan2(cart[,2],cart[,1])*180/pi)+180
  return(az)
}

dm3 <-  dm %>%
  mutate(wdir = def.pol.cart(matrix(c(V_C,U_C),ncol = 2)))

dm <- dm3 %>%
  filter(HGT_RADR < 500) %>%
  openair::timeAverage(avg.time = "min")
#filter(flight == 263) %>%

dm$wind_flag <- 1

dm$wind_flag[dm$wdir >315 | dm$wdir < 45] <- "N"
dm$wind_flag[dm$wdir >= 45 & dm$wdir <= 135] <- "E"
dm$wind_flag[dm$wdir >135 & dm$wdir < 225] <- "S"
dm$wind_flag[dm$wdir >= 225 & dm$wdir <= 315] <- "W"




  
dm %>%  
  filter(co2<416 & co2>402) %>%
  ggplot()+
  geom_point(aes(x=LON_GIN,
                 y=co2,
                 colour=wind_flag),
             size=4,
             alpha=0.8)+
  theme_bw()+
  scale_color_manual(values = c("E" = "#21918c",
                                "W" = "#5ec962",
                                "N"="#482576",
                                "S"="#fc8961"))+
  theme(text = element_text(size=14),
        axis.text.x = element_text(vjust = 0.5, hjust=1))+
  labs(y=bquote(''~CO[2]~(ppm)*''), x="Longitude", colour="Wind")


dm %>% 
  #filter(SO2_TECO < 10)%>%
  ggplot()+
  geom_point(aes(x=LON_GIN,
                 y=so2,
                 colour=wind_flag),
             size=4,
             alpha=0.8)+
  theme_bw()+
  scale_color_manual(values = c("E" = "#21918c",
                                "W" = "#5ec962",
                                "N"="#482576",
                                "S"="#fc8961"))+
  theme(text = element_text(size=14),
        axis.text.x = element_text(vjust = 0.5, hjust=1))+
  labs(y=bquote(''~SO[2]~(ppb)*''), x="Longitude", colour="Wind")


#



#a3 merge

dm2 <- bind_rows(c284_core_data,c285_core_data,c286_core_data,c287_core_data)
tz(dm2$date)

fgga <- bind_rows(c284_fgga_10hz_data, c285_fgga_10hz_data, c286_fgga_10hz_data, c287_fgga_10hz_data)
fgga$date <- as.POSIXct(fgga$date)
tz(fgga$date) <- "UTC"
fgga <- openair::timeAverage(fgga, avg.time = "sec")

alll <- left_join(dm2,fgga, by="date")

saveRDS(alll, "G:/My Drive/ACRUISE/ACRUISE3/data/A3_all_merge.RDS")


#a1 prep

dm <- dm %>% rename(V_C = v,
              U_C= u,
              W_C = w,
              HGT_RADR = hgt_radr,
              LON_GIN = lon,
              LAT_GIN = lat)































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
