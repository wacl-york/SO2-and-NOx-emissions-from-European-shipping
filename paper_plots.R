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
library(plotrix)



##### ACRUISE-3
dm3 <- read.csv("G:/My Drive/ACRUISE/Stuarts_integration/ACRUISE-3_integration/ACRUISE-3_integration_uncert_1Hz.csv",
               stringsAsFactors = F,
               header=T)

dm3 <-  dm3[1:38,]

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




############ differences in areas

dm %>%
  #filter(!is.na(SFC_5hz))%>%
  ggplot()+
  geom_line(aes(x=num,
                 y=co2_10Hz_area,
                colour="10 Hz"),
             size=2,
             alpha=0.8)+
  geom_line(aes(x=num,
                 y=co2_5Hz_area,
                colour = "5 Hz"),
             size=2,
             alpha=0.8)+
  geom_line(aes(x=num,
                 y=co2_1Hz_area,
                colour="1 Hz"),
             size=2,
             alpha=0.8)+
  theme_bw()+
  scale_color_manual(name = "colours", values = c("10 Hz" = "#31688e", "5 Hz" = "#90d743", "1 Hz" = "#fde725")) +
  
  theme(text = element_text(size=14),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())+
  labs(y=bquote(''~CO[2]~area*''))



dm %>%
  #filter(!is.na(SFC_5hz))%>%
  ggplot()+
  geom_line(aes(x=num,
                y=so2_lif_area,
                colour = "5 Hz"),
            size=2,
            alpha=0.8)+
  geom_line(aes(x=num,
                y=so2_lif_1hz_area,
                colour="1 Hz LIF"),
            size=2,
            alpha=0.8)+
  geom_line(aes(x=num,
                y=so2_lif_1hz_area,
                colour="1 Hz TECO"),
            size=2,
            alpha=0.8)+
  theme_bw()+
  scale_color_manual(name = "colours", values = c("5 Hz" = "#fc8961", "1 Hz LIF" = "#b73779", "1 Hz TECO" = "#51127c")) +
  theme(text = element_text(size=14),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())+
  labs(y=bquote(''~SO[2]~area*''))




dm %>%
  #filter(!is.na(SFC_5hz))%>%
  ggplot()+
  geom_hline(yintercept = 0,
             size=2)+
  geom_point(aes(x=num,
                y=co2_10Hz_area-co2_1Hz_area,
                colour=Flight),
            size=4)+
  geom_errorbar(aes(x=num,
                    y=co2_10Hz_area-co2_1Hz_area,
                    ymin=co2_10Hz_area-co2_1Hz_area-co2_1Hz_uncert-co2_10Hz_unc,
                    ymax=co2_10Hz_area-co2_1Hz_area+co2_1Hz_uncert+co2_10Hz_unc),
                width=0.5)+
  theme_bw()+
  theme(text = element_text(size=14),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())+
  viridis::scale_colour_viridis(option="viridis", discrete=T) +
  labs(y=bquote(''~CO[2]~10~Hz~area~-~1~Hz~area*''))

#





























##### ACRUISE-2
dm <- read.csv("G:/My Drive/ACRUISE/Stuarts_integration/ACRUISE-2_integration/ACRUISE-2_integration_prelim_uncert.csv",
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


dm$date <- dmy_hms(dm$date) 

dm$date <- dm$date - hours(1)


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

dm_map <- dm_map %>% filter(Scrubber=="yes")

dm_map$sfc_group <- 0.1
dm_map$sfc_group[dm_map$SFC>0.1 & dm_map$SFC<=0.5] <- 0.5
dm_map$sfc_group[dm_map$SFC>0.5] <- 1

saveRDS(dm_map, "G:/My Drive/ACRUISE/Stuarts_integration/ACRUISE-2_integration/acruise2_sfcs_latlon.RDS")
write.csv(dm_map, "G:/My Drive/ACRUISE/Stuarts_integration/ACRUISE-2_integration/acruise2_sfcs_latlon_2_scrubbers_only.csv")





################ scrubbers only


dm %>%
  filter(Scrubber == "yes") %>%
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
                position = position_dodge(0.05))+
  facet_grid(~Sea, scales = "free", space="free_x")+
  theme_bw()+
  theme(text = element_text(size=14),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  viridis::scale_colour_viridis(option="viridis", discrete=T) +
  labs(x= "Ship", y="SFC (%)", colour="Sea")+
  guides(colour="none")



a <- dm %>%
  filter(Scrubber == "no") 

a <- dm %>%
  filter(Sea != "EC") 

mean(a$SFC)
#sd(a$SFC)
std.error(a$SFC)

#



range(a$SFC)
















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
ggplot(data=dm1)+
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




##################################

# co2/nox

#read
dm <- read.csv("G:/My Drive/ACRUISE/nvm/ACRUISE_integration/integration/integrations_ACRUISE1_C179_C190.csv",
               stringsAsFactors = F,
               header=T)


dm$limit <- 3.5
dm$limit[dm$Sea == "EC"] <- 0.1
dm <-  dm %>%
  filter(CO2 != 0)
dm$nox_ratio <- dm$NOx/(dm$CO2*1000)

#colour variable
dm %>%
  filter(Ship != "Star Cosmo" & Ship != "Grande Argentina") %>%
ggplot()+
  geom_point(aes(x=Ship,
                 y=nox_ratio,
                 colour=Sea),
             size=4)+
  geom_errorbar(aes(x=Ship,
                    y=nox_ratio,
                    ymin=nox_ratio-nox_ratio*0.11,
                    ymax=nox_ratio+nox_ratio*0.11),
                width=0.1,
                position = position_dodge(0.05),
                alpha=0.7)+
  theme_bw()+
  theme(text = element_text(size=13),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  viridis::scale_colour_viridis(option="viridis", discrete=T) +
  labs(x= "Ship", y=bquote(''~NO[x]/CO[2]~ratio*''), colour="Sea")#


a <- dm %>%
  filter(Sea == "EC") 

  mean(dm$nox_ratio)
  sd(dm$nox_ratio)
  std.error(dm$nox_ratio)
  range(dm$nox_ratio)
  


b <- c(3.45,2.98,2.51,2.59)

mean(a)
std.error(a$ratio_avg)
range(a$ratio_avg)



a <- a %>%
  filter(Ship != "Star Cosmo" & Ship != "Grande Argentina") %>%
  group_by(Ship) %>%
  summarise(ratio_avg = mean(nox_ratio))%>%
  na.omit()

a <- a[2:20,]

saveRDS(a, "G:/My Drive/ACRUISE/model/ACRUISE-1_ships_avg_nox_ratio.RDS")




###### nox mod comparison

ship_mod <- read.csv("G:/My Drive/ACRUISE/model/ACRUISE_1_model.csv", stringsAsFactors = F) 


ship_avg <- readRDS("G:/My Drive/ACRUISE/model/ACRUISE-1_ships_avg_nox_ratio.RDS")
ship_avg <-  ship_avg[2:18,]


ship_names <- read.csv("G:/My Drive/ACRUISE/model/ACRUISE-1_ship_names.csv", stringsAsFactors = F) %>%
  select("Name", "IMO") %>%
  rename("Ship"="Name") %>%
  na.omit()

ship_all <- left_join(ship_avg, ship_names, by="Ship")

ship_avg <- ship_all[!duplicated(ship_all),]

#merge with model
ship_comp <- left_join(ship_avg,ship_mod,by="IMO")

#make model sfc and pray
ship_comp$ratio_mod <- ship_comp$NOx..kg.*1.05/ship_comp$CO2..kg.


ship_comp$diff <- ship_comp$ratio_mod - ship_comp$ratio_avg

#ship_comp <- na.omit(ship_comp)

ship_comp %>%
  ggplot()+
  geom_col(aes(x=Ship,
               y=diff),
           fill="grey")+
  geom_point(aes(x=Ship,
                 y=ratio_avg),
             size=4,
             colour="#35b779")+
  geom_point(aes(x=Ship,
                 y=ratio_mod),
             size=4,
             colour="#440154")+
  theme_bw()+
  theme(text = element_text(size=14),
        axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5, 
                                   hjust=1))+
  #scale_y_continuous(breaks=seq(-3,5,0.5))+
  labs(x= "Ship name", 
       y=bquote(''~NO[x]~to~CO[2]~ratio*''))


#


mean(na.omit(ship_comp$SFC_avg))
sd(na.omit(ship_comp$SFC_avg))


#

a1 <- ship_comp

ggplot()+
  geom_line(aes(x=a1$ratio_avg,
                y=a1$diff),
            size=2,
            colour="#fde725",
            alpha=0.5)+
  geom_point(aes(x=a1$ratio_avg,
                 y=a1$diff),
             size=4,
             fill="#fde725",
             shape=21)+
  theme_bw()+
  theme(text = element_text(size=14))+
  scale_y_continuous(breaks=seq(-3,5,0.5))+
  labs(x=bquote(''~NO[x]~to~CO[2]~observed~ratio*''), 
       y="Ratio difference")

#




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
  geom_abline(slope=1,
              intercept = 0,
              colour="#31688e",
              alpha=0.8,
              size=3)+
  geom_abline(slope = results$Slope, 
              intercept = results$Intercept, 
              colour = "#a0da39",
              size=3) +
  geom_point(size=4,
             colour="grey")+
  geom_errorbarh(aes(xmax = X + Xstd, xmin = X - Xstd)) +
  geom_errorbar(aes(ymax = Y + Ystd, ymin = Y - Ystd)) + 
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
  mutate(wdir = def.pol.cart(matrix(c(v,u),ncol = 2)))

dm <- dm3 %>%
  filter(hgt_radr < 500) %>%
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
  geom_point(aes(x=lon,
                 y=so2,
                 colour=wind_flag,
                 shape=flight),
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









###################################

# comparison of all 3 campaigns

dm <-  read.csv("G:/My Drive/ACRUISE/Stuarts_integration/all_sfcs_310323.csv", 
         stringsAsFactors = F, 
         header = T)

dm$SFC <- dm$SFC*100
dm$Absolute.unc <-  dm$Absolute.unc*100

dm$zone <- "out"
dm$zone[dm$Sea == "EC"] <- "in"

dm$Where <- dm$Sea
dm$Where[dm$Where == "PTO" | dm$Where == "BB" | dm$Where == "PTL"] <- "SL"


dm$When <- as.factor(dm$When)

dm$SFC <- round(dm$SFC, digits = 2)
dm$SFC <-  as.numeric(dm$SFC)

df <- dm[dm$Where =="EC" & dm$When=="2019",]

df <- na.omit(df)
mean(df$SFC)
sd(df$SFC)

dm %>% 
  filter(Sea !="AO") %>%
  group_by(Where) %>%
  ggplot(aes(x=When,
             y=SFC,
             fill=When))+
  stat_boxplot(geom = "errorbar",
              width=0.5, size=2)+
  geom_boxplot(notch=F,
               alpha=0.5,
               outlier.size = 4)+
  facet_grid(~Where,
             space = "free_x")+
  theme_bw()+
  scale_fill_viridis(discrete=T, 
                     option="magma")+
  theme(text = element_text(size=24))+
  labs(y= "SFC (%)", x="Year")+
  guides(fill="none")














dm %>%
  filter(Sea !="AO") %>%
  group_by(zone) %>%
  ggplot()+
  geom_point(aes(x=SFC,
                 y=When,
                 colour=Sea),
             size=4)+
  # geom_errorbar(aes(x=SFC,
  #                   y=Sea,
  #                   xmin=SFC-Absolute.unc,
  #                   xmax=SFC*1.06+Absolute.unc),
  #               width=0.1,
  #               position = position_dodge(0.05),
  #               colour="grey")+
  facet_grid(~zone,
             space = "free_x")+
  theme_bw()+
  theme(text = element_text(size=16))+
  scale_colour_manual(values = c("EC" = "#21918c",
                                 "BB"="#440154",
                                 "SW"="#fde725",
                                 "PTO"="#35b779",
                                 "PTL"="#31688e"))+
  labs(y= "Sea", x="SFC (%)")+
guides(colour="none")



scale_colour_manual(values = c("EC" = "#21918c",
                               "BB"="#440154",
                               "SW"="#fde725",
                               "PTO"="#35b779",
                               "PTL"="#31688e"))





  
  
  #SHIP AREA LIMIT
  
library(dplyr)
dm <- dm %>% group_by(Ship) %>% mutate(id=cur_group_id())
  
dm$limit <- 0.5
dm$limit[dm$When==2019] <- 3.5
dm$limit[dm$Sea=="EC"] <- 0.1


dm %>%
  filter(Sea !="AO") %>%
  ggplot()+
  geom_hline(aes(yintercept = limit),
             colour="#BF8A8A",
             size=5)+
  geom_point(aes(x=id,
                 y=SFC,
                 colour=zone),
             size=10)+
  facet_grid(~When, scales = "free", space="free_x")+
  theme_bw()+
  theme(text = element_text(size=40,
                            colour="white"),
        axis.text = element_text(colour="white"),
        strip.background =element_rect(fill="black"),
        strip.text = element_text(colour = 'white'),
        axis.line = element_line(colour="white"),
        panel.border = element_rect(color = "white", 
                                    fill = NA, 
                                    size = 2))+
  scale_colour_manual(values = c("in" = "#2669BF",
                                 "out" = "#F26241"))+
labs(x= "Ship", y="SFC (%)", colour="SECA")




  
  ggplot()+
    geom_histogram(data=dm, 
                   aes(x=SFC,
                       y=..count../sum(..count..)),
                   binwidth =0.02,
                   fill="blue",
                   colour="blue",
                   alpha=0.5)+
    scale_x_continuous(breaks=seq(0,5,0.5))+
    theme_bw()+
    theme(text = element_text(size=14))+
    labs(x= "SFC (%): ACRUISE-2", y="Counts (fraction of total)")
  
  










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











#--------------------------------------------------

#comparison with model

#load  data

#acruise 2
ship_avg <- readRDS("G:/My Drive/ACRUISE/model/ACRUISE-2_ships_avg_SFC.RDS")
ship_mod <- read.csv("G:/My Drive/ACRUISE/model/ACRUISE_2_model.csv", stringsAsFactors = F)

#acruise 1
ship_avg <- readRDS("G:/My Drive/ACRUISE/model/ACRUISE-1_ships_avg_SFC.RDS")
ship_mod <- read.csv("G:/My Drive/ACRUISE/model/ACRUISE_1_model.csv", stringsAsFactors = F)
ship_avg$SFC_avg <- ship_avg$SFC_avg*100



#merge with model
ship_comp <- left_join(ship_avg,ship_mod,by="IMO")

#make model sfc and pray
ship_comp$SFC_mod <- ship_comp$SOx..kg.*1.6/ship_comp$CO2..kg.

ship_comp$SFC_mod <- ship_comp$SFC_mod*100


a1 <- ship_comp

#plot this diseaster

ship_comp %>%
  ggplot()+
  geom_point(aes(x=Ship,
                 y=SFC_avg),
             size=4,
             colour="#35b779")+
  geom_point(aes(x=Ship,
                 y=SFC_mod),
             size=4,
             colour="#440154")+
  theme_bw()+
  theme(text = element_text(size=10),
        axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5, 
                                   hjust=1))+
  labs(x= "Ship", y="SFC (%)")


# plot comparison

ship_comp %>%
ggplot()+
  geom_point(aes(x=SFC_avg,
             y=SFC_mod))




ggplot()+
  geom_histogram(data=ship_a2, 
                 aes(x=SFC_avg,
                     y=..count../sum(..count..)),
                 binwidth = 0.1)+
  geom_histogram(data=ship_a2, 
                 aes(x=SFC_mod,
                     y=..count../sum(..count..)),
                 binwidth =0.1,
                 fill="#440154",
                 alpha=0.6)+
  theme_bw()+
  theme(text = element_text(size=14))+
  viridis::scale_colour_viridis(option="viridis", discrete=T)+
  labs(x= "SFC (%)", y="Counts")


#


## difference plots

a2$diff <- a2$SFC_mod - a2$SFC_avg

ship_comp <- na.omit(ship_comp)

ship_comp %>%
  ggplot()+
  geom_col(aes(x=Ship,
                 y=diff),
             fill="grey")+
  geom_point(aes(x=Ship,
                 y=SFC_avg),
             size=4,
             colour="#35b779")+
  geom_point(aes(x=Ship,
                 y=SFC_mod),
             size=4,
             colour="#440154")+
  theme_bw()+
  theme(text = element_text(size=14),
        axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5, 
                                   hjust=1))+
  scale_y_continuous(breaks=seq(-3,5,0.5))+
  labs(x= "Ship name", 
       y="SFC (%)")


#


  mean(na.omit(ship_comp$SFC_avg))
  sd(na.omit(ship_comp$SFC_avg))


#
  
  ggplot()+
    geom_vline(xintercept=0.5,
               colour="black",
               alpha=0.5,
               size=2,
               linetype="dotted")+
    geom_vline(xintercept=3.5,
               colour="black",
               alpha=0.5,
               size=2,
               linetype="dashed")+
    geom_line(aes(x=a1$SFC_avg,
                   y=a1$diff),
               size=2,
               colour="#fde725",
              alpha=0.5)+
    geom_point(aes(x=a1$SFC_avg,
                  y=a1$diff),
              size=4,
              fill="#fde725",
              shape=21)+
    geom_line(aes(x=a2$SFC_avg,
                   y=a2$diff),
               size=2,
               colour="#31688e",
              alpha=0.5)+
    geom_point(aes(x=a2$SFC_avg,
                   y=a2$diff),
               size=4,
               fill="#31688e",
               shape=21)+
    theme_bw()+
    theme(text = element_text(size=14))+
    scale_y_continuous(breaks=seq(-3,5,0.5))+
    labs(x= "Observed SFC (%)", 
         y="SFC difference (%)")
  
  #











######################
# add imos a2
ship_names <- read.csv("G:/My Drive/ACRUISE/model/ACRUISE-2_ships.csv", stringsAsFactors = F) %>%
  select("Name", "IMO") %>%
  rename("Ship"="Name") %>%
  na.omit()

ship_all <- left_join(dm, ship_names, by="Ship")

ship_all <- ship_all[!is.na(ship_all$IMO),]

ship_avg <- ship_all %>%
  group_by(IMO) %>%
  summarise(SFC_avg = mean(SFC)) %>%
  left_join(., ship_names, by="IMO")

saveRDS(ship_avg, "G:/My Drive/ACRUISE/model/ACRUISE-2_ships_avg_SFC.RDS")




# add imos a1
ship_names <- read.csv("G:/My Drive/ACRUISE/model/ACRUISE-1_ship_names.csv", stringsAsFactors = F) %>%
  select("Name", "IMO") %>%
  rename("Ship"="Name") %>%
  na.omit()

dm <- read.csv("G:/My Drive/ACRUISE/Stuarts_integration/ACRUISE-1_integration/ACRUISE-1_integration_uncert.csv",
                stringsAsFactors = F,
                header=T) %>%
  select("SFC", "Ship")

ship_all <- left_join(dm, ship_names, by="Ship")

ship_all <- ship_all[!is.na(ship_all$IMO),]

ship_avg <- ship_all %>%
  group_by(IMO) %>%
  summarise(SFC_avg = mean(SFC)) %>%
  left_join(., ship_names, by="IMO")

saveRDS(ship_avg, "G:/My Drive/ACRUISE/model/ACRUISE-1_ships_avg_SFC.RDS")

#





#############################

#origing compare sfcs

dm <- read.csv("G:/My Drive/ACRUISE/Stuarts_integration/ACRUISE-1_integration/origin_comparison/comparison.csv",
               stringsAsFactors = F)

dm$SFCo <- dm$SO2_origin*0.232*100/dm$CO2_origin

dm$SFCr <- dm$SO2_r*0.232*100/dm$CO2_r

dm$diff <- dm$SFCo - dm$SFCr

dm$flag <- "b"
dm$flag[dm$diff>0] <- "a"

dm %>%
  ggplot()+
  geom_hline(aes(yintercept=0),
             colour="grey",
             size=2)+
  geom_point(aes(x=SFCr,
                 y=diff,
                 colour=flag),
             size=4
             )+
  theme_bw()+
  theme(text = element_text(size=14))+
  scale_colour_manual(values = c("b" = "#21918c",
                                 "a"="#440154"))+
  labs(x= "SFC R (%)", y="SFC Origin - SFC R (%)")+
  guides(colour="none")



