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
library(acruiseR)
library(nanotime)
library(ggplot2)
library(plotly)


# import and format
dm2 <-  readRDS("G:/My Drive/ACRUISE/ACRUISE2/data_raw/corechem_prelim_1Hz.RDS")
dm2$date_char <- as.character(dm2$date)
dm2$time_nano <- as.nanotime(dm2$date_char, format="%Y/%m/%d %H:%M:%E3S", tz="UTC")
dm2$flight <- as.numeric(dm2$flight)


fn <- 256
dm <- dm2[dm2$flight==fn,] 
plot(dm$date,dm$CO2_ppm)

dm <-  dm %>%
  filter(between(date, 
                 ymd_hms("2021-09-24 12:55:00"),
                 ymd_hms("2021-09-24 14:25:00"))) # c251

dm <-  dm %>%
  filter(between(date, 
                 ymd_hms("2021-09-27 14:15:00"),
                 ymd_hms("2021-09-27 16:30:00"))) # c253

dm <-  dm %>%
  filter(between(date, 
                 ymd_hms("2021-09-28 12:50:00"),
                 ymd_hms("2021-09-28 14:10:00"))) # c254

dm <-  dm %>%
  filter(between(date, 
                 ymd_hms("2021-09-29 11:30:00"),
                 ymd_hms("2021-09-29 14:30:00"))) # c255

dm <-  dm %>%
  filter(between(date, 
                 ymd_hms("2021-10-01 11:20:00"),
                 ymd_hms("2021-10-01 15:50:00"))) # c256

dm <-  dm %>%
  filter(between(date, 
                 ymd_hms("2021-10-03 10:00:00"),
                 ymd_hms("2021-10-03 13:00:00"))) # c257

dm <-  dm %>%
  filter(between(date, 
                 ymd_hms("2021-10-04 09:45:00"),
                 ymd_hms("2021-10-04 13:05:00"))) # c258

dm <-  dm %>%
  filter(between(date, 
                 ymd_hms("2021-10-05 12:15:00"),
                 ymd_hms("2021-10-05 14:30:00"))) # c259


dm <-  dm %>%
  filter(between(date, 
                 ymd_hms("2021-10-07 10:00:00"),
                 ymd_hms("2021-10-07 13:30:00"))) # c261


dm <-  dm %>%
  filter(between(date, 
                 ymd_hms("2021-10-08 12:40:00"),
                 ymd_hms("2021-10-08 16:15:00"))) # c262

dm <-  dm %>%
  filter(between(date, 
                 ymd_hms("2021-10-09 11:00:00"),
                 ymd_hms("2021-10-09 14:45:00"))) # c263


dm <-  dm %>%
  filter(between(date, 
                 ymd_hms("2021-10-11 11:00:00"),
                 ymd_hms("2021-10-11 14:00:00"))) # c264


dm <-  dm %>%
  filter(between(date, 
                 ymd_hms("2021-10-12 12:10:00"),
                 ymd_hms("2021-10-12 14:10:00"))) # c265




### SO2 ###

#background 
bg_so2 <- identify_background(dm$SO2_conc_scaled, method="gam", k=10)

acruiseR::plot_background(dm$SO2_conc_scaled, dm$time_nano, bg_so2,
                          plume_sd_threshold = 3,
                          plume_sd_starting = 1,
                          ylabel = "Concentration",
                          xlabel = "Time",
                          date_fmt = "%H:%M",
                          bg_alpha = 0.9) +
  theme(legend.position = "none") #+ ylim(-2,15)

#ggplotly()


#plumes 
plumz_so2 <- acruiseR::detect_plumes(dm$SO2_conc_scaled, bg_so2, dm$time_nano,
                                     plume_sd_threshold = 3,
                                     plume_sd_starting = 2,
                                     plume_buffer = 15,
                                     refit = TRUE )


acruiseR::plot_plumes(dm$SO2_conc_scaled, dm$time_nano, plumz_so2,
                      ylabel = "Concentration",
                      xlabel = "Time",
                      date_fmt = "%H:%M",
                      bg_alpha = 0.9)+
  theme(legend.position='none')#+ ylim(-2,10)

#ggplotly()



#areas
areaz_so2 <-  acruiseR::integrate_aup_trapz(dm$SO2_conc_scaled, dm$time_nano, plumz_so2, dx=1, uncertainty = 0.09, uncertainty_type = "relative")





### CO2 ###

#background 
bg_co2 <- identify_background(dm$CO2_ppm, method="gam", k=20)


# acruiseR::plot_background(dm$CO2_ppm, dm$time_nano, bg_co2,  
#                           plume_sd_threshold = 2,
#                           plume_sd_starting = 0.5,
#                           ylabel = "Concentration",
#                           xlabel = "Time",
#                           date_fmt = "%H:%M",
#                           bg_alpha = 0.9) +
#   theme(legend.position = "none") #+ylim(410,430)

#ggplotly()


#plumes 
plumz_co2 <- acruiseR::detect_plumes(dm$CO2_ppm, bg_co2, dm$time_nano,
                                     plume_sd_threshold = 3,
                                     plume_sd_starting = 0.5,
                                     plume_buffer = 15,
                                     refit = TRUE)

dm$CO2_ppm <- dm$CO2_ppm - bg_co2$bg

acruiseR::plot_plumes(dm$CO2_ppm, dm$time_nano, plumz_co2,
                      ylabel = "Concentration",
                      xlabel = "Time",
                      date_fmt = "%H:%M",
                      bg_alpha = 0.9)+
  theme(legend.position='none')# +ylim(407,412)

#ggplotly()


#areas
areaz_co2 <-  acruiseR::integrate_aup_trapz(dm$CO2_ppm, dm$time_nano, plumz_co2, dx=1, uncertainty = 0.599, uncertainty_type = "absolute") 




areaz_so2$start <- as.POSIXct(areaz_so2$start)
areaz_so2$end <- as.POSIXct(areaz_so2$end)
write.csv(areaz_so2, paste0("G:/My Drive/ACRUISE/Stuarts_integration/",fn,"_so2.csv"))



areaz_co2$start <- as.POSIXct(areaz_co2$start)
areaz_co2$end <- as.POSIXct(areaz_co2$end)
write.csv(areaz_co2, paste0("G:/My Drive/ACRUISE/Stuarts_integration/",fn,"_co2.csv"))







#############

#compare wonky data


dt.df_snap <- melt(dm, measure.vars = c("CO2_ppm", "SO2_conc_scaled"))

levels(dt.df_snap$variable) <- c("CO[2] (ppm)", "SO[2] (ppb)")

ggplot(data = dt.df_snap, 
       aes(x = date, 
           y = value)) +
  geom_line(aes(color = variable),
            size=1) +
  scale_colour_manual(values=c("#440154","#de4968")) +
  facet_grid(variable ~ ., 
             scales = "free_y", 
             labeller = label_parsed) +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size=12), 
        legend.position = "none", 
        axis.title.x=element_blank(), 
        axis.title.y=element_blank())

ggplotly()



ggplot(dm)+
  geom_point(aes(x=LON_GIN,
                 y=LAT_GIN,
                 colour=SO2_conc_scaled))



# plot so2, co2 and ch4 for appendix


dm <- read.csv("G:/My Drive/ACRUISE/Stuarts_integration/ACRUISE-2_integration/ACRUISE-2_integration_prelim_uncert.csv",
               stringsAsFactors = F,
               header=T)
#dm <- dm %>% select(-c("X"))


dm$SFC <- dm$SFC*100
dm$Relative.unc <- dm$Relative.unc*100
dm$Absolute.unc <- dm$Absolute.unc*100

dm$seaf = as.factor(dm$Sea)

dm$limit <- 0.5
dm$limit[dm$Sea == "EC"] <- 0.1



#fix time
dm$co2_start <-  dmy_hms(dm$co2_start)
dm$co2_end <-  dmy_hms(dm$co2_end)
dm$so2_start <-  dmy_hms(dm$so2_start)
dm$so2_end <-  dmy_hms(dm$so2_end)

dm$co2_start <- dm$co2_start - lubridate::hours(1)
dm$co2_end <- dm$co2_end - lubridate::hours(1)
dm$so2_start <- dm$so2_start - lubridate::hours(1)
dm$so2_end <- dm$so2_end - lubridate::hours(1)


# obviousely can't make it work
mydata <-  dm2
df <- dm
temp = mydata 
for(i in 1:nrow(df)){
  temp = temp %>% 
    filter(!between(date, df$co2_start[i], df$co2_end[i]))
}






###

dm2$CH4_ppb <-  dm2$CH4_ppm * 1000

flight <- dm2 %>% 
  filter(flight == 265)

dt.df_snap <- melt(flight, measure.vars = c("CO2_ppm", "SO2_conc_scaled", "CH4_ppb"))

levels(dt.df_snap$variable) <- c("CO[2] (ppm)", "SO[2] (ppb)", "CH[4] (ppb)")

ggplot(data = dt.df_snap, 
       aes(x = date, 
           y = value)) +
  geom_line(aes(color = variable),
            size=1) +
  scale_colour_manual(values=c("#440154","#de4968", "#7ad151")) +
  facet_grid(variable ~ ., 
             scales = "free_y", 
             labeller = label_parsed) +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size=14), 
        legend.position = "none", 
        axis.title.x=element_blank(), 
        axis.title.y=element_blank())



ggplotly()





