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


# import and format
dm <-  read.csv("G:/My Drive/ACRUISE/ACRUISE1/merged/c183_merge_2.csv",
                header = T,
                stringsAsFactors = F)
dm$time_nano <- as.nanotime(dm$date, format="%d/%m/%Y %H:%M:%E3S", tz="UTC")
dm$date <- as.POSIXct(dm$date, tz="UTC")
#study_window <- as.nanoival("+2019-07-16 T08:05:00UTC -> 2019-07-16T11:55:00UTC+") #c186

# dm <- dm83
# dm$date_num <- as.numeric(dm$date)
# dm$time_nano <- as.nanotime(dm$date_num, format="%d/%m/%Y %H:%M:%E3S", tz="UTC")

dm <-  dm %>%
  filter(between(date, 
                 ymd_hms("2019-07-11 10:30:00"),
                 ymd_hms("2019-07-11 11:00:00"))) # c179

dm <-  dm %>%
  filter(between(date, 
                 ymd_hms("2019-07-11 13:20:00"),
                 ymd_hms("2019-07-11 14:20:00"))) # c180

dm <-  dm %>%
  filter(between(date, 
                 ymd_hms("2019-07-12 08:10:00"),
                 ymd_hms("2019-07-12 12:00:00"))) # c181

dm <-  dm %>%
  filter(between(date, 
                 ymd_hms("2019-07-12 14:10:00"),
                 ymd_hms("2019-07-12 16:50:00"))) # c182


dm <-  dm %>%
  filter(between(date, 
                 ymd_hms("2019-07-13 12:30:00"),
                 ymd_hms("2019-07-13 14:00:00"))) # c183


dm <-  dm %>%
  filter(between(date, 
                 ymd_hms("2019-07-16 08:50:00"),
                 ymd_hms("2019-07-16 11:15:00"))) # c186

dm <-  dm %>%
  filter(between(date, 
                 ymd_hms("2019-07-16 14:00:00"),
                 ymd_hms("2019-07-16 15:55:00"))) # c187

dm <-  dm %>%
  filter(between(date, 
                 ymd_hms("2019-07-17 08:20:00"),
                 ymd_hms("2019-07-17 11:04:00"))) # c188

dm <-  dm %>%
  filter(between(date, 
                 ymd_hms("2019-07-18 12:30:00"),
                 ymd_hms("2019-07-18 13:30:00"))) # c190


### SO2 ###

#background 
bg_so2 <- identify_background(dm$so2, method="gam", k=10)

acruiseR::plot_background(dm$so2, dm$time_nano, bg_so2,  
                          plume_sd_threshold = 3,
                          plume_sd_starting = 1,
                          ylabel = "Concentration",
                          xlabel = "Time",
                          date_fmt = "%H:%M",
                          bg_alpha = 0.9) +
  theme(legend.position = "none") #+ ylim(-2,15) 
            

#plumes 
plumz_so2 <- acruiseR::detect_plumes(dm$so2, bg_so2, dm$time_nano,
                        plume_sd_threshold = 3,
                        plume_sd_starting = 1,
                        plume_buffer = 15)


acruiseR::plot_plumes(dm$so2, dm$time_nano, plumz_so2,
                      ylabel = "Concentration",
                      xlabel = "Time",
                      date_fmt = "%H:%M",
                      bg_alpha = 0.9)  #+ ylim(-2,10)

#areas
areaz_so2 <-  acruiseR::integrate_aup_trapz(dm$so2, dm$time_nano, plumz_so2, dx=1)





### CO2 ###

#background 
bg_co2 <- identify_background(dm$co2, method="gam", k=50)


acruiseR::plot_background(dm$co2, dm$time_nano, bg_co2,  
                          plume_sd_threshold = 3,
                          plume_sd_starting = 0.5,
                          ylabel = "Concentration",
                          xlabel = "Time",
                          date_fmt = "%H:%M",
                          bg_alpha = 0.9) +
  theme(legend.position = "none")

#plumes 
plumz_co2 <- acruiseR::detect_plumes(dm$co2, bg_co2, dm$time_nano,
                                 plume_sd_threshold = 3,
                                 plume_sd_starting = 0.5,
                                 plume_buffer = 15)


acruiseR::plot_plumes(dm$co2, dm$time_nano, plumz_co2,
                      ylabel = "Concentration",
                      xlabel = "Time",
                      date_fmt = "%H:%M",
                      bg_alpha = 0.9) 

#areas
areaz_co2 <-  acruiseR::integrate_aup_trapz(dm$co2, dm$time_nano, plumz_co2,
                                        dx=1) 





### CH4 ###

#background 
bg_ch4 <- identify_background(dm$ch4, method="gam", k=10)

acruiseR::plot_background(dm$ch4, dm$time_nano, bg_ch4,  
                          plume_sd_threshold = 3,
                          plume_sd_starting = 0.5,
                          ylabel = "Concentration",
                          xlabel = "Time",
                          date_fmt = "%H:%M",
                          bg_alpha = 0.7) +
  theme(legend.position = "none") 

#plumes 
plumz_ch4 <- acruiseR::detect_plumes(dm$ch4, bg_ch4, dm$time_nano,
                                     plume_sd_threshold = 3,
                                     plume_sd_starting = 0.5,
                                     plume_buffer = 15)


acruiseR::plot_plumes(dm$ch4, dm$time_nano, plumz_ch4,
                      ylabel = "Concentration",
                      xlabel = "Time",
                      date_fmt = "%H:%M",
                      bg_alpha = 0.7) 

#areas
areaz_ch4 <-  acruiseR::integrate_aup_trapz(dm$ch4, dm$time_nano, plumz_ch4,
                                            dx=1)








areaz_so2$start <- as.POSIXct(areaz_so2$start)
areaz_so2$end <- as.POSIXct(areaz_so2$end)
write.csv(areaz_so2, "G:/My Drive/ACRUISE/Stuarts_integration/c188_so2.csv")


areaz_co2$start <- as.POSIXct(areaz_co2$start)
areaz_co2$end <- as.POSIXct(areaz_co2$end)
write.csv(areaz_co2, "G:/My Drive/ACRUISE/Stuarts_integration/c188_co2.csv")


areaz_ch4$start <- as.POSIXct(areaz_ch4$start)
areaz_ch4$end <- as.POSIXct(areaz_ch4$end)
write.csv(areaz_ch4, "G:/My Drive/ACRUISE/Stuarts_integration/c183_ch4.csv")
