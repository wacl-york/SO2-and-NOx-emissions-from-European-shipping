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
dm <-  read.csv("G:/My Drive/ACRUISE/Stuarts_integration/Methane-to-go-Poland_integration/MethanToGoPoland-Jun22_Helipod-F04_2022-Jun-14_quickloockextract_v04.csv",
                header = T,
                stringsAsFactors = F)
dm$date <- as.POSIXct(dm$NTPTime, tz="UTC", origin = dmy_hms("01-01-1970 00:00:00"))
dm$date_ch <- as.character(dm$date)
dm$time_nano <- as.nanotime(dm$date_ch, format="%Y-%m-%d %H:%M:%E3S", tz="UTC")

plot(dm$date,dm$Pic2401CH4)

dm <-  dm %>%
  filter(between(date, 
                 ymd_hms("2022-06-14 12:45:00"),
                 ymd_hms("2022-06-14 15:17:00"))) 


### CH4 Picarro ###

#background 
bg_ch4p <- identify_background(dm$Pic2401CH4, method="gam", k=20)

acruiseR::plot_background(dm$Pic2401CH4, dm$time_nano, bg_ch4p,  
                          plume_sd_threshold = 0.2,
                          plume_sd_starting = 0.001,
                          ylabel = "Concentration",
                          xlabel = "Time",
                          date_fmt = "%H:%M",
                          bg_alpha = 0.9) +
  theme(legend.position = "none") + ylim(1.95,2.05) 


#plumes 
plumz_ch4p <- acruiseR::detect_plumes(dm$Pic2401CH4, bg_ch4p, dm$time_nano,
                                     plume_sd_threshold = 0.2,
                                     plume_sd_starting = 0.001,
                                     plume_buffer = 15,
                                     refit = TRUE )


acruiseR::plot_plumes(dm$Pic2401CH4, dm$time_nano, plumz_ch4p,
                      ylabel = "Concentration",
                      xlabel = "Time",
                      date_fmt = "%H:%M",
                      bg_alpha = 0.9)  #+ ylim(1.95,2.05)

#areas
areaz_ch4p <-  acruiseR::integrate_aup_trapz(dm$Pic2401CH4, dm$time_nano, plumz_ch4p, dx=1)



### CH4 Licor ###

#background 
bg_ch4l <- identify_background(dm$Li7700CH4, method="gam", k=20)

acruiseR::plot_background(dm$Li7700CH4, dm$time_nano, bg_ch4l,  
                          plume_sd_threshold = 0.2,
                          plume_sd_starting = 0.001,
                          ylabel = "Concentration",
                          xlabel = "Time",
                          date_fmt = "%H:%M",
                          bg_alpha = 0.9) +
  theme(legend.position = "none") + ylim(1.95,2.15) 


#plumes 
plumz_ch4l <- acruiseR::detect_plumes(dm$Li7700CH4, bg_ch4l, dm$time_nano,
                                      plume_sd_threshold = 0.2,
                                      plume_sd_starting = 0.001,
                                      plume_buffer = 15,
                                      refit = TRUE )


acruiseR::plot_plumes(dm$Li7700CH4, dm$time_nano, plumz_ch4l,
                      ylabel = "Concentration",
                      xlabel = "Time",
                      date_fmt = "%H:%M",
                      bg_alpha = 0.9)  + ylim(1.95,2.15)

#areas
areaz_ch4l <-  acruiseR::integrate_aup_trapz(dm$Li7700CH4, dm$time_nano, plumz_ch4l, dx=1)






### CO2 ###

#background 
bg_co2 <- identify_background(dm$Li7500ACO2Mf, method="gam", k=50)


acruiseR::plot_background(dm$Li7500ACO2Mf, dm$time_nano, bg_co2,  
                          plume_sd_threshold = 3,
                          plume_sd_starting = 0.5,
                          ylabel = "Concentration",
                          xlabel = "Time",
                          date_fmt = "%H:%M",
                          bg_alpha = 0.9) +
  theme(legend.position = "none")

#plumes 
plumz_co2 <- acruiseR::detect_plumes(dm$Li7500ACO2Mf, bg_co2, dm$time_nano,
                                     plume_sd_threshold = 3,
                                     plume_sd_starting = 0.5,
                                     plume_buffer = 15,
                                     refit = TRUE)


acruiseR::plot_plumes(dm$Li7500ACO2Mf, dm$time_nano, plumz_co2,
                      ylabel = "Concentration",
                      xlabel = "Time",
                      date_fmt = "%H:%M",
                      bg_alpha = 0.9) 

#areas
areaz_co2 <-  acruiseR::integrate_aup_trapz(dm$Li7500ACO2Mf, dm$time_nano, plumz_co2,
                                            dx=1) 







areaz_ch4p$start <- as.POSIXct(areaz_ch4p$start)
areaz_ch4p$end <- as.POSIXct(areaz_ch4p$end)
write.csv(areaz_ch4p, "G:/My Drive/ACRUISE/Stuarts_integration/Picaro_ch4.csv")

areaz_ch4l$start <- as.POSIXct(areaz_ch4l$start)
areaz_ch4l$end <- as.POSIXct(areaz_ch4l$end)
write.csv(areaz_ch4l, "G:/My Drive/ACRUISE/Stuarts_integration/Licor_ch4.csv")

areaz_co2$start <- as.POSIXct(areaz_co2$start)
areaz_co2$end <- as.POSIXct(areaz_co2$end)
write.csv(areaz_co2, "G:/My Drive/ACRUISE/Stuarts_integration/c190_co2.csv")



