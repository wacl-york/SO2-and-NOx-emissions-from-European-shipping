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

dm <- dm2[dm2$flight==264,] 
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
                          plume_sd_starting = 0.5,
                          ylabel = "Concentration",
                          xlabel = "Time",
                          date_fmt = "%H:%M",
                          bg_alpha = 0.9) +
  theme(legend.position = "none") #+ ylim(-2,15) 

ggplotly()


#plumes 
plumz_so2 <- acruiseR::detect_plumes(dm$SO2_conc_scaled, bg_so2, dm$time_nano,
                                     plume_sd_threshold = 3,
                                     plume_sd_starting = 0.5,
                                     plume_buffer = 15,
                                     refit = TRUE )


acruiseR::plot_plumes(dm$SO2_conc_scaled, dm$time_nano, plumz_so2,
                      ylabel = "Concentration",
                      xlabel = "Time",
                      date_fmt = "%H:%M",
                      bg_alpha = 0.9)+
  theme(legend.position='none')#+ ylim(-2,10)

ggplotly()




#thesis - peak zoom 
dm3 <- cbind.data.frame(dm$date,dm$SO2_conc_scaled, backg) %>%
  dplyr::rename(date = `dm$date`,
                so2 = `dm$SO2_conc_scaled`)

dm3 <-  dm3 %>%
  filter(between(date, 
                 ymd_hms("2021-10-11 13:23:00"),
                 ymd_hms("2021-10-11 13:30:00"))) # c264


ggplot()+
  geom_line(aes(dm3$date, dm3$so2))







#areas
areaz_so2 <-  acruiseR::integrate_aup_trapz(dm$SO2_conc_scaled, dm$time_nano, plumz_so2, dx=1)





### CO2 ###

#background 
bg_co2 <- identify_background(dm$CO2_ppm, method="gam", k=50)


acruiseR::plot_background(dm$CO2_ppm, dm$time_nano, bg_co2,  
                          plume_sd_threshold = 3,
                          plume_sd_starting = 0.5,
                          ylabel = "Concentration",
                          xlabel = "Time",
                          date_fmt = "%H:%M",
                          bg_alpha = 0.9) +
  theme(legend.position = "none") #+ylim(410,430)

ggplotly()


#plumes 
plumz_co2 <- acruiseR::detect_plumes(dm$CO2_ppm, bg_co2, dm$time_nano,
                                     plume_sd_threshold = 3,
                                     plume_sd_starting = 0.5,
                                     plume_buffer = 15,
                                     refit = TRUE)


acruiseR::plot_plumes(dm$CO2_ppm, dm$time_nano, plumz_co2,
                      ylabel = "Concentration",
                      xlabel = "Time",
                      date_fmt = "%H:%M",
                      bg_alpha = 0.9)+
  theme(legend.position='none')# +ylim(407,412)

ggplotly()


#areas
areaz_co2 <-  acruiseR::integrate_aup_trapz(dm$CO2_ppm, dm$time_nano, plumz_co2,
                                            dx=1) 





### CH4 ###

#background 
bg_ch4 <- identify_background(dm$CH4_ppm, method="gam", k=20)

acruiseR::plot_background(dm$CH4_ppm, dm$time_nano, bg_ch4,  
                          plume_sd_threshold = 3,
                          plume_sd_starting = 1,
                          ylabel = "Concentration",
                          xlabel = "Time",
                          date_fmt = "%H:%M",
                          bg_alpha = 0.7) +
  theme(legend.position = "none") 

#plumes 
plumz_ch4 <- acruiseR::detect_plumes(dm$CH4_ppm, bg_ch4, dm$time_nano,
                                     plume_sd_threshold = 3,
                                     plume_sd_starting = 1,
                                     plume_buffer = 15,
                                     refit=TRUE)


acruiseR::plot_plumes(dm$CH4_ppm, dm$time_nano, plumz_ch4,
                      ylabel = "Concentration",
                      xlabel = "Time",
                      date_fmt = "%H:%M",
                      bg_alpha = 0.7) 

#areas
areaz_ch4 <-  acruiseR::integrate_aup_trapz(dm$CH4_ppm, dm$time_nano, plumz_ch4,
                                            dx=1)








areaz_so2$start <- as.POSIXct(areaz_so2$start)
areaz_so2$end <- as.POSIXct(areaz_so2$end)
write.csv(areaz_so2, "G:/My Drive/ACRUISE/Stuarts_integration/c263_so2.csv")


areaz_co2$start <- as.POSIXct(areaz_co2$start)
areaz_co2$end <- as.POSIXct(areaz_co2$end)
write.csv(areaz_co2, "G:/My Drive/ACRUISE/Stuarts_integration/c263_co2.csv")


areaz_ch4$start <- as.POSIXct(areaz_ch4$start)
areaz_ch4$end <- as.POSIXct(areaz_ch4$end)
write.csv(areaz_ch4, "G:/My Drive/ACRUISE/Stuarts_integration/c258_ch4.csv")
