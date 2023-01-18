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
setwd("G:/My Drive/ACRUISE/ACRUISE2/data_raw/final_merge/")


#find and load files
acruise_files <-  list.files(pattern = ".RDS") 

#flight number
fn <- 265

#chose file
acruise <- paste0(acruise_files[grep(fn,acruise_files,ignore.case=TRUE)])


core <-  readRDS(acruise[1])
core$date_char <- as.character(core$date)
core <- core[!is.na(core$date_char),]
core$time_nano <- as.nanotime(core$date_char, format="%Y-%m-%d %H:%M:%S", tz="UTC")
tz(core$date) <- "UTC"


fgga <- readRDS(acruise[2])

plot(core$date,core$SO2_TECO)



dm <-  dm2 %>%
  filter(between(date, 
                 ymd_hms("2021-09-24 12:55:00"),
                 ymd_hms("2021-09-24 14:25:00"))) # c251

dm <-  dm2 %>%
  filter(between(date, 
                 ymd_hms("2021-09-27 14:15:00"),
                 ymd_hms("2021-09-27 16:30:00"))) # c253

dm <-  dm2 %>%
  filter(between(date, 
                 ymd_hms("2021-09-28 12:50:00"),
                 ymd_hms("2021-09-28 14:10:00"))) # c254

dm <-  dm2 %>%
  filter(between(date, 
                 ymd_hms("2021-09-29 11:30:00"),
                 ymd_hms("2021-09-29 14:30:00"))) # c255

dm <-  dm2 %>%
  filter(between(date, 
                 ymd_hms("2021-10-01 11:20:00"),
                 ymd_hms("2021-10-01 15:50:00"))) # c256

dm <-  dm2 %>%
  filter(between(date, 
                 ymd_hms("2021-10-03 10:00:00"),
                 ymd_hms("2021-10-03 13:00:00"))) # c257

dm <-  dm2 %>%
  filter(between(date, 
                 ymd_hms("2021-10-04 09:45:00"),
                 ymd_hms("2021-10-04 13:05:00"))) # c258

dm <-  dm2 %>%
  filter(between(date, 
                 ymd_hms("2021-10-05 12:15:00"),
                 ymd_hms("2021-10-05 14:30:00"))) # c259


dm <-  dm2 %>%
  filter(between(date, 
                 ymd_hms("2021-10-07 10:00:00"),
                 ymd_hms("2021-10-07 13:30:00"))) # c261


dm <-  dm2 %>%
  filter(between(date, 
                 ymd_hms("2021-10-08 12:40:00"),
                 ymd_hms("2021-10-08 16:15:00"))) # c262

dm <-  dm2 %>%
  filter(between(date, 
                 ymd_hms("2021-10-09 11:00:00"),
                 ymd_hms("2021-10-09 14:45:00"))) # c263


dm <-  dm2 %>%
  filter(between(date, 
                 ymd_hms("2021-10-11 11:00:00"),
                 ymd_hms("2021-10-11 14:00:00"))) # c264


dmc <-  core %>%
  filter(between(date, 
                 ymd_hms("2021-10-12 12:10:00"),
                 ymd_hms("2021-10-12 14:10:00"))) # c265

dmf <-  fgga %>%
  filter(between(date, 
                 as.nanotime("2021-10-12 12:10:00", format="%Y-%m-%d %H:%M:%S", tz="UTC"),
                 as.nanotime("2021-10-12 14:10:00", format="%Y-%m-%d %H:%M:%S", tz="UTC"))) # c265



### SO2 ###

#background 
bg_so2 <- identify_background(dmc$SO2_TECO, method="gam", k=10)

acruiseR::plot_background(dmc$SO2_TECO, dmc$time_nano, bg_so2,  
                          plume_sd_threshold = 3,
                          plume_sd_starting = 0.5,
                          ylabel = "Concentration",
                          xlabel = "Time",
                          date_fmt = "%H:%M",
                          bg_alpha = 0.9) +
  theme(legend.position = "none") #+ ylim(-2,15) 

#ggplotly()


#plumes 
plumz_so2 <- acruiseR::detect_plumes(dmc$SO2_TECO, bg_so2, dmc$time_nano,
                                     plume_sd_threshold = 3,
                                     plume_sd_starting = 0.5,
                                     plume_buffer = 15,
                                     refit = TRUE )


acruiseR::plot_plumes(dmc$SO2_TECO, dmc$time_nano, plumz_so2,
                      ylabel = "Concentration",
                      xlabel = "Time",
                      date_fmt = "%H:%M",
                      bg_alpha = 0.9)+
  theme(legend.position='none')#+ ylim(-2,10)


#areas
areaz_so2 <-  acruiseR::integrate_aup_trapz(dmc$SO2_TECO, dmc$time_nano, plumz_so2, dx=1, uncertainty = 0.09, uncertainty_type = "relative")





### CO2 ###

#background 
bg_co2 <- identify_background(dmf$co2, method="gam", k=50)


acruiseR::plot_background(dmf$co2, dmf$date, bg_co2,  
                          plume_sd_threshold = 3,
                          plume_sd_starting = 0.5,
                          ylabel = "Concentration",
                          xlabel = "Time",
                          date_fmt = "%H:%M",
                          bg_alpha = 0.9) +
  theme(legend.position = "none") #+ylim(410,430)

ggplotly()


#plumes 
plumz_co2 <- acruiseR::detect_plumes(dmf$co2, bg_co2, dmf$date,
                                     plume_sd_threshold = 3,
                                     plume_sd_starting = 0.5,
                                     plume_buffer = 15,
                                     refit = TRUE)


acruiseR::plot_plumes(dmf$co2, dmf$date, plumz_co2,
                      ylabel = "Concentration",
                      xlabel = "Time",
                      date_fmt = "%H:%M",
                      bg_alpha = 0.9)+
  theme(legend.position='none')# +ylim(407,412)

ggplotly()


#areas
areaz_co2 <-  acruiseR::integrate_aup_trapz(dmf$co2, dmf$date, plumz_co2, dx=1, uncertainty = 0.599, uncertainty_type = "absolute") 




areaz_so2$start <- as.POSIXct(areaz_so2$start)
areaz_so2$end <- as.POSIXct(areaz_so2$end)
write.csv(areaz_so2, paste0("G:/My Drive/ACRUISE/Stuarts_integration/",fn,"_so2.csv"))



areaz_co2$start <- as.POSIXct(areaz_co2$start)
areaz_co2$end <- as.POSIXct(areaz_co2$end)
write.csv(areaz_co2, paste0("G:/My Drive/ACRUISE/Stuarts_integration/",fn,"_co2.csv"))





















########################










g = acruiseR::plot_plumes(dm$SO2_TECO, dm$time_nano, plumz_so2,
                          ylabel = "Concentration",
                          xlabel = "Time",
                          date_fmt = "%H:%M",
                          bg_alpha = 0.9)+
  theme(legend.position='none')#+ ylim(-2,10)

plume_data = g$data %>% 
  tibble()

plume_data %>% 
  filter(is.na(plume_id)) 


ggplotly()




#thesis - peak zoom 
dm3 <- cbind.data.frame(dm$date,dm$SO2_conc_scaled, backg) %>%
  dplyr::rename(date = `dm$date`,
                so2 = `dm$SO2_conc_scaled`)

dm3 <-  dm3 %>%
  filter(between(date, 
                 ymd_hms("2021-10-11 11:45:00"),
                 ymd_hms("2021-10-11 11:49:00"))) # c264


ggplot()+
  geom_line(aes(dm3$date, dm3$so2),
            colour="#fc8961",
            size=2,
            alpha=0.8)+
  geom_line(aes(dm3$date, dm3$backg),
            colour="#51127c",
            size=2,
            alpha=0.8)+
  geom_point(aes(dm3$date, dm3$so2),
             colour="black",
             size=2,
             shape=4,
             stroke=2)+
  theme_bw()+
  theme(text = element_text(size=13), 
        #legend.title = element_blank(),
  )+
  labs(x= "Time", y=bquote(''~SO[2]~(ppb)~''))#+
#guides(colour="none")


