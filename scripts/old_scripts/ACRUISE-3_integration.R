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
library(ggmap)
library(ggplot2)
library(Rmisc)
library(plotly)
library(tidyverse)
library(shonarrr)
library(wsdmiscr)
library(lubridate) 
library(viridis)
library(waclr)
library(data.table) 
library(stringr)






#find and load files
setwd("G:/My Drive/ACRUISE/ACRUISE3/data/")
acruise_files <-  list.files("./core_rds/", pattern = "RDS") 
na_files <-  list.files("./fgga_rds/", pattern = "RDS") 

# choose flight 
fn <- 287

#choose files
acruise <- paste0("./core_rds/",acruise_files[grep(fn,acruise_files,ignore.case=TRUE)])
nafile <- paste0("./fgga_rds/",na_files[grep(fn,na_files,ignore.case=TRUE)])

# core load and sort the date
core <-  readRDS(acruise)
core$date_char <- as.character(core$date)
core <- core[!is.na(core$date_char),]
core$time_nano <- as.nanotime(core$date_char, format="%Y-%m-%d %H:%M:%S", tz="UTC")
tz(core$date) <- "UTC"

#fgga load
fgga <- readRDS(nafile)



#peak task time
ggplot()+
  geom_point(aes(x=dmc$LON_GIN, y=dmc$LAT_GIN, colour=dmc$HGT_RADR))



### c284
dmc <-  core %>%
  filter(between(date, 
                 ymd_hms("2022-04-29 12:20:00"),
                 ymd_hms("2022-04-29 15:05:00"))) 

dmf <-  fgga %>%
  filter(between(date, 
                 as.nanotime("2022-04-29 12:20:00", 
                             format="%Y-%m-%d %H:%M:%S", 
                             tz="UTC"),
                 as.nanotime("2022-04-29 15:05:00", 
                             format="%Y-%m-%d %H:%M:%S", 
                             tz="UTC"))) 

### c285
dmc <-  core %>%
  filter(between(date, 
                 ymd_hms("2022-04-30 12:30:00"),
                 ymd_hms("2022-04-30 16:10:00"))) 

dmc <-  fgga %>%
  filter(between(date, 
                 as.nanotime("2022-04-30 14:00:00", 
                             format="%Y-%m-%d %H:%M:%S", 
                             tz="UTC"),
                 as.nanotime("2022-04-30 16:10:00", 
                             format="%Y-%m-%d %H:%M:%S", 
                             tz="UTC"))) 


#### c286
dmc <-  core %>%
  filter(between(date, 
                 ymd_hms("2022-05-01 12:15:00"),
                 ymd_hms("2022-05-01 15:20:00"))) 


dmf <-  fgga %>%
  filter(between(date, 
                 as.nanotime("2022-05-01 12:15:00", 
                             format="%Y-%m-%d %H:%M:%S", 
                             tz="UTC"),
                 as.nanotime("2022-05-01 15:20:00", 
                             format="%Y-%m-%d %H:%M:%S", 
                             tz="UTC"))) 

#### c287
dmc <-  core %>%
  filter(between(date, 
                 ymd_hms("2022-05-02 12:10:00"),
                 ymd_hms("2022-05-02 15:40:00")))

dmc <-  fgga %>%
  filter(between(date, 
                 as.nanotime("2022-05-02 13:50:00", 
                             format="%Y-%m-%d %H:%M:%S", 
                             tz="UTC"),
                 as.nanotime("2022-05-02 16:10:00", 
                             format="%Y-%m-%d %H:%M:%S", 
                             tz="UTC"))) 


### c292
dmc <-  core %>%
  filter(between(date, 
                 ymd_hms("2022-05-07 15:25:00"),
                 ymd_hms("2022-05-07 16:10:00"))) 

dmf <-  fgga %>%
  filter(between(date, 
                 as.nanotime("2022-05-07 15:25:00", 
                             format="%Y-%m-%d %H:%M:%S", 
                             tz="UTC"),
                 as.nanotime("2022-05-07 16:10:00", 
                             format="%Y-%m-%d %H:%M:%S", 
                             tz="UTC")))








### SO2 ###

#background 
bg_so2 <- identify_background(dmc$SO2_TECO, method="gam", k=5)

acruiseR::plot_background(dmc$SO2_TECO, dmc$time_nano, bg_so2,  
                          plume_sd_threshold = 5,
                          plume_sd_starting = 0.5,
                          ylabel = "Concentration",
                          xlabel = "Time",
                          date_fmt = "%H:%M",
                          bg_alpha = 0.9) +
  theme(legend.position = "none") #+ ylim(-2,15) 

#ggplotly()


#plumes 
plumz_so2 <- acruiseR::detect_plumes(dmc$SO2_TECO, bg_so2, dmc$time_nano,
                                     plume_sd_threshold =2,
                                     plume_sd_starting = 2,
                                     plume_buffer = 15,
                                     refit = TRUE )


acruiseR::plot_plumes(dmc$SO2_TECO, dmc$time_nano, plumz_so2,
                      ylabel = "Concentration",
                      xlabel = "Time",
                      date_fmt = "%H:%M",
                      bg_alpha = 0.9)+
  theme(legend.position='none')#+ ylim(-2,10)

ggplotly()

#areas
areaz_so2 <-  acruiseR::integrate_aup_trapz(dmc$SO2_TECO, dmc$time_nano, plumz_so2, dx=0.2, uncertainty = 0.3, uncertainty_type = "absolute")


#fn <- 284


#### peak heights
peakMaximumsList = list()

for(i in 1:nrow(plumz_so2)){
  
  peakMaximumsList[[i]] = dmc %>% 
    filter(between(time_nano, plumz_so2$start[i], plumz_so2$end[i])) %>% 
    filter(SO2_TECO == max(SO2_TECO))
}

peakMaximums = bind_rows(peakMaximumsList) %>% 
  tibble()


write.csv(peakMaximums, paste0("G:/My Drive/ACRUISE/Stuarts_integration/",fn,"_so2_ph.csv"))


### CO2 ###

#make 1 Hz
orgin <- as.character(core$date[1])
orgin <- paste0(substr(orgin, 1, 10), " 00:00:00")
dmf$date <- ymd_hms(orgin) + dmf$seconds_since_midnight

# dmf <- dmf %>%
#   mutate(date = round_date(date, unit = "0.25s")) %>%
#   group_by(date) %>%
#   summarise_all(mean, na.rm = T)

dmf$date_char <- as.character(dmf$date)
dmf <- dmf[!is.na(dmf$date_char),]
dmf$time_nano <- as.nanotime(dmf$date_char, format="%Y-%m-%d %H:%M:%S", tz="UTC")
tz(dmf$date) <- "UTC"

#dmf$time_nano <- dmf$date


#background 
bg_co2 <- identify_background(dmf$co2, method="gam", k=20)


acruiseR::plot_background(dmf$co2, dmf$time_nano, bg_co2,  
                          plume_sd_threshold = 3,
                          plume_sd_starting = 0.5,
                          ylabel = "Concentration",
                          xlabel = "Time",
                          date_fmt = "%H:%M",
                          bg_alpha = 0.9) +
  theme(legend.position = "none") #+ylim(410,430)

#ggplotly()


#plumes 
plumz_co2 <- acruiseR::detect_plumes(dmf$co2, bg_co2, dmf$time_nano,
                                     plume_sd_threshold = 3,
                                     plume_sd_starting = 0.5,
                                     plume_buffer = 15,
                                     refit = TRUE)


acruiseR::plot_plumes(dmf$co2, dmf$time_nano, plumz_co2,
                      ylabel = "Concentration",
                      xlabel = "Time",
                      date_fmt = "%H:%M",
                      bg_alpha = 0.9)+
  theme(legend.position='none')# +ylim(407,412)

#ggplotly()


#areas
areaz_co2 <-  acruiseR::integrate_aup_trapz(dmf$co2, dmf$time_nano, plumz_co2, dx=0.1, uncertainty = 0.574, uncertainty_type = "absolute") 




areaz_so2$start <- as.POSIXct(areaz_so2$start)
areaz_so2$end <- as.POSIXct(areaz_so2$end)
write.csv(areaz_so2, paste0("G:/My Drive/ACRUISE/Stuarts_integration/",fn,"_so2_ph.csv"))



areaz_co2$start <- as.POSIXct(areaz_co2$start)
areaz_co2$end <- as.POSIXct(areaz_co2$end)
write.csv(areaz_co2, paste0("G:/My Drive/ACRUISE/Stuarts_integration/",fn,"_co2_ph.csv"))










#############################

#read cims

fgga <- read.csv("G:/My Drive/ACRUISE/so2_comparison/ACSIS7_SO2_4Hz.csv", header = T, stringsAsFactors = F)
fgga$SO2_ppb <- fgga$SO2_ppt/1000
fgga$time_nano <- as.nanotime(fgga$date_time, format="%d/%m/%Y %H:%M:%E1S", tz="UTC")
fgga$date <- fgga$time_nano


#read lif
fgga <- read.delim("G:/My Drive/ACRUISE/so2_comparison/reso2datafromacruise3/SO2_mr_10Hz_C287.txt", header = T, sep=",", stringsAsFactors = F)

#fgga$SO2_ppb <- fgga$SO2_mr/1000

orgin <- as.character(fgga$Date_time[1])
orgin <- paste0(substr(orgin, 1, 10), " 00:00:00")

fgga$nsec <- as.numeric(substr(fgga$Date_time, 20, 23))
fgga$time <- as.numeric(hms(substr(fgga$Date_time, 12, 19)))
fgga$sec_from_mid <- fgga$nsec+fgga$time

fgga$date <- ymd_hms(orgin) + fgga$sec_from_mid

fgga <- fgga %>%
  mutate(date = round_date(date, unit = "0.2s")) %>%
  group_by(date) %>%
  summarise_all(mean, na.rm = T)


fgga$time_nano <- as.character(fgga$date)
fgga$time_nano <- as.nanotime(fgga$time_nano, format="%Y-%m-%d %H:%M:%E3S", tz="UTC")
fgga$date <- fgga$time_nano

dmc <- fgga

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





########################
# plot so2, co2 and ch4 for appendix

# choose flight 
fn <- 292

#choose files
acruise <- paste0("./core_rds/",acruise_files[grep(fn,acruise_files,ignore.case=TRUE)])
nafile <- paste0("./fgga_rds/",na_files[grep(fn,na_files,ignore.case=TRUE)])

# core load and sort the date
core <-  readRDS(acruise)
tz(core$date) <- "UTC"

#fgga load
dmf <- readRDS(nafile)

#average fgga
orgin <- as.character(core$date[1])
orgin <- paste0(substr(orgin, 1, 10), " 00:00:00")

dmf$date <- ymd_hms(orgin) + dmf$seconds_since_midnight
dmf <- openair::timeAverage(dmf, avg.time = "sec")
dmf$date_char <- as.character(dmf$date)
dmf <- dmf[!is.na(dmf$date_char),]
dmf$time_nano <- as.nanotime(dmf$date_char, format="%Y-%m-%d %H:%M:%S", tz="UTC")
tz(dmf$date) <- "UTC"


# drop unnecessary and match date
dmf <- subset(dmf, select=c(date, co2, ch4))
core <- subset(core, select=c(date, SO2_TECO))

#merge
dm2 <- left_join(core, dmf, keep=FALSE)



###


flight <- dm2

dt.df_snap <- melt(flight, measure.vars = c("co2", "SO2_TECO", "ch4"))

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






