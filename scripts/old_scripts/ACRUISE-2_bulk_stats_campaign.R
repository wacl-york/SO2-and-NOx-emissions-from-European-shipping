################################################################################
### Loading packages ###

library(dplyr)
library(ggmap)
library(ggplot2)
library(ggrepel)
library(tidyverse)
library(lubridate) 
library(viridis)
library(data.table)
library(openair)
library(shonarrr)

#############################################################################
### Merge ###

#set working directory
setwd("G:/My Drive/ACRUISE/ACRUISE2/data_raw/core_for_stats")

#list all files
flight_list <-  list.files("./",pattern = "core") 

#merge all files adding flight number
for (file in flight_list){
  # if the merged dataset doesn't exist, create it
  if (!exists("flights")){
    flights <- readRDS(file)
    flights$flight <- file %>% map_chr(str_sub, start = 2, end = 4)
  }
  # if the merged dataset does exist, append to it
  if (exists("flights")){
    temp_dataset <-readRDS(file)
    temp_dataset$flight <- file %>% map_chr(str_sub, start = 2, end = 4)
    flights<-rbind(flights, temp_dataset)
    rm(temp_dataset)
  }
}

saveRDS(flights, "./ACRUISE-2_merged_r0.RDS")
write.csv(flights, "./ACRUISE-2_merged_r0.csv")



#############################################################################
### FGGA and SO2 prelim data ###

#set working directory
setwd("G:/My Drive/ACRUISE/ACRUISE2/data_raw/fgga_prelim")

#list all swas logs
fgga_list <- list.files("./", pattern = "FGGA")

#merge all files 
for (file in fgga_list){
  # if the merged dataset doesn't exist, create it
  if (!exists("fgga")){
    fgga <- read.table(file, 
                     header = T,
                     sep=",",
                     stringsAsFactors = F)
  }
  # if the merged dataset does exist, append to it
  if (exists("fgga")){
    temp_dataset <-read.table(file, 
                            header = T, 
                            sep=",",
                            stringsAsFactors = F)
    fgga<-rbind(fgga, temp_dataset)
    rm(temp_dataset)
  }
}

#format fgga data
fgga <- fgga %>%
  dplyr::mutate(UTC_time=as.POSIXct(strptime(UTC_time, "%d/%m/%Y %H:%M:%OS", tz="UTC"))) %>%
  dplyr::rename(date=UTC_time)

#tidy up
rm(file, fgga_list)

#round seconds and marry up time zones
fgga <- openair::timeAverage(fgga, avg.time="sec")
dm$date <- round.POSIXt(dm$date, units = "secs") %>% as.POSIXct(tz="UTC")

#marry fgga and lat lon 
fgga2 <-  merge(dm, fgga, by="date", all.x=T) 
fgga2 <- left_join(dm,fgga,by="date")


### SO2 ###

#set working directory
setwd("G:/My Drive/ACRUISE/ACRUISE2/data_raw/so2_prelim")

#list all swas logs
so2_list <- list.files("./", pattern = "SO2")

#merge all files 
for (file in so2_list){
  # if the merged dataset doesn't exist, create it
  if (!exists("so2")){
    so2 <- read.table(file, 
                       header = T,
                       sep=",",
                       stringsAsFactors = F)
  }
  # if the merged dataset does exist, append to it
  if (exists("so2")){
    temp_dataset <-read.table(file, 
                              header = T, 
                              sep=",",
                              stringsAsFactors = F)
    so2<-rbind(so2, temp_dataset)
    rm(temp_dataset)
  }
}

#format so2 data
so2 <- so2 %>%
  dplyr::mutate(UTC_time=as.POSIXct(strptime(UTC_time, "%d/%m/%Y %H:%M:%S", tz="UTC"))) %>%
  dplyr::rename(date=UTC_time)

#tidy up
rm(file, so2_list)

#round seconds and marry up time zones
so2$date <- round.POSIXt(so2$date, units = "secs") %>% as.POSIXct(tz="UTC")
fgga2$date <- round.POSIXt(fgga2$date, units = "secs") %>% as.POSIXct(tz="UTC")

#marry so2 and lat lon 
corechem <-  merge(fgga2, so2, by="date", all=T) 

corechem2 <- corechem[!(corechem$flight==249),]


#save
saveRDS(corechem2, "./corechem_prelim.RDS")
write.csv(corechem2, "./corechem_prelim.csv")



############################################################################
### SWAS + LATLON DRAMA ###

# read the swas file (manually pre processed, because automated attempts resulted in some garbage)
swas <- read.csv("G:/My Drive/ACRUISE/ACRUISE2/SWAS_ACRUISE2/swas_soft_logs/combined_swas_logs.csv") %>% na.omit()

# read general flight dat file
dm <- readRDS("G:/My Drive/ACRUISE/ACRUISE2/data_raw/core_for_stats/ACRUISE-2_merged_r0.RDS")

# format swas data
swas <- swas %>%
  dplyr::mutate(Sample.Start=as.POSIXct(Sample.Start, 
                                        origin="1970-01-01", 
                                        tz="UTC"), 
                Sample.End=as.POSIXct(Sample.End, 
                                      origin="1970-01-01", 
                                      tz="UTC")) %>%
  dplyr::rename(case=Case,
                bottle=Bottle,
                start=Sample.Start,
                end=Sample.End,
                pres_psi=Pressure.at.close,
                bottle_id=Bottle.ID) %>%
  dplyr::select(., c(bottle_id, case, bottle, start, end, pres_psi,))

# remove flights that don't concern us to avoid unnecessary potential for confusion
dm <- dm %>% filter(flight != 249)

# get rid of the dubious NAs in the core faam (there's a vaguely legit reason why they are there, but don't remember rn)
cnt_na <- apply(dm, 1, function(z) sum(is.na(z))) # alternatively drop_na(LAT_GIN)
dm <- dm[cnt_na < 3,] # remove if there's more than 3 NAs, seems like I'm not asking for too much 

# extract lat lon from the bulk stats file
latlon <- dplyr::select(dm, c(date,LAT_GIN, LON_GIN, flight))

# round seconds and marry up time zones
swas$date <- round.POSIXt(swas$start, units = "secs") %>% as.POSIXct(tz="UTC")
latlon$date <- round.POSIXt(latlon$date, units = "secs") %>% as.POSIXct(tz="UTC")

# marry swas and lat lon 
swas2 <-  left_join(swas, latlon, by="date", all.x=T) 

# save because let's face it - you'll have to redo it anyway
saveRDS(swas2, "G:/My Drive/ACRUISE/ACRUISE2/SWAS_ACRUISE2/swas_all_logs_r1.RDS")
write.csv(swas2, "G:/My Drive/ACRUISE/ACRUISE2/SWAS_ACRUISE2/swas_all_logs_r1.csv")

############################################################################
### MAPS ###

# pick flight(s)
flights <- dm %>% filter(flight == 261) %>% na.omit() # just in case so the map box doesn't freak out
bottles <- swas2 %>% filter(flight == 261)

#make map box
bbox_cropped=c(min(flights$LON_GIN-0.1),min(flights$LAT_GIN-0.1),max(flights$LON_GIN+0.1),max(flights$LAT_GIN+0.1))
#bbox_cropped=c(-7.3, 51, -5, 51.75)

#make a map background
mymap = ggmap::get_stamenmap(bbox_cropped, zoom = 7)

#plot swas & flight tracks on the map
ggmap(mymap)+
  geom_point(data = flights, 
             aes(LON_GIN,LAT_GIN),
             size = 1,
             alpha = .7,
             colour="goldenrod2") +
  geom_point(data=bottles,
             aes(LON_GIN,LAT_GIN),
             size = 2,
             shape=4,
             stroke=2,
             colour="deeppink4") +
  geom_label_repel(data=bottles,
                   aes(x=LON_GIN,
                       y=LAT_GIN,
                       label=bottle_id))+
  theme_minimal() +
  theme(axis.title = element_blank())

#wind direction
flights <- flights %>% mutate(wd = shonarrr::calc_wind_direction(flights$U_C,flights$V_C))















