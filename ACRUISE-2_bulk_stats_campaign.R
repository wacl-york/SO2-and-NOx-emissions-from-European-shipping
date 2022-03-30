################################################################################
### Loading packages ###

library(dplyr)
library(ggmap)
library(ggplot2)
library(tidyverse)
library(lubridate) 
library(viridis)
library(data.table)
library(openair)

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
### Format SWAS ###

#set working directory
setwd("G:/My Drive/ACRUISE/ACRUISE2/data_raw/swas_soft_logs")

#list all swas logs
swas_list <- list.files("./", pattern = "log")

#merge all files 
for (file in swas_list){
  # if the merged dataset doesn't exist, create it
  if (!exists("swas")){
    swas <- read.csv(file, 
                     skip = 2, 
                     header = T, 
                     stringsAsFactors = F)
  }
  # if the merged dataset does exist, append to it
  if (exists("swas")){
    temp_dataset <-read.csv(file, 
                            skip = 2, 
                            header = T, 
                            stringsAsFactors = F)
    swas<-rbind(swas, temp_dataset)
    rm(temp_dataset)
  }
}

#format swas data
swas <- swas %>%
  dplyr::mutate(Sample.Start=as.POSIXct(Sample.Start, 
                                        origin="1970-01-01", 
                                        tz="UTC"), 
                Sample.End=as.POSIXct(Sample.End, 
                                      origin="1970-01-01", 
                                      tz="UTC")) %>%
  dplyr::rename(case=CASE.ID,
                canister=Canister,
                start=Sample.Start,
                end=Sample.End,
                pres_psi=Pressure.at.close) %>%
  dplyr::select(., c(case, canister, start, end, pres_psi))

#tidy up
rm(temp_dataset, file, swas_list)

#remove wonky bottles
#swas$pres_psi[swas$pres_psi<30] <- NA
#swas <- na.omit(swas)

#remove duplicates
swas <- dplyr::distinct(swas)

#extract lat lon from the bulk stats file
latlon <- dplyr::select(dm, c(date,LAT_GIN, LON_GIN))

#round seconds and marry up time zones
swas$date <- round.POSIXt(swas$Start, units = "secs") %>% as.POSIXct(tz="UTC")
latlon$date <- round.POSIXt(latlon$date, units = "secs") %>% as.POSIXct(tz="UTC")

#marry swas and lat lon 
swas2 <-  merge(swas, latlon, by="date", all.x=T) 

#save
saveRDS(swas, "./swas_all_logs_r0.RDS")
write.csv(swas, "./swas_all_logs_r0.csv")




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

#read swas csv & remove NAs so merge doesn't freak out
swas <- read.csv("G:/My Drive/ACRUISE/ACRUISE2/ACRUISE_latlon.csv") %>% na.omit()
swas$Start <- dmy_hms(swas$Start)
swas$End <- dmy_hms(swas$End)

#read bulk core merge
dm <- readRDS("G:/My Drive/ACRUISE/ACRUISE2/data_raw/core_for_stats/ACRUISE-2_merged_r0.RDS")

#extract coordinates data
latlon <- data.frame(dm$date, dm$LAT_GIN, dm$LON_GIN) %>% rename(lat=dm.LAT_GIN,lon=dm.LON_GIN, date=dm.date)

#round seconds and marry up time zones
swas$date <- round.POSIXt(swas$Start, units = "secs") %>% as.POSIXct(tz="UTC")
latlon$date <- round.POSIXt(latlon$date, units = "secs") %>% as.POSIXct(tz="UTC")

#marry swas and lat lon 
swas2 <-  merge(swas, latlon, by="date", all.x=T) %>% rename(lat_start=lat,lon_start=lon) %>% select(-c(date)) %>% na.omit()

#save the bitch
write.csv(swas2, "G:/My Drive/ACRUISE/ACRUISE2/ACRUISE_latlon_done.csv")









############################################################################
### MAPS ###

#kick out NAs
flights <- flights %>% drop_na(LAT_GIN)
flights <- filter(flights, flight == 261)

#make map box
bbox_cropped=c(min(flights$LON_GIN-0.1),min(flights$LAT_GIN-0.1),max(flights$LON_GIN+0.1),max(flights$LAT_GIN+0.1))

#make a map background
mymap = ggmap::get_stamenmap(bbox_cropped, zoom = 6)

#plot stuff on the map
ggmap(mymap)+
  geom_point(data = swas, 
            aes(LON_GIN,LAT_GIN),
            size = 1,
            alpha = .8) +
  scale_colour_viridis(discrete=TRUE) +
  #theme_minimal() +
  theme(text = element_text(size=14), axis.title = element_blank())+
  guides(colour=F)

