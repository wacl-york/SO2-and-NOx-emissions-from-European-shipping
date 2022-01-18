################################################################################
### Loading packages ###

library(dplyr)
library(ggmap)
library(ggplot2)
library(tidyverse)
library(lubridate) 
library(viridis)
library(data.table)

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
    flights<-merge(flights, temp_dataset, all=T)
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
swas$date <- round.POSIXt(swas$start, units = "secs") %>% as.POSIXct(tz="UTC")
latlon$date <- round.POSIXt(latlon$date, units = "secs") %>% as.POSIXct(tz="UTC")

#marry swas and lat lon 
swas <-  merge(swas, latlon, by="date")

#save
saveRDS(swas, "./swas_all_logs_r0.RDS")
write.csv(swas, "./swas_all_logs_r0.csv")


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

