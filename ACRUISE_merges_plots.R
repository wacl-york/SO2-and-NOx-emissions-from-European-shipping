################################################################################
### ACRUISE-1 data visualisation ###

#Contributions from: Dominika Pasternak, Will Drysdale, Adam Vaughan.

################################################################################
### Loading packages ###

library(dplyr)
library(ggmap)
library(ggplot2)
library(reshape2)
library(grid)
library(gridExtra)
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

################################################################################
### Initial setup ###

#set working directory
setwd("G:/My Drive/ACRUISE/ACRUISE2/data")


################################################################################
### NCDF file ###


# for(fn in c("c265","c264", "c263", "c262", "c261", "c260", "c259", "c258", "c257", "c256", "c255", "c254", "c253")){
#   print(paste("now starting:",fn))
  
  for(fn in c("c284","c285", "c286", "c287", "c292")){
    print(paste("now starting:",fn))

 # put processing code here
#find and load files
core_1_files <-  list.files("./core_1hz",pattern = ".nc") # core 1 Hz data

#load files
ncdf <- paste0("./core_1hz/",core_1_files[grep(fn,core_1_files,ignore.case=TRUE)])
#csv <-  "./merged_data_v2.1/Merge_ ozone/c181_merge_ICL_HCHO_O3.csv"

#get date
origin <- ncdf %>% map_chr(str_sub, start = -28, end = -21)
origin <-  paste0(origin, " 00:00")

#open the ncdf
data_nc <-  ncdf4::nc_open(ncdf)

#choose variables
vars_nc <- c("U_C", "V_C", "W_C", "LAT_GIN", "LON_GIN","HGT_RADR", "WOW_IND", "SO2_TECO", "SO2_TECO_FLAG", "O3_2BTECH", "O3_2BTECH_FLAG")

#turn NetCDF into data frame
for (i in 1:length(vars_nc)) {
  vname <- vars_nc[i]
  raw <- as.vector(ncdf4::ncvar_get(data_nc,vname,collapse_degen=FALSE))
  if(i==1){
    CORE_1Hz <- data.frame(raw)
    names(CORE_1Hz) <- vname
  } 
  else {
    CORE_1Hz <- cbind(CORE_1Hz,raw)
    names(CORE_1Hz)[ncol(CORE_1Hz)] <- vname
  }
}

#get time and adjust frequency 
core_time <- ncvar_get(data_nc, attributes(data_nc$dim)$names[1]) %>% as.vector()
date <- strptime(x = origin, format ="%Y%m%d %H:%M") + (core_time)
CORE_1Hz$date <- base::as.POSIXct(seq.POSIXt(from = min(date)+(1),
                                             to = max(date)+1, 
                                             by = 1),
                                  tz = "UTC") 

tz(CORE_1Hz$date) <- "UTC"

#tidy up
rm(date, core_time, i, raw, vname, vars_nc)

#flags core
CORE_1Hz$O3_2BTECH[CORE_1Hz$O3_2BTECH_FLAG != 0] <-  NA
CORE_1Hz$SO2_TECO[CORE_1Hz$SO2_TECO_FLAG != 0] <-  NA

#trim to flight only by weight on wheels
CORE_1Hz <- CORE_1Hz[!CORE_1Hz$WOW_IND != 0,]

#save prelim
saveRDS(CORE_1Hz, paste0("./core_rds/",fn,"_core_data.RDS"))


}



################################################################################
### fgga ###


for(fn in c("c265","c264", "c263", "c262", "c261", "c260", "c259", "c258", "c257", "c256", "c255", "c254", "c253")){
  print(paste("now starting:",fn))
  
  
  #find and load files
  fgga_files <-  list.files("./fgga_r0",pattern = ".na") # fgga data
  
  #chose file
  fgga_paths <- paste0("./fgga_r0/",fgga_files[grep(fn,fgga_files,ignore.case=TRUE)])
  
  #get origin
  core_1_files <-  list.files("./core_1hz",pattern = ".nc") # core 1 Hz data
  ncdf <- paste0("./core_1hz/",core_1_files[grep(fn,core_1_files,ignore.case=TRUE)])
  origin <- ncdf %>% map_chr(str_sub, start = -28, end = -21)
  origin <-  paste0(origin, " 00:00")
  
  
  #format file
  fgga <- read.delim(fgga_paths, header=FALSE, sep=" ", skip=62) %>% 
    dplyr::rename(seconds_since_midnight=V1,
                  co2=V2,
                  co2_flag=V3,
                  ch4=V4,
                  ch4_flag=V5,
                  flow=V6) %>% 
    tibble() %>% 
    rowwise() %>% 
    mutate(whole_secs = floor(seconds_since_midnight),
           decimal_secs = as.character(round(seconds_since_midnight-whole_secs,1))) %>% 
    ungroup() %>% 
    mutate(decimal_secs = ifelse(decimal_secs == "0", ".0", str_remove(decimal_secs,"0")),
           date = paste0(as.character(ymd_hm(origin)+whole_secs),decimal_secs) %>% 
             as.nanotime(format = "%Y-%m-%d %H:%M:%E1S"))
  
  fgga$co2[fgga$co2_flag != 0] <- NA
  fgga$ch4[fgga$ch4_flag != 0] <- NA
  
  saveRDS(fgga, paste0("./final_merge/",fn,"_fgga_10hz_data.RDS"))
  
}


################################################################################
### 1 Hz file ###


for(fn in c("c265","c264", "c263", "c262", "c261", "c260", "c259", "c258", "c257", "c256", "c255", "c254", "c253"
            )){
  print(paste("now starting:",fn))
  
  # put processing code here
  #find and load files
  core_1_files <-  list.files("./core_1hz",pattern = ".nc") # core 1 Hz data
  
  #load files
  ncdf <- paste0("./core_1hz/",core_1_files[grep(fn,core_1_files,ignore.case=TRUE)])
  #csv <-  "./merged_data_v2.1/Merge_ ozone/c181_merge_ICL_HCHO_O3.csv"
  
  #get date
  origin <- ncdf %>% map_chr(str_sub, start = -28, end = -21)
  origin <-  paste0(origin, " 00:00")
  
  #open the ncdf
  data_nc <-  ncdf4::nc_open(ncdf)
  
  #choose variables
  vars_nc <- c("U_C", "V_C", "W_C", "LAT_GIN", "LON_GIN","HGT_RADR", "WOW_IND", "SO2_TECO", "SO2_TECO_FLAG", "O3_2BTECH", "O3_2BTECH_FLAG")
  
  #turn NetCDF into data frame
  for (i in 1:length(vars_nc)) {
    vname <- vars_nc[i]
    raw <- as.vector(ncdf4::ncvar_get(data_nc,vname,collapse_degen=FALSE))
    if(i==1){
      CORE_1Hz <- data.frame(raw)
      names(CORE_1Hz) <- vname
    } 
    else {
      CORE_1Hz <- cbind(CORE_1Hz,raw)
      names(CORE_1Hz)[ncol(CORE_1Hz)] <- vname
    }
  }
  
  #get time and adjust frequency 
  core_time <- ncvar_get(data_nc, attributes(data_nc$dim)$names[1]) %>% as.vector()
  date <- strptime(x = origin, format ="%Y%m%d %H:%M") + (core_time)
  CORE_1Hz$date <- base::as.POSIXct(seq.POSIXt(from = min(date)+(1),
                                               to = max(date)+1, 
                                               by = 1),
                                    tz = "UTC") 
  
  tz(CORE_1Hz$date) <- "UTC"
  
  #tidy up
  rm(date, core_time, i, raw, vname, vars_nc)
  
  #flags core
  CORE_1Hz$O3_2BTECH[CORE_1Hz$O3_2BTECH_FLAG != 0] <-  NA
  CORE_1Hz$SO2_TECO[CORE_1Hz$SO2_TECO_FLAG != 0] <-  NA
  
  #trim to flight only by weight on wheels
  CORE_1Hz <- CORE_1Hz[!CORE_1Hz$WOW_IND != 0,]
  
  
  #find and load files
  fgga_files <-  list.files("./fgga_r0",pattern = ".na") # fgga data
  
  #chose file
  fgga_paths <- paste0("./fgga_r0/",fgga_files[grep(fn,fgga_files,ignore.case=TRUE)])
  
  #format file
  fgga <- read.delim(fgga_paths, header=FALSE, sep=" ", skip=62) %>% 
    dplyr::rename(date=V1,
                  co2=V2,
                  co2_flag=V3,
                  ch4=V4,
                  ch4_flag=V5,
                  flow=V6) 
  
  fgga$date <- strptime(x = origin, format ="%Y%m%d %H:%M") + (fgga$date)
  
  
  fgga$co2[fgga$co2_flag != 0] <- NA
  fgga$ch4[fgga$ch4_flag != 0] <- NA
  
  
  df <-  merge(CORE_1Hz, fgga, all=FALSE)
  
  #save prelim
  saveRDS(df, paste0("./final_merge/",fn,"_all_1Hz_data.RDS"))
  
  
}
























































################################################################################




ship <-  df %>% filter(between(date, 
                               ymd_hms("2019-07-12 10:50:00"),
                               ymd_hms("2019-07-12 11:29:00")))




#map - comparison
map_o3 <- ggmap(mymap)+
  geom_path(data = ship, 
            aes(lon,lat, colour = o3),
            size = 4) +
  scale_color_viridis(option="viridis" 
                      #,limits=c(300, 2000)
  ) +
  #labs(x="Longitude", y="Latitude") +
  ggtitle(bquote(''~O[3]~ (ppb)*'')) +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size=24), legend.title = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())

map_so2 <- ggmap(mymap)+
  geom_path(data = ship, 
            aes(lon,lat, colour = so2),
            size = 4) +
  scale_color_viridis(option="magma" 
                      #,limits=c(300, 2000)
  ) +
  #labs(x="Longitude", y="Latitude") +
  ggtitle(bquote(''~SO[2]~ (ppb)*'')) +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size=24), legend.title = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())

map_nox <- ggmap(mymap)+
  geom_path(data = ship, 
            aes(lon,lat, colour = nox),
            size = 4) +
  +
  scale_color_viridis(option="cividis" 
#                      ,limits=c(0, 100)
  ) +
  #labs(x="Longitude", y="Latitude") +
  ggtitle(bquote(''~NO[x]~ (ppb)*'')) +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size=24), legend.title = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())

map_alt <- ggmap(mymap)+
  geom_path(data = ship, 
            aes(lon,lat, colour = hgt_radr),
            size = 4) +
  scale_color_viridis(option="plasma" 
                      #                      ,limits=c(0, 100)
  ) +
  labs(title="Altitude (m)") +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size=24), legend.title = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())

multiplot(map_o3, map_so2, map_nox, map_alt, cols =4)


# Use melt to reshape data so values and variables are in separate columns
dt.df <- melt(dm, measure.vars = c("co2", "ch4","so2" ))

varlabs <-  c( so2="SO2", co2="CO2", ch4="CH4")

ggplot(dt.df, aes(x = date, y = value)) +
  geom_line(aes(color = variable),size=1) +
  facet_grid(variable ~ ., scales = "free_y", labeller = labeller(variable=varlabs)) +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size=24), legend.position = "none", axis.title.x=element_blank(), axis.title.y=element_blank())



#map + vocs
ggmap(mymap)+
  geom_(data = ship, 
            aes(lon,lat, colour = so2),
            size = 4) +
  geom_point(data=shipp, aes(lon, lat), colour="white", size =5)+
  scale_color_viridis(option="magma") +
  ggtitle(bquote(''~SO[2]~ (ppb)*'')) +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size=24), legend.title = element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())


#o3 vs nox
ship1 <- ship %>% filter(nox>.2)
ggplot(ship) +
  geom_point(aes(x = nox, y = o3, colour=so2),size=4, alpha=.9) +
  scale_color_viridis(option="magma", limits=c(-1.5,2.3)) +
  theme_bw()+
  labs(x=bquote(''~NO[x]~ (ppb)*''), y=bquote(''~O[3]~ (ppb)*''), color=bquote(''~SO[2]~ (ppb)*''))+
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size=24))






# ### Plots for peak numbering ###
# plot_so2 <-  ggplot() +
#   geom_line(data=ship,
#             aes(date, so2),
#             size=1, color="deeppink4") +
#   labs(x= NULL, y=bquote(''~SO[2]~ (ppb)*'')) +
#   theme_bw() +
#   theme(plot.title = element_text(hjust = 0.5),  text = element_text(size=24), axis.text.x=element_blank(), axis.title.y=element_text(colour="deeppink4"))+
#   guides(size = FALSE) 
# 
# 
# plot_nox <-  ggplot() +
#   geom_line(data=ship,
#             aes(date, nox),
#             size=1, color="dodgerblue4") +
#   labs(x="Time UTC", y=bquote(''~NO[x]~ (ppb)*'')) +
#   theme_bw() +
#   theme(plot.title = element_text(hjust = 0.5),  text = element_text(size=24), axis.title.y=element_text(colour="dodgerblue4"))+
#   guides(size = FALSE)
# 
# plot_o3 <-  ggplot() +
#   geom_line(data=ship,
#             aes(date, o3),
#             size=1, color="springgreen3") +
#   labs(x= NULL, y=bquote(''~O[3]~ (ppb)*'')) +
#   theme_bw() +
#   theme(plot.title = element_text(hjust = 0.5),  text = element_text(size=24),axis.text.x=element_blank(), axis.title.y=element_text(colour="springgreen3"))+
#   guides(size = FALSE)
# 
# #plot
# grid.arrange(plot_o3, plot_so2, plot_nox, nrow = 3)
