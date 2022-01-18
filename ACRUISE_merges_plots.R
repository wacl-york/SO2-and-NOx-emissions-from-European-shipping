################################################################################
### ACRUISE-1 data visualisation ###

#Contributions from: Dominika Pasternak, Will Drysdale.

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

################################################################################
### Initial setup ###

#set working directory
setwd("G:/My Drive/ACRUISE/ACRUISE2/data_raw")

#flight number
fn <-  c("c256")

#find and load files
core_1_files <-  list.files("./core_1hz",pattern = ".nc") # core 1 Hz data

#load files
ncdf <- paste0("./core_1hz/",core_1_files[grep(fn,core_1_files,ignore.case=TRUE)])
#csv <-  "./merged_data_v2.1/Merge_ ozone/c181_merge_ICL_HCHO_O3.csv"

#get date
origin <- ncdf %>% map_chr(str_sub, start = -28, end = -21)
origin <-  paste0(origin, " 00:00")



################################################################################
### NCDF file ###

#open the ncdf
data_nc <-  ncdf4::nc_open(ncdf)

#choose variables
vars_nc <- c("U_C", "V_C", "W_C", "LAT_GIN", "LON_GIN","HGT_RADR", "CPC_CNTS", "CPC_CNTS_FLAG", "WOW_IND")

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
                                              by = 1,
                                              tz = "UTC"))

#tidy up
rm(date, core_time, i, raw, vname, vars_nc)

#flags core
#CORE_1Hz$o3[CORE_1Hz$o3_flag != 0] <-  NA
#CORE_1Hz$so2[CORE_1Hz$so2_flag != 0] <-  NA
CORE_1Hz$CPC_CNTS[CORE_1Hz$CPC_CNTS_FLAG != 0] <- NA
#CORE_1Hz$co[CORE_1Hz$co_aero_flag != 0] <-  NA

#trim to flight only by weight on wheels
CORE_1Hz <- CORE_1Hz[!CORE_1Hz$WOW_IND != 0,]

#save prelim
saveRDS(CORE_1Hz, paste0("./core_for_stats/",fn,"_core_basic.RDS"))


################################################################################
### ICL, NOx and HCHO merge file ###

#read
dm1 <- read.csv(csv, stringsAsFactors = F)

#format date
dm1$date <-  as.POSIXct(dm1$date, tz="UTC")

#extract variables and rename
dm <-  subset(dm1, select=c(date, lat_gin, lon_gin, hgt_radr, v_c, u_c, w_c, cpc_cnts, cpc_cnts_flag, co_aero, co_aero_flag, co2, co2_flag, ch4, ch4_flag, no2_mr, no2_flag, no_mr, no_flag, ethane_icl, ethane_icl_flag, hcho_ppb)) %>% 
  dplyr::rename(lat = lat_gin,
                lon = lon_gin,
                v = v_c,
                u = u_c,
                w = w_c,
                cpc = cpc_cnts,
                co = co_aero,
                no2 = no2_mr,
                no = no_mr,
                ethane = ethane_icl,
                hcho = hcho_ppb)



### flagging and converting the data ###

#make NOx, NOx ratio and convert NOx species to ppb
dm$no <- dm$no*0.001
dm$no2 <- dm$no2*0.001
dm$nox <- (dm$no + dm$no2)
dm$nox_rat <- dm$no2/dm$no

#flags other
dm$ch4[dm$ch4_flag !=0] <-  NA
dm$co2[dm$co2_flag !=0] <- NA
dm$ethane[dm$ethane_icl_flag !=0] <- NA
dm$no[dm$no_flag !=0] <- NA
dm$no2[dm$no2_flag !=0] <- NA
dm$nox[dm$no2_flag !=0 | dm$no_flag !=0] <- NA
dm$no2[dm$no2 <= 0] <-  NA
dm$no[dm$no <= 0] <-  NA



################################################################################
### merge & save ###

#put all together
df <-  merge(dm, CORE_1Hz)

#export  
saveRDS(df, paste0("./full_merge/",fn,"_all_data_r0.RDS"))

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
dt.df <- melt(ship, measure.vars = c("hgt_radr", "o3", "so2", "nox_rat"))

varlabs <-  c(hgt_radr="Altitude (m)", o3="O3 (ppb)", so2="SO2 (ppb)", nox_rat="NO2/NO ")

ggplot(dt.df, aes(x = date, y = value)) +
  geom_line(aes(color = variable),size=1.5) +
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
