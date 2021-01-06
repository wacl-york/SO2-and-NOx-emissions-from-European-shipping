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
setwd("G:/My Drive/ACRUISE_integration2/merges")

#load files
ncdf <- "./ncdfs/core_faam_20190711_v004_r1_c180_1hz.nc"
csv <-  "G:/My Drive/Ships_ACRUISE Campaign 2019/data/merged_data_v2.1/Merge_ ozone/c180_merge_ICL_HCHO_O3.csv"

#get date
origin <- stringr::str_sub(ncdf, start= 19, end=26) %>% gsub(".na","",.)
origin <-  paste0(origin, " 00:00")



################################################################################
### NCDF file ###

#extract variables
data_nc <-  ncdf4::nc_open(ncdf)
o3 <- ncvar_get(data_nc, attributes(data_nc$var)$names[125]) %>% as.vector()
o3_flag <- ncvar_get(data_nc, attributes(data_nc$var)$names[126]) %>% as.vector()
so2 <- ncvar_get(data_nc, attributes(data_nc$var)$names[67]) %>% as.vector()
so2_flag <- ncvar_get(data_nc, attributes(data_nc$var)$names[68]) %>% as.vector()

#extract and format time
time <- ncvar_get(data_nc, attributes(data_nc$dim)$names[1]) %>% as.vector()
date <- strptime(x = origin, format ="%Y%m%d %H:%M", tz="UTC") + (time)

#put vectors together
core <- data.frame(date, o3, o3_flag, so2, so2_flag) %>% na.omit

#flags
core$o3[core$o3_flag != 0] <-  NA
core$so2[core$so2_flag != 0] <-  NA


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

#make NOx, NOx ratio and convert NOx species to ppb
dm$no <- dm$no*0.001
dm$no2 <- dm$no2*0.001
dm$nox <- (dm$no + dm$no2)
dm$nox_rat <- dm$no2/dm$no

#remove flagged data
dm$cpc[dm$cpc_cnts_flag != 0] <- NA
dm$co[dm$co_aero_flag != 0] <-  NA
dm$ch4[dm$ch4_flag !=0] <-  NA
dm$co2[dm$co2_flag !=0] <- NA
dm$ethane[dm$ethane_icl_flag !=0] <- NA
dm$no[dm$no_flag !=0] <- NA
dm$no2[dm$no2_flag !=0] <- NA
dm$nox[dm$no2_flag !=0 | dm$no_flag !=0] <- NA
dm$no2[dm$no2 <= 0] <-  NA
dm$no[dm$no <= 0] <-  NA

#put all together
df <-  merge(dm, core)

#export  
saveRDS(df, "G:/My Drive/ACRUISE_integration2/merges/merged/c180_merge.rds")

################################################################################




ship <-  dm %>% filter(between(date, 
                               ymd_hms("2019-07-12 14:30:00"),
                               ymd_hms("2019-07-12 15:03:00")))


mymap = ggmap::get_stamenmap(bbox = c(min(ship$lon-0.02),min(ship$lat-0.02),max(ship$lon+0.02),max(ship$lat+0.02)), zoom = 7)

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
  scale_color_viridis(option="magma", ,limits=c(-1.5,2.3)) +
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
