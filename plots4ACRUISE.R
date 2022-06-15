library(dplyr)
library(ggmap)
library(ggplot2)
library(reshape)
library(dygraphs)
library(xts)
library(grid)
library(Rmisc)
library(plotly)
library(tidyverse)
library(shonarrr)
library(wsdmiscr)
library(lubridate)
library(viridis)




#############################  READING FILES  #################################

#find & read the file
setwd("G:/My Drive/Ships_ACRUISE Campaign 2019/data/merged_data/shona_merge_files")
shona_merge <- read.csv("c187_all_data_merge_v1.csv", stringsAsFactors = F)

#make date being read as date
shona_merge$date <- ymd_hms(shona_merge$date,tz="UTC")

#average down to 1 Hz for NOx
shona_merge = shona_merge %>% 
  group_by(date) %>% 
  summarise_all(mean,na.rm = T)

#extract useful stuff
dm = shona_merge %>% 
  dplyr::rename(lat = lat_gin,
                lon = lon_gin) %>% 
  dplyr::select(date, lat, lon, hgt_radr, wow_ind, co2, co2_flag, ch4, ch4_flag, so2_teco, so2_teco_flag, co_aero, co_aero_flag, no_mr, no2_mr, no_flag, no2_flag) %>% 
  na.omit()

#rad alt to ft
dm$hgt_radr_ft = dm$hgt_radr * 3.28084


#############################  CROPPING & FILTERING DATA  #################################
#filter out flagged data
dm$no2_mr[dm$no2_flag == 2 | dm$no2_flag == 3] = NA
dm$no_mr[dm$no_flag==2 | dm$no_flag==3] = NA
dm$so2_teco[dm$so2_teco_flag==3] = NA
dm$co2[dm$co2_flag==3] = NA
dm$ch4[dm$ch4_flag==3] = NA


#crop data by date & time
data_cropped = so2 %>%
  filter(between(UTC_time, 
                 ymd_hms("2021-10-04 10:30:00"),
                 ymd_hms("2021-10-04 11:00:00")))

#choose data to plot
data_plot = dm

#if WOW wonky filter using
shona_merge[shona_merge$wow_ind == 0,]

#alternative filtering data
data_merge$date[data_merge$so2_teco > 10 & lubridate::hour(dm$date) >= 14] %>% 
  na.omit() %>% 
  round_date(unit = "1 min") %>% 
  unique()
#also ...lubridate::hour(dm$date) %in% c(14,15)...


#############################################################################




#############################  INTERACTIVE PLOTS #################################

data_merge_xts = xts::xts(dm[,c("AltMSL")],order.by = dm$date)

#timezone important, defaults to false
dygraphs::dygraph(data_merge_xts) %>% 
  dygraphs::dyRangeSelector() %>% 
  dygraphs::dyOptions(useDataTimezone = TRUE)

#############################################################################




#############################  STANDARD PLOTS #################################

#choose data
dm_plot = dm

#make a basic plot
ggplot() +
  geom_line(data=data_cropped,
            aes(UTC_time, SO2_conc_scaled),
            colour="#FDE725FF",
            size=2) +
  scale_color_viridis() +
  labs(x="Time", y=bquote(''~SO[2]~(ppb)*''))+
  theme_minimal() +
  theme(plot.title = element_blank(),  text = element_text(size=20, colour="white"), axis.text = element_text(colour = "white"))

#############################################################################




#############################  ALT VS LAT/LON PLOTS #################################

#choose data
dm_plot = data_cropped

#plot vs lat or lon
ggplot() +
  geom_point(data=dm_plot, aes(lat, hgt_radr_ft, col = so2_teco), size = 4) +
  scale_color_viridis(option="magma"
                      #, limits=c(-1.5, 16)
                      ) +
  #scale_x_continuous( limits = c(39.35, 39.6)) +
  #scale_y_continuous( limits = c(275, 500)) +
  labs(x="Latitude", y="Altitude / ft") +
  ggtitle("SO2 / ppb") +
  theme(plot.title = element_text(hjust = 0.5),  text = element_text(size=24), legend.title = element_blank()) +
  guides(size = FALSE)

#############################################################################




#############################  2 TRACES PLOTS #################################

#choose data
dm_plot = data_cropped

#to rescale axes artificcially multiply/divide secondary trace data and do opposit to the axis scale
ggplot() +
  geom_line(aes(dm_plot$date, dm_plot$no2_mr), col="red", size=2) +
  geom_line(aes(dm_plot$date, dm_plot$no2_flag*1000), size=1) +
  scale_color_viridis() +
  scale_y_continuous(sec.axis = sec_axis(~./1000, name = "Altitude / ft"))+
  labs(x="Time UTC", y="SO2 / ppb", color="CO2 / ppm") +
  theme(plot.title = element_text(hjust = 0.5),  text = element_text(size=24))+
  guides(size = FALSE)

#############################################################################




#############################  3D PLOTS #################################

#choose data
dm_map = dm1


axx <- list(
  title="Longitude"
  #,range = c(-9.865,-9.84)
)

axy <- list(
  title="Latitude"#,
  #range = c(39.6,39.35)
)

axz <- list(
  title="Altitude / ft"#,
  #range = c(275,500)
)




plot_ly(data=dm_map, 
        type='scatter3d',
        x=dm_map$lon,
        y=dm_map$lat,
        z=dm_map$hgt_radr_ft,
        mode='markers',
        marker=list(color=dm_map$so2, 
                    na.value = NA,
                    colorbar=list(title='SO2 / ppb'),
                    colorscale='Viridis',
                    opacity=0.5,
                    size=5)) %>%
  layout(
    title="Ship plume",
    scene=list(
      xaxis=axx,
      yaxis=axy,
      zaxis=axz
    ))



scene=list(
  xaxis=list(title="Longitude"),
  yaxis=list(title="Latitude"),
  zaxis=list(title="Altitude / ft"))



