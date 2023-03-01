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
dm <-  read.csv("G:/My Drive/ACRUISE/ACRUISE1/merged/c179_merge.csv",
                header = T,
                stringsAsFactors = F)
dm$time_nano <- as.nanotime(dm$date, format="%d/%m/%Y %H:%M:%E3S", tz="UTC")
dm$date <- as.POSIXct(dm$date, tz="UTC")
#study_window <- as.nanoival("+2019-07-16 T08:05:00UTC -> 2019-07-16T11:55:00UTC+") #c186

# dm <- dm83
# dm$date_num <- as.numeric(dm$date)
# dm$time_nano <- as.nanotime(dm$date_num, format="%d/%m/%Y %H:%M:%E3S", tz="UTC")

dm <-  dm %>%
  filter(between(date, 
                 ymd_hms("2019-07-11 09:00:00"),
                 ymd_hms("2019-07-11 11:00:00"))) # c179

dm <-  dm %>%
  filter(between(date, 
                 ymd_hms("2019-07-11 13:40:00"),
                 ymd_hms("2019-07-11 14:00:00"))) # c180

dm <-  dm %>%
  filter(between(date, 
                 ymd_hms("2019-07-12 10:45:00"),
                 ymd_hms("2019-07-12 11:30:00"))) # c181

dm <-  dm %>%
  filter(between(date, 
                 ymd_hms("2019-07-12 14:10:00"),
                 ymd_hms("2019-07-12 16:50:00"))) # c182


dm <-  dm %>%
  filter(between(date, 
                 ymd_hms("2019-07-13 12:30:00"),
                 ymd_hms("2019-07-13 17:10:00"))) # c183

dm$co2[dm$co2 > 800] <- NA

dm$ch4[dm$ch4 > 8000] <- NA


dm <-  dm %>%
  filter(between(date, 
                 ymd_hms("2019-07-16 08:50:00"),
                 ymd_hms("2019-07-16 11:15:00"))) # c186

dm <-  dm %>%
  filter(between(date, 
                 ymd_hms("2019-07-16 14:00:00"),
                 ymd_hms("2019-07-16 15:55:00"))) # c187

dm <-  dm %>%
  filter(between(date, 
                 ymd_hms("2019-07-17 08:20:00"),
                 ymd_hms("2019-07-17 11:04:00"))) # c188

dm <-  dm %>%
  filter(between(date, 
                 ymd_hms("2019-07-18 12:30:00"),
                 ymd_hms("2019-07-18 13:30:00"))) # c190


### SO2 ###

#background 

bg_so2 <- identify_background(dm$so2, method="gam", k=3)

acruiseR::plot_background(dm$so2, dm$time_nano, bg_so2,  
                          plume_sd_threshold = 2,
                          plume_sd_starting = .5,
                          ylabel = "Concentration",
                          xlabel = "Time",
                          date_fmt = "%H:%M",
                          bg_alpha = 0.9) +
  theme(legend.position = "none") #+ ylim(-2,15) 

#ggplotly()
            

#plumes 
plumz_so2 <- acruiseR::detect_plumes(dm$so2, bg_so2, dm$time_nano,
                        plume_sd_threshold = 2,
                        plume_sd_starting = .5,
                        plume_buffer = 15,
                        refit = TRUE )


acruiseR::plot_plumes(dm$so2, dm$time_nano, plumz_so2,
                      ylabel = "Concentration",
                      xlabel = "Time",
                      date_fmt = "%H:%M",
                      bg_alpha = 0.9)+
  theme(legend.position = "none") #+ ylim(-2,15) 

#ggplotly()

#areas
areaz_so2 <-  acruiseR::integrate_aup_trapz(dm$so2, dm$time_nano, plumz_so2, dx=1, uncertainty = 0.06, uncertainty_type = "relative")




### CO2 ###

#background 
bg_co2 <- identify_background(dm$co2, method="gam", k=50)


acruiseR::plot_background(dm$co2, dm$time_nano, bg_co2,  
                          plume_sd_threshold = 1.2,
                          plume_sd_starting = .5,
                          ylabel = "Concentration",
                          xlabel = "Time",
                          date_fmt = "%H:%M",
                          bg_alpha = 0.9) +
  theme(legend.position = "none")

#ggplotly()


#plumes 
plumz_co2 <- acruiseR::detect_plumes(dm$co2, bg_co2, dm$time_nano,
                                 plume_sd_threshold = 3,
                                 plume_sd_starting = .5,
                                 plume_buffer = 15,
                                 refit = TRUE)


acruiseR::plot_plumes(dm$co2, dm$time_nano, plumz_co2,
                      ylabel = "Concentration",
                      xlabel = "Time",
                      date_fmt = "%H:%M",
                      bg_alpha = 0.9) +
  theme(legend.position = "none")

#ggplotly()

#areas
areaz_co2 <-  acruiseR::integrate_aup_trapz(dm$co2, dm$time_nano, plumz_co2, dx=1, uncertainty = 0.348, uncertainty_type = "absolute" ) 











areaz_so2$start <- as.POSIXct(areaz_so2$start)
areaz_so2$end <- as.POSIXct(areaz_so2$end)
write.csv(areaz_so2, "G:/My Drive/ACRUISE/Stuarts_integration/c180_so2.csv")


areaz_co2$start <- as.POSIXct(areaz_co2$start)
areaz_co2$end <- as.POSIXct(areaz_co2$end)
write.csv(areaz_co2, "G:/My Drive/ACRUISE/Stuarts_integration/c180_co2.csv")




#### plots





dm <-  dm %>%
  filter(between(date, 
                 ymd_hms("2019-07-12 15:16:00"),
                 ymd_hms("2019-07-12 16:06:00"))) # c182

dm <-  dm %>%
  filter(between(date, 
                 ymd_hms("2019-07-12 15:16:00"),
                 ymd_hms("2019-07-12 15:45:00"))) # c182



#basic map
data_map <-  dm #%>% na.omit() #pick data 
bbox = c(min(data_map$lon-0.2),min(data_map$lat-0.1),max(data_map$lon+0.2),max(data_map$lat+0.1)) #pick area
mymap = ggmap::get_stamenmap(bbox, zoom = 7) #pick zoom

ggmap(mymap)+
  geom_point(data = data_map, 
             aes(x = lon,
                 y = lat, 
                 colour = so2)) +
  scale_color_viridis(option="inferno", #limits=c(-10,150)
  ) +
  labs(title=bquote(''~SO[2]~ (ppb)*''))+
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size=14), 
        legend.title = element_blank(), 
        axis.title = element_blank()) +
  guides(size = FALSE) 


ggplot()+
  geom_line(data = data_map, 
            aes(x = date,
                y = so2, 
                colour = so2),
            size=1) +
  scale_color_viridis(option="inferno") +
  labs(x="date", y=bquote(''~SO[2]~ (ppb)*''))+
  theme_bw()+
  theme(text = element_text(size=13), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.title = element_blank())




#####################

dm <-  read.csv("G:/My Drive/ACRUISE/ACRUISE1/merged/c191_merge.csv",
                header = T,
                stringsAsFactors = F)

dm$date <- as.POSIXct(dm$date, tz="UTC")
dm2 <- subset(dm, !is.na(co2_flag))



dt.df_snap <- melt(dm2, measure.vars = c("co2", "so2", "ch4"))

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



#ggplotly()


























