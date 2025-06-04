library(ggplot2)
library(reshape)
library(tidyverse)
library(lubridate) 
library(viridis)
library(openair)


ship <-  dm %>% filter(flight == 264) %>%
                na.omit()
              

short <-  ship %>% filter(between(date, 
                               ymd_hms("2021-10-11 09:50:00"),
                               ymd_hms("2021-10-11 10:30:00")))


ggplot(ship) +
  geom_point(aes(x = LON_GIN, y = LAT_GIN, colour=HGT_RADR),size=2) +
  scale_color_viridis(option="magma")+
  theme_minimal()


# Use melt to reshape data so values and variables are in separate columns
varlabs <-  c(HGT_RADR="Altitude (m)", CO2_ppm="CO2 (ppm)", CH4_ppm="CH4 (ppm)", SO2_conc_scaled="SO2 (ppb)")

dm2 <- dm %>% filter(flight == 258)
dm3 <- dm2 %>% filter(between(date, 
                              ymd_hms("2021-10-04 10:26:00"),
                              ymd_hms("2021-10-04 10:36:00")))

dt.df <- melt(dm3, measure.vars = c("HGT_RADR", "CO2_ppm","CH4_ppm", "SO2_conc_scaled"))

ggplot(dt.df, aes(x = date, y = value)) +
  geom_line(aes(color = variable),size=1) +
  facet_grid(variable ~ ., scales = "free_y", labeller = labeller(variable=varlabs)) +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none", axis.title.x=element_blank(), axis.title.y=element_blank())






