library(dplyr)
library(ggplot2)
library(nanotime)

sfc_per_plume = readRDS(here::here('data','sfc_per_plume.RDS'))
sfc_per_ship = readRDS(here::here('data','sfc_per_ship.RDS'))

plotList = list()

plotList$sfc_per_plume = sfc_per_plume |> 
  filter(sfc != 0) |> 
  ggplot()+
  geom_point(aes(ship_name, sfc))+
  geom_errorbar(aes(ship_name, ymin = sfc-uncertainty_sfc, ymax = sfc+uncertainty_sfc))+
  facet_wrap(~campaign, scales = "free_x")+
  scale_y_continuous(
    trans = scales::transform_pseudo_log(sigma = 0.01, base = 10),
    breaks = c(0,10^(-2:1))
  )+
  annotation_logticks(sides = "l")

plotList$sfc_ship_mean = sfc_per_ship |> 
  ggplot()+
  geom_point(aes(ship_name, sfc))+
  geom_errorbar(aes(ship_name, ymin = sfc-uncertainty_sfc, ymax = sfc+uncertainty_sfc))+
  scale_y_continuous(
    trans = scales::transform_pseudo_log(sigma = 0.01, base = 10),
    breaks = c(0,10^(-2:1))
    )+
  facet_wrap(~campaign, scales = "free_x")+
  annotation_logticks(sides = "l")

plotList$sfc_region_boxes = sfc_per_plume |> 
  filter(!is.na(sea)) |>
  mutate(sea = ifelse(sea == "EC", "SECA", "Open Ocean")) |> 
  ggplot()+
  geom_boxplot(aes(sea, sfc, fill = campaign))+
  scale_y_continuous(
    trans = scales::transform_pseudo_log(sigma = 0.01, base = 10),
    breaks = c(0,10^(-2:1))
  )+
  annotation_logticks(sides = "l")



pdf(here::here('plots','3_1_diagnostics.pdf'), width = 10, height = 8)

for(i in 1:length(plotList)){
  print(plotList[[i]])  
}

dev.off()




