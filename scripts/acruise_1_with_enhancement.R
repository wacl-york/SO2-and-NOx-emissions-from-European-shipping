library(here)
library(dplyr)
library(stringr)
library(nanotime)
library(lubridate)

flightFilters = tribble(
  ~flight, ~date_start, ~date_end,
  "179", ymd_hms("2019-07-11 09:00:00"), ymd_hms("2019-07-11 11:00:00"),
  "180", ymd_hms("2019-07-11 13:40:00"), ymd_hms("2019-07-11 14:00:00"),
  "181", ymd_hms("2019-07-12 10:45:00"), ymd_hms("2019-07-12 11:30:00"),
  "182", ymd_hms("2019-07-12 14:10:00"), ymd_hms("2019-07-12 16:50:00"),
  "183", ymd_hms("2019-07-13 12:30:00"), ymd_hms("2019-07-13 17:10:00"),
)

# import and format
acruise1 <-  readRDS(here::here('data','data_merges','acruise 1','final_merge','ACRUISE-1_merged.RDS')) |> 
  tibble() |> 
  inner_join(flightFilters, join_by(flight == flight, between(date, date_start, date_end))) |> 
  mutate(time_nano = as.nanotime(date)) |> 
  nest_by(flight) |> 
  mutate(
    bg_so2 = acruiseR::identify_background(
      concentration = data$so2, 
      method="gam", 
      k=3) |> 
      list(),
    plumes_so2 = acruiseR::detect_plumes(
      concentration = data$so2, 
      background = bg_so2, 
      time = data$time_nano,
      plume_sd_threshold = 2,
      plume_sd_starting = .5,
      plume_buffer = 15,
      refit = TRUE ) |> 
      list(),
    bg_co2 = acruiseR::identify_background(
      concentration = data$co2, 
      method="gam", 
      k=3) |> 
      list(),
    plumes_co2 = acruiseR::detect_plumes(
      concentration = data$co2, 
      background = bg_co2, 
      time = data$time_nano,
      plume_sd_threshold = 3,
      plume_sd_starting = .5,
      plume_buffer = 15,
      refit = TRUE ) |> 
      list(),
    data = data |> 
      mutate(so2_enhance = so2-bg_so2$bg,
             co2_enhance = co2-bg_co2$bg
             ) |> 
      list(),
    area_so2 = acruiseR::integrate_aup_trapz(
      concentration = data$so2_enhance,
      time = data$time_nano, 
      plumes = plumes_so2,
      dx = 1,
      uncertainty = 0.348,
      uncertainty_type = "absolute") |> 
      list(),
    area_co2 = acruiseR::integrate_aup_trapz(
      concentration = data$co2_enhance,
      time = data$time_nano, 
      plumes = plumes_co2,
      dx = 1,
      uncertainty = 0.348,
      uncertainty_type = "absolute") |> 
      list()
    )





acruise1$data[[1]]

  
g1 = acruiseR::plot_plumes(concentration = acruise1$data[[1]]$so2,
                      time = acruise1$data[[1]]$time_nano,
                      plumes = acruise1$plumes_so2[[1]],
                      background = acruise1$bg_so2[[1]],  
                      plume_sd_threshold = 2,
                      plume_sd_starting = .5,
                      ylabel = "Concentration",
                      xlabel = "Time",
                      date_fmt = "%H:%M",
                      bg_alpha = 0.9) +
  theme(legend.position = "none") #+ ylim(-2,15) 

g2 = acruiseR::plot_plumes(concentration = acruise1$data[[1]]$co2,
                      time = acruise1$data[[1]]$time_nano,
                      plumes = acruise1$plumes_co2[[1]],
                      background = acruise1$bg_co2[[1]],  
                      plume_sd_threshold = 3,
                      plume_sd_starting = .5,
                      ylabel = "Concentration",
                      xlabel = "Time",
                      date_fmt = "%H:%M",
                      bg_alpha = 0.9) +
  theme(legend.position = "none") #+ ylim(-2,15) 


ggpubr::ggarrange(g1, g2, ncol = 1)

