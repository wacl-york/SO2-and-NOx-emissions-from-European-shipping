library(here)
library(dplyr)
library(stringr)
library(nanotime)
library(lubridate)

source(here::here('functions','flight_details.R'))

plumes = readRDS(here::here('data','faam_merge','acruise_merge.RDS')) |> 
  nest_by(flight) |> 
  inner_join(flight_details(), by = "flight") |> 
  mutate(
    bg_so2 = acruiseR::identify_background(
      concentration = data$SO2_TECO, 
      method="gam", 
      k=so2_k) |> 
      list(),
    plumes_so2 = acruiseR::detect_plumes(
      concentration = data$SO2_TECO, 
      background = bg_so2, 
      time = data$date,
      plume_sd_threshold = so2_a,
      plume_sd_starting = so2_b,
      plume_buffer = 15,
      refit = TRUE ) |> 
      list(),
    bg_co2 = acruiseR::identify_background(
      concentration = data$co2_drymole_ppm, 
      method="gam", 
      k=co2_k) |> 
      list(),
    plumes_co2 = acruiseR::detect_plumes(
      concentration = data$co2_drymole_ppm, 
      background = bg_co2, 
      time = data$date,
      plume_sd_threshold = co2_a,
      plume_sd_starting = co2_b,
      plume_buffer = 15,
      refit = TRUE ) |> 
      list(),
    area_so2 = acruiseR::integrate_aup_trapz(
      concentration = data$SO2_TECO,
      background = bg_so2$bg2,
      time = data$date, 
      plumes = plumes_so2,
      dx = 1,
      uncertainty = 0.348,
      uncertainty_type = "absolute") |> 
      list(),
    area_co2 = acruiseR::integrate_aup_trapz(
      concentration = data$co2_drymole_ppm,
      background = bg_co2$bg,
      time = data$date, 
      plumes = plumes_co2,
      dx = 1,
      uncertainty = 0.348,
      uncertainty_type = "absolute") |> 
      list()
  )

saveRDS(plumes, here::here('data','integration.RDS'))
