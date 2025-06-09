library(here)
library(dplyr)
library(faamr)
library(tidyr)
library(stringr)

source(here::here('functions','flight_details.R'))

files = tibble(path = list.files(here::here('data','faam_raw'), recursive = T)) |> 
  mutate(
    full_path = here::here('data','faam_raw', path),
    flight = word(path, 1, sep = "/"),
         type = case_when(str_detect(path, "fgga") ~ "fgga",
                          str_detect(path, "core_faam") ~ "core",
                          str_detect(path, "flight-sum") ~ "flight-sum")
         ) |> 
  filter(type != "flight-sum") |> 
  nest_by(flight) |> 
  left_join(flight_details(), "flight")

for(i in 1:nrow(files)){
  
  core = files$data[[i]] |> 
    filter(type == "core") |> 
    pull(full_path) |> 
    read_faam_core(startDate = files$date_start[i], endDate = files$date_end[i], selectVar = c("SO2_TECO", "SO2_TECO_FLAG")) |> 
    pivot_wider() |> 
    filter(SO2_TECO_FLAG == 0)
    
  fgga = files$data[[i]] |> 
    filter(type == "fgga") |> 
    pull(full_path) |> 
    read_faam_fgga() |> 
    filter(co2_error_flag == 0) |> 
    select(date, co2_ppm = carbon_dioxide_co2_dry_mole_fraction_in_ppm)
  
}


files$data[[i]] |> 
  filter(type == "fgga") |> 
  pull(full_path) |> 
  readLines(n = 51)
