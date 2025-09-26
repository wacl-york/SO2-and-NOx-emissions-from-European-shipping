library(here)
library(dplyr)
library(faamr)
library(tidyr)
library(stringr)

source(here::here('functions','flight_details.R'))

files = list_flight_data_local(here::here('data','faam_raw')) |> 
  rename(flight = flightNumber) |> 
  filter(fileType != "flight-sum.txt") |> 
  nest_by(flight) |> 
  left_join(flight_details(), "flight")

dirOut = here::here('data','faam_merge')

if(!dir.exists(dirOut)){
  dir.create(dirOut)
}

fileOut = here::here(dirOut, "acruise_merge.RDS")

datList = list()

cli::cli_progress_bar(total = nrow(files))

for(i in 1:nrow(files)){
  
  cli::cli_progress_update()
  
  core = files$data[[i]] |> 
    filter(fileType == "core_1hz.nc") |> 
    pull(filePath) |> 
    read_faam_core(
      startDate = files$date_start[i], 
      endDate = files$date_end[i], 
      selectVar = c("SO2_TECO", "SO2_TECO_FLAG", "LAT_GIN", "LON_GIN", "ALT_GIN","PALT_RVS","HGT_RADR"),
      averageNanoString = "00:00:01") |> 
    pivot_wider() |> 
    filter(SO2_TECO_FLAG == 0) 
  
  fgga = files$data[[i]] |> 
    filter(fileType == "faam-fgga.na") |> 
    pull(filePath) |> 
    read_faam_fgga(
      allowExtrapolatedCal = TRUE,
      requireHighFlow = FALSE,
      averageNanoString = "00:00:01",
      extractUncert = TRUE,
      applyBias = TRUE)
  
  datList[[i]] = tibble(date = seq(files$date_start[i], files$date_end[i], 1e9)) |> # these are nanotimes so 1e9 is 1 second
    left_join(core, "date") |> 
    left_join(fgga, "date") |> 
    mutate(flight = files$flight[i])
  
  
  if("core-nitrates.nc" %in% files$data[[i]]$fileType){
    
    nitrates = files$data[[i]] |> 
      filter(fileType == "core-nitrates.nc") |> 
      pull(filePath) |> 
      read_faam_nitrates(
        averageNanoString = "00:00:01",
        startDate = files$date_start[i],
        endDate = files$date_end[i],
        allowReducedQuality = TRUE,
        allowSuspect = TRUE) |> 
      pivot_wider() |> 
      select(-seconds_since_midnight)
    
    datList[[i]] = datList[[i]] |> 
      left_join(nitrates, by = "date")
  }
  
}

dat = bind_rows(datList)

saveRDS(dat, fileOut)
