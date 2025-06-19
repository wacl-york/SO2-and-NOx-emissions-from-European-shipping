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
    filter(type == "core") |> 
    pull(full_path) |> 
    read_faam_core(
      startDate = files$date_start[i], 
      endDate = files$date_end[i], 
      selectVar = c("SO2_TECO", "SO2_TECO_FLAG", "LAT_GIN", "LON_GIN", "ALT_GIN","PALT_RVS","HGT_RADR"),
      averageNanoString = "00:00:01") |> 
    pivot_wider() |> 
    filter(SO2_TECO_FLAG == 0) 
    
  fgga = files$data[[i]] |> 
    filter(type == "fgga") |> 
    pull(full_path) |> 
    read_faam_fgga(averageNanoString = "00:00:01")
  
  datList[[i]] = tibble(date = seq(files$date_start[i], files$date_end[i], 1e9)) |> # these are nanotimes so 1e9 is 1 second
    left_join(core, "date") |> 
    left_join(fgga, "date") |> 
    mutate(flight = files$flight[i])

}

dat = bind_rows(datList)

saveRDS(dat, fileOut)
