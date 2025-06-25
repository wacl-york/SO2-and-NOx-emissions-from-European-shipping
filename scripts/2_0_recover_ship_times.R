library(dplyr)
library(tidyr)
library(readxl)
library(nanotime)
library(lubridate)

number_encounter = function(namesOrder){
  # namesOrder is the ship names in ship_rle$values
  
  count = vector(mode = "numeric", length = length(namesOrder))
  
  for(i in 1:length(namesOrder)){
    
    currentName = namesOrder[i]
    subList = namesOrder[1:i]
    
    count[i] = length(subList[subList == currentName])
    
  }
  
  count
}

handle_encounters = function(ships){
  ships |> 
    filter(!is.na(ship_name)) |>
    arrange(co2_start) |> 
    nest_by(flight) |> 
    mutate(ship_rle = rle(data$ship_name) |> 
             list(),
           number_of_encounters = number_encounter(ship_rle$values) |> 
             list(),
           expanded_encounters = purrr::map2(number_of_encounters,ship_rle$lengths, rep) |> 
             unlist() |> 
             list(),
           data = data |> 
             mutate(encounter_number = expanded_encounters) |> 
             list()) |> 
    select(flight, data) |> 
    unnest(data) |> 
    group_by(flight, ship_name, type, construction_year, tonnage, scrubber, sea, encounter_number) |> 
    summarise(encounter_start = min(c(co2_start,so2_start), na.rm = T),
              encounter_end = max(c(co2_end, so2_end), na.rm = T)) |> 
    ungroup() |> 
    arrange(encounter_start)
}

c251 = tribble(
  ~ship_name,  ~encounter_number, ~encounter_start, ~encounter_end, ~type, ~construction_year, ~tonnage, ~scrubber, ~sea,
  "Indi",             1, "2021/09/24 12:59:00", "2021/09/24 13:01:00", "general cargp", 2009, 2545, TRUE, "BB",
  "Xin Tian Jin",     1, "2021/09/24 13:11:00", "2021/09/24 13:19:00", "container ship", 2003, 66433, TRUE, "BB",
  "Maersk Stratus",   1, "2021/09/24 13:19:00", "2021/09/24 13:21:00", "oil chemical tanker", 2017, 28137, FALSE, "BB",
  "Xin Tian Jin",     2, "2021/09/24 13:21:00", "2021/09/24 13:25:00", "container ship", 2003, 66433, TRUE, "BB",
  "Unity Star",       1, "2021/09/24 13:27:00", "2021/09/24 13:29:00", NA, NA, NA, NA, "BB",
  "Ardmore Defender", 1, "2021/09/24 13:39:00", "2021/09/24 13:44:00", "oil chemical tanker", 2015, 23702, TRUE, "BB",
  "MSC Lausane",      1, "2021/09/24 13:47:00", "2021/09/24 13:56:00", "container ship", 2005, 62702, FALSE, "BB",
  "CL Teresa",        1, "2021/09/24 13:56:00", "2021/09/24 13:59:00", NA, NA, NA, NA, "BB",
  "Bulker Bee 10",    1, "2021/09/24 14:04:00", "2021/09/24 14:06:00", NA, NA, NA, NA, "BB",
  "Silver London",    1, "2021/09/24 14:06:00", "2021/09/24 14:08:00", "oil chemical tanker", 2014, 29553, TRUE, "BB",
) |> 
  mutate(flight = "c251",
         campaign = "acruise2",
         encounter_start = ymd_hms(encounter_start),
         encounter_end = ymd_hms(encounter_end))


path = here::here('data','ACRUISE_all_integration_uncert_1Hz.xlsx')

ship_times = list(
  read_xlsx(path, sheet = "A1") |>
    janitor::clean_names() |>
    select(flight, co2_start, co2_end, so2_start, so2_end, ship_name = ship, type, construction_year = year, tonnage, scrubber, sea) |>
    mutate(scrubber = scrubber == "yes",
           flight = paste0("c",flight)) |>
    handle_encounters() |> 
    mutate(campaign = "acruise1"),
  read_xlsx(path, sheet = "A2") |> 
    janitor::clean_names() |> 
    select(flight, co2_start, co2_end, so2_start, so2_end, ship_name = ship, type, construction_year = year, tonnage, scrubber, sea) |> 
    mutate(scrubber = scrubber == "yes",
           flight = tolower(flight)) |> 
    handle_encounters() |> 
    mutate(campaign = "acruise2"),
  read_xlsx(path, sheet = "A3") |> 
    janitor::clean_names() |> 
    select(flight, co2_start, co2_end, so2_start, so2_end, ship_name = ship, type, construction_year = year, tonnage, scrubber, sea) |> 
    mutate(scrubber = scrubber == "yes",
           flight = tolower(flight)) |> 
    handle_encounters() |> 
    mutate(campaign = "acruise3")) |> 
  bind_rows() |> 
  mutate(encounter_start = encounter_start |> 
           force_tz("Etc/GMT-1") |> 
           with_tz("UTC"),
         encounter_end = encounter_end |> 
           force_tz("Etc/GMT-1") |> 
           with_tz("UTC")
         ) |> 
  bind_rows(c251) |> 
  arrange(encounter_start) |> 
  mutate(encounter_start = as.nanotime(encounter_start),
         encounter_end = as.nanotime(encounter_end),
         campaign = ifelse(flight == "c292", "acsis", campaign))

saveRDS(ship_times, here::here('data','ship_times.RDS'))
