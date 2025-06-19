library(here)
library(dplyr)
library(tidyr)
library(nanotime)

ship_times = readRDS(here::here('data','ship_times.RDS'))

plumes = readRDS(here::here('data','integration.RDS'))

test = plumes |> 
  mutate(area_so2 = area_so2 |>
           rename_with(\(x) paste0(x, "_so2")) |> 
           list(),
         area_co2 = area_co2 |>
           rename_with(\(x) paste0(x, "_co2")) |> 
           list(),
         combined_areas = full_join(
           area_co2,
           area_so2,
           by = join_by(
             overlaps(start_co2, 
                      end_co2, 
                      start_so2, 
                      end_so2))) |> 
           list()
  ) |> 
  select(flight, campaign, combined_areas) |> 
  unnest(combined_areas) |> 
  ungroup() |> 
  mutate(
    area_so2 = ifelse(is.na(area_so2), 0, area_so2),
    sfc = (area_so2/area_co2) * 0.232) |> 
  filter(!is.na(area_co2)) |> 
  rowwise() |>
  mutate(start_plume = min(start_co2, start_so2, na.rm = T),
         end_plume = max(end_co2, end_so2, na.rm = T)) |> 
  ungroup() |> 
  full_join(ship_times, by = join_by(flight, campaign, overlaps(start_plume, end_plume, encounter_start, encounter_end)))

test |> 
  filter(sfc != 0) |> 
  group_by(campaign) |> 
  summarise(sfc_min = min(sfc, na.rm = T))



test |> 
  filter(flight == "c251") |> 
  View()

test$sfc |> plot()
