library(here)
library(dplyr)
library(tidyr)
library(nanotime)

ship_times = readRDS(here::here('data','ship_times.RDS'))

plumes = readRDS(here::here('data','integration.RDS'))

sfc_per_plume = plumes |> 
  mutate(area_so2 = area_so2 |>
           rename_with(\(x) paste0(x, "_so2")) |>
           mutate(start_so2 = start_so2-(10*1e9),
                  end_so2 = end_so2+(10*1e9)) |>
           list(),
         area_co2 = area_co2 |>
           rename_with(\(x) paste0(x, "_co2")) |>
           list(),
         combined_areas = full_join(
           area_co2,
           area_so2,
           by = join_by(
             overlaps(start_co2, # join the so2 and co2 plumes where there start and end times overlap
                      end_co2,
                      start_so2, 
                      end_so2))) |> 
           list()
  ) |> 
  select(flight, campaign, combined_areas) |> 
  unnest(combined_areas) |> 
  ungroup() |> 
  mutate(
    area_so2 = ifelse(is.na(area_so2), 0, area_so2), # if there is a CO2 plume but no SO2 plume, set so2 area to 0
    sfc = (area_so2/area_co2) * 0.232,
    uncertainty_sfc = sqrt((uncertainty_so2/area_so2)^2 + (uncertainty_co2/area_co2)^2)*sfc # sigma(z) = sqrt((sigma(x)/x)^2 + (sigma(y)/y)^2) * z
    ) |> # calculate sfc %. [SO2 (ppb)] / [CO2 (ppm)] * 0.232 == SFC % 
  rowwise() |>
  mutate(start_plume = min(start_co2, start_so2, na.rm = T), # make the start and end time of the plume irrespective of species
         end_plume = max(end_co2, end_so2, na.rm = T)) |>    # plume is then the widest possible to line up with ship encounters
  ungroup() |> 
  full_join(ship_times, 
            by = join_by( # join on ships when encounter time overlaps with plume time
              flight, 
              campaign, 
              overlaps(start_plume, 
                       end_plume, 
                       encounter_start, 
                       encounter_end))) |> 
  filter(!is.na(area_co2),
         area_co2 != 0, # if there is no CO2 plume, remove the observation
         !is.na(ship_name)) # if we can't assing the ship, remove the observation

sfc_minima = sfc_per_plume |> 
  filter(sfc != 0) |> 
  mutate(campaign = ifelse(campaign == "acsis", "acruise3", campaign)) |>  # for the sake of the LODS, include acsis with acruise3
  group_by(campaign) |> 
  filter(sfc == min(sfc)) |> 
  select(campaign, sfc_min = sfc, uncertainty_sfc_min = uncertainty_sfc) |> 
  mutate(sfc_min = sfc_min/2)

# restore acsis for future joining
minima_acsis = sfc_minima |> 
  filter(campaign == "acruise3") |> 
  mutate(campaign = "acsis")

sfc_minima = sfc_minima |> 
  bind_rows(minima_acsis)


sfc_per_plume = sfc_per_plume |> 
  left_join(sfc_minima, by = "campaign") |> 
  mutate(
    uncertainty_sfc = ifelse(sfc == 0, uncertainty_sfc_min, uncertainty_sfc),
    sfc = ifelse(sfc == 0, sfc_min, sfc)
  ) |> 
  select(-sfc_min, -uncertainty_sfc_min)

sfc_per_ship = sfc_per_plume |> 
  group_by(flight, campaign, ship_name, type, construction_year, tonnage, scrubber, sea) |> 
  summarise(sfc = mean(sfc, na.rm = T),
            uncertainty_sfc = sqrt(sum(uncertainty_sfc, na.rm = T))
            ) |> 
  ungroup()


saveRDS(sfc_per_plume, here::here('data','sfc_per_plume.RDS'))
saveRDS(sfc_per_ship, here::here('data','sfc_per_ship.RDS'))
