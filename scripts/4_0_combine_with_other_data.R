library(dplyr)
library(readxl)
library(janitor)

sfc_per_ship = readRDS(here::here('data','sfc_per_ship.RDS')) |>
  rename(name = ship_name,
         year_built = construction_year, 
         average_sfc = sfc, 
         sfc_error = uncertainty_sfc,
         area = sea)

otherSFC = read_excel(here::here('data','all ships new jul 2025.xlsx'), "master") |> 
  clean_names() |> 
  mutate(
    across(all_of(c("average_sfc_9","error_10","sfc_model","average_sfc_18","error_19","n_ox_co2","error_12", "n_ox_co2_model")), \(x) as.numeric(x)),
    across(all_of(c("average_sfc_9","error_10","sfc_model","average_sfc_18","error_19")), \(x) x*100),
    campaign = case_when(
      campaign == "ACRUISE-1" ~ "acruise1",
      campaign == "ACRUISE-2" ~ "acruise2",
      campaign == "ACRUISE-3" ~ "acruise3",
      campaign == "ACSIS" ~ "acsis",
      TRUE ~ campaign),
    flight = tolower(flight)
    )

waspSFC = otherSFC |> 
  filter(campaign == "WASP") |> 
  select(flight, campaign, name, type, year_built, tonnage, area, average_sfc = average_sfc_9, sfc_error = error_10, nox_co2 = n_ox_co2, nox_co2_error = error_12) 

c251_model = tribble(
  ~flight, ~campaign, ~name, ~sfc_model, ~nox_co2_model,
  "c251","acruise2","Ardmore Defender", 0.416,  0.013038256,
  "c251","acruise2","Indi",             0.415,  0.020510597,
  "c251","acruise2","MSC Lausanne",     0.197,  0.0000549,
  "c251","acruise2","Maersk Stratus",   0.484,  0.02067,
  "c251","acruise2","Silver London",    0.129,  0.020676217,
  "c251","acruise2","Xin Tian Jin",     0.1389, 0.020630546
)

nox_model = otherSFC |> 
  select(flight, campaign, name, nox_co2 = n_ox_co2, nox_co2_error = error_12, sfc_model, nox_co2_model = n_ox_co2_model) |>
  mutate(nox_co2 = as.numeric(nox_co2),
         nox_co2_error = as.numeric(nox_co2_error),
         sfc_model = as.numeric(sfc_model),
         nox_co2_model = as.numeric(nox_co2_model)) |> 
  bind_rows(c251_model)

old_sfc = otherSFC |> 
  select(flight, campaign, name, average_sfc_old = average_sfc_18, sfc_error_old = error_19)


sfc = sfc_per_ship |> 
  left_join(nox_model, by = c("flight", "campaign", "name")) |> 
  bind_rows(waspSFC)


write.csv(sfc, here::here('data','sfc.csv'), row.names = F)
write.csv(old_sfc, here::here('data','sfc_old.csv'), row.names = F)
