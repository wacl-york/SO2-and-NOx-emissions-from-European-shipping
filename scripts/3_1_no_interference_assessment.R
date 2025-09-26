library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

source(here::here('functions','flight_details.R'))

plumetimes = readRDS(here::here('data','sfc_per_plume.RDS')) |> 
  select(start_so2, end_so2) |> 
  filter(!is.na(start_so2)) |> 
  mutate(start_so2 = start_so2-15,
         end_so2 = end_so2+10,
         plume_idx = row_number())

dat = readRDS(here::here('data','faam_merge','acruise_merge.RDS')) |>
  select(date, flight, SO2_TECO, no_mr) |> 
  left_join(
    flight_details() |> 
      select(campaign, flight),
    "flight"
      ) |> 
  filter(campaign == "acruise1") |> 
  left_join(
    plumetimes,
    by = join_by(between(date, start_so2, end_so2))
  ) |> 
  mutate(date = as_datetime(date, tz = "UTC"),
         no_mr = no_mr/1e3) |> 
  filter(!is.na(start_so2)) |> 
  group_by(flight) |> 
  mutate(idx = row_number()) |> 
  ungroup() |> 
  nest_by(plume_idx) |> 
  mutate(
    so2_peak = min(data$idx[data$SO2_TECO == max(data$SO2_TECO)]),
    no_peak = min(data$idx[data$no_mr == max(data$no_mr)]),
    lag = so2_peak-no_peak) |> 
  filter(!is.na(lag)) |> 
  mutate(
    data = data |> 
      mutate(so2_shift = lead(SO2_TECO, lag)
      ) |> 
      list()
  ) |> 
  unnest(data) |> 
  ungroup()

g1 = dat |> 
  select(idx, flight, SO2_TECO, so2_shift, no_mr) |> 
  pivot_longer(-c(idx,flight)) |> 
  ggplot()+
  geom_line(aes(idx, value, colour = name))+
  facet_wrap(~flight, scale = "free_x")+
  theme_minimal()

clear_flights = c("c179", "c183")

g2 = dat |> 
  filter(flight %in% clear_flights) |> 
  select(idx, flight, so2_shift, no_mr) |> 
  pivot_longer(-c(idx, flight)) |> 
  ggplot()+
  geom_line(aes(idx, value, colour = name))+
  facet_wrap(~flight)+
  theme_minimal()


g3 = dat |> 
  mutate(so2_from_no = no_mr/145) |> 
  filter(flight %in% clear_flights) |> 
  select(idx, flight, so2_shift, so2_from_no) |> 
  pivot_longer(-c(idx, flight)) |> 
  ggplot()+
  geom_line(aes(idx, value, colour = name))+
  facet_wrap(~flight)+
  theme_minimal()


interf = dat |> 
  filter(flight %in% clear_flights) |> 
  group_by(plume_idx) |> 
  summarise(no_max = max(no_mr),
            no_q99 = quantile(no_mr, 0.99),
            no_q95 = quantile(no_mr, 0.95),
            so2_max = max(SO2_TECO)) |> 
  pivot_longer(contains("no"), names_to = "stat", values_to = "no") |> 
  mutate(stat = stringr::str_remove(stat, "no_")) |> 
  mutate(so2_from_no = no/145,
         perc_int = (so2_from_no/so2_max)*100
  )

interf |> 
  write.csv(here::here('no_interference','interference_perc.csv'), row.names = F)

interf |> 
  group_by(stat) |> 
  summarise(perc_int = mean(perc_int))

g4 = interf |> 
  ggplot()+
  geom_point(aes(plume_idx, perc_int, colour = stat))+
  theme_minimal()

png(here::here('no_interference','plume_shift.png'), res = 300, width = 4000, height = 3000)
print(g1)
dev.off()

png(here::here('no_interference','good_plumes.png'), res = 300, width = 4000, height = 3000)
print(g2)
dev.off()

png(here::here('no_interference','so2_from_no.png'), res = 300, width = 4000, height = 3000)
print(g3)
dev.off()

png(here::here('no_interference','interference_perc.png'), res = 300, width = 4000, height = 3000)
print(g4)
dev.off()



# Campagin Comparison -----------------------------------------------------

datAll = readRDS(here::here('data','faam_merge','acruise_merge.RDS')) |>
  select(date, flight, SO2_TECO, no_mr) |> 
  left_join(
    flight_details() |> 
      select(campaign, flight),
    "flight"
  ) |> 
  left_join(
    plumetimes,
    by = join_by(between(date, start_so2, end_so2))
  ) |> 
  mutate(date = as_datetime(date, tz = "UTC")) |> 
  filter(!is.na(start_so2)) 

compDat = datAll |> 
  group_by(campaign, plume_idx) |> 
  summarise(so2 = max(SO2_TECO)) |> 
  bind_rows(
    dat |> 
      mutate(so2_from_no = no_mr/145) |> 
      group_by(campaign, plume_idx) |> 
      summarise(so2 = max(so2_from_no)) |> 
      mutate(campaign = "acruise_1_interf")
  ) |> 
  filter(campaign != "acsis")

compDat |> 
  group_by(campaign) |> 
  summarise(so2 = mean(so2, na.rm =T))

g5 = compDat |> 
  ggplot()+
  geom_density(aes(so2, fill = campaign))+
  theme_minimal()



png(here::here('no_interference','acruise_comp.png'), res = 300, width = 4000, height = 3000)
print(g5)
dev.off()
