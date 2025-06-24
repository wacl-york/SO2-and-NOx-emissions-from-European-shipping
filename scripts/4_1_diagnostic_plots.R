library(dplyr)
library(ggplot2)

sfc = read.csv(here::here('data','sfc.csv')) |> 
  tibble()

old_sfc = read.csv(here::here('data','sfc_old.csv')) |> 
  tibble()

sfc |> 
  filter(!is.na(area)) |>
  mutate(area = case_when(area == "EC" ~ "SECA",
                         area == "Port of Tyne" ~ "Port of Tyne",
                         area == "Port of Valencia" ~ "Port of Valencia",
                         TRUE ~ "Open Ocean")) |> 
  ggplot()+
  geom_boxplot(aes(area, average_sfc, fill = campaign))+
  scale_y_continuous(
    trans = scales::transform_pseudo_log(sigma = 0.01, base = 10),
    breaks = c(0,10^(-2:1))
  )+
  annotation_logticks(sides = "l")



sfc |> 
  filter(campaign == "acruise2", 
         area == "EC") |>
  filter(average_sfc != min(average_sfc))
