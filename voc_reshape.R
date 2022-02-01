library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)


# Read --------------------------------------------------------------------

voc = read.csv("ACRUISE-2_gc_ships_r0.csv") %>% 
  tibble()


# Tidy --------------------------------------------------------------------


voc_long = voc %>% 
  select(-starts_with("X")) %>% # tidy weird excel column
  pivot_longer(-c(Ship:file)) %>% # make long (values are measurement, uncertainty and flag)
  mutate(name = str_replace(name,  "_uncertainty","__uncertainty"), # make separator nice
         name = str_replace(name,  "_flag","__flag"),
         name = ifelse(str_detect(name, "__"), name, paste0(name, "__spec")) # give measurement a name
         ) %>% 
  separate(name, c("name","type"), "__") %>% # split into name and type of values 
  pivot_wider(names_from = "type", values_from = "value") %>% # widen the three value types (measurement, uncertainty and flag)
  rename(value = spec) %>% # rename spec to value for semantics
  mutate(value = ifelse(flag == 0, value, NA), # NA all level 2 flagged values
         Ship = str_trim(Ship, "both"),
         case_bottle = interaction(case,bottle)) # remove whitespace frome Ship names


# plot --------------------------------------------------------------------

voc_long %>% 
  ggplot()+
  geom_bar(aes(case_bottle, value, fill = name), position = "stack", stat = "identity")+
  facet_wrap(~Ship, scales = "free_x")

voc_long %>% 
  filter(!is.na(Ship)) %>% 
  ggplot()+
  geom_bar(aes(case_bottle, value, fill = name), position = "stack", stat = "identity")+
  facet_wrap(~Ship, scales = "free_x")

voc_long %>% 
  filter(Ship %in% c("MSC Branka", "MSC La Spezia")) %>% 
  ggplot()+
  geom_bar(aes(case_bottle, value, fill = name), position = "stack", stat = "identity")+
  facet_wrap(~Ship, scales = "free_x")

# Notes on widening again -------------------------------------------------

voc_long %>% 
   %>% 
  pivot_wider(names_from = "name",
              values_from = c(value,uncertainty,flag),
              names_glue = "{name}__{.value}") %>% 
  rename_all(~str_remove(.x,"__value"))

