library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(viridis)


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
  scale_fill_viridis(discrete=TRUE) +
  facet_wrap(~Ship, scales = "free_x")+
  labs(x="SWAS case / bottle", y="Percentage")+
  theme_minimal() +
  theme(plot.title = element_blank(),  text = element_text(size=14, colour="black"), axis.text = element_text(colour = "black"))

voc_long %>% 
  filter(Ship %in% c("background")) %>% 
  ggplot()+
  geom_bar(aes(case_bottle, value, fill = name), position = "stack", stat = "identity")+
  scale_fill_viridis(discrete=TRUE) +
  facet_wrap(~Ship, scales = "free_x")+
  labs(x="SWAS case / bottle", y="Percentage")+
  theme_minimal() +
  theme(plot.title = element_blank(),  text = element_text(size=14, colour="black"), axis.text = element_text(colour = "black"))













#all by bottle
plot_lables = voc_long %>% 
  filter(Ship %in% "background") %>% 
  select(case_bottle, flight) %>% 
  mutate(y = -0.1) %>% 
  distinct()

voc_long %>% 
  filter(Ship %in% c("background")) %>% 
  ggplot()+
  geom_bar(aes(case_bottle, value, fill = name), position = "stack", stat = "identity")+
  geom_text(data = plot_lables,
            aes(case_bottle, y, label=flight))+
  scale_fill_viridis(discrete=TRUE) +
  facet_wrap(~Ship, scales = "free_x")+
  labs(x="SWAS case / bottle", y="Percentage")+
  theme_minimal() +
  theme(plot.title = element_blank(),  text = element_text(size=14, colour="black"), axis.text = element_text(colour = "black"))


#all divided into flights
voc_long %>% 
  filter(Ship %in% c("background")) %>% 
  ggplot()+
  geom_bar(aes(case_bottle, value, fill = name), position = "stack", stat = "identity")+
  # geom_text(data = plot_lables,
  #           aes(case_bottle, y, label=flight))+
  scale_fill_viridis(discrete=TRUE) +
  facet_wrap(~flight, scales = "free_x")+
  labs(x="SWAS case / bottle", y="Percentage")+
  theme_minimal() +
  theme(plot.title = element_blank(),  text = element_text(size=14, colour="black"), axis.text = element_text(colour = "black"))

#all ordered
plot_data = voc_long %>% 
  filter(Ship == "background") %>% 
  nest_by(case_bottle) %>% 
  mutate(s = sum(data$value, na.rm = T),
         case_bottle = as.character(case_bottle)) %>% 
  arrange(desc(s)) %>% 
  mutate(case_bottle = factor(case_bottle, unique(case_bottle))) %>%
  unnest(data)

plot_lables = plot_data %>% 
  select(case_bottle, flight) %>% 
  distinct()

plot_data %>% 
  ggplot()+
  geom_bar(aes(case_bottle, value, fill = name), position = "stack", stat = "identity")+
  geom_text(data = plot_lables,
            aes(case_bottle, -0.1, label = flight))


















# Notes on widening again -------------------------------------------------

voc_long %>% 
  pivot_wider(names_from = "name",
              values_from = c(value,uncertainty,flag),
              names_glue = "{name}__{.value}") %>% 
  rename_all(~str_remove(.x,"__value"))

