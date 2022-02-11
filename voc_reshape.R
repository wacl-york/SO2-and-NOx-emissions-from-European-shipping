library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(viridis)


# Read --------------------------------------------------------------------

setwd("G:/My Drive/ACRUISE/ACRUISE2/GC")
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


#all ships by ship (no background)
voc_long %>% 
  filter(!is.na(Ship), !Ship=="background") %>% 
  ggplot()+
  geom_bar(aes(case_bottle, value, fill = name), position = "fill", stat = "identity", width=0.2)+
  scale_fill_viridis(discrete=TRUE) +
  facet_wrap(~Ship, scales = "free_x", ncol=10)+
  labs(x="SWAS case / bottle", y="VOC content")+
  theme_minimal() +
  theme(plot.title = element_blank(),  
        text = element_text(size=14, colour="white"),
        #axis.text = element_text(colour="black"),
        axis.text = element_blank(),
        legend.title = element_blank(),
        strip.text = element_text(colour = 'white'),
        panel.spacing.x = unit(0,"line"),
        panel.border = element_rect(color = "grey", fill = NA, size = 1), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())


#all ships better



#specific ship/flight etc
voc_long %>% 
  filter(flight %in% c("C256")) %>% 
  filter(Ship %in% c("Anthem of the Seas", "background")) %>% 
  ggplot()+
  geom_bar(aes(case_bottle, value, fill = name), position = "stack", stat = "identity")+
  scale_fill_viridis(discrete=TRUE) +
  facet_grid(~Ship, scales = "free_x", space='free')+
  labs(x="SWAS case / bottle", y="VOC content (ppb)")+
  theme_minimal() +
  theme(plot.title = element_blank(),  
        text = element_text(size=14, colour="white"),
        axis.text = element_text(colour="white"),
        axis.text.x = element_blank(),
        legend.title = element_blank(),
        strip.text = element_text(colour = 'white'),
        panel.spacing.x = unit(0,"line"),
        panel.border = element_rect(color = "grey", fill = NA, size = 1), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())



#all by bottle
plot_lables = voc_long %>% 
  filter(Ship %in% "background") %>% 
  select(case_bottle, flight) %>% 
  mutate(y = -0.1) %>% 
  distinct()

voc_long %>% 
  filter(flight %in% c("background")) %>% 
  ggplot()+
  geom_bar(aes(case_bottle, value, fill = name), position = "stack", stat = "identity")+
  geom_text(data = plot_lables,
            aes(case_bottle, y, label=flight))+
  scale_fill_viridis(discrete=TRUE) +
  #facet_wrap(~Ship, scales = "free_x")+
  labs(x="SWAS case / bottle", y="Percentage")+
  theme_minimal() +
  theme(plot.title = element_blank(),  text = element_text(size=14, colour="black"), axis.text = element_text(colour = "black"))


#all divided into flights
voc_long %>% 
  filter(Ship %in% c("background"))  %>% 
  ggplot()+
  geom_bar(aes(case_bottle, value, fill = name), position = "stack", stat = "identity")+
  # geom_text(data = plot_lables,
  #           aes(case_bottle, y, label=flight))+
  scale_fill_viridis(discrete=TRUE) +
  facet_grid(~flight, scales = "free_x", space='free')+
  labs(x="SWAS case / bottle", y="VOC content (ppb)")+
  theme_minimal() +
  theme(plot.title = element_blank(),  
        text = element_text(size=14, colour="white"),
        axis.text = element_text(colour="white"),
        axis.text.x = element_blank(),
        legend.title = element_blank(),
        strip.text = element_text(colour = 'white'),
        panel.spacing.x = unit(0,"line"),
        panel.border = element_rect(color = "grey", fill = NA, size = 1), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())


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

