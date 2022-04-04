library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggrepel)
library(ggmap)
library(viridis)
library(shonarrr)


# Read --------------------------------------------------------------------


setwd("G:/My Drive/ACRUISE/ACRUISE2/GC")
voc = read.csv("ACRUISE-2_gc_ships_r1.csv") %>% 
  tibble()



# Tidy --------------------------------------------------------------------


# labels
colOrder = c("carbon_dioxide","methane","ethane","ethene","propane","propene",
             "iso_butane","n_butane","acetylene",
             "but_1_ene","iso_butene","iso_pentane",
             "n_pentane","cis_2_pentene","benzene",
             "ethylbenzene","toluene","p_xylene", "m_xylene","o_xylene") # order of VOCs (otherwise alphabetical)

vocNames = colOrder[colOrder != "carbon_dioxide"] # make CO2 free list for ratios


# tidy the VOCs
voc_long_noratio = voc %>% 
  select(-starts_with("X")) %>% # tidy weird excel column
  pivot_longer(-c(ship:file)) %>% # make long (values are measurement, uncertainty and flag)
  mutate(name = str_replace(name,  "_uncertainty","__uncertainty"), # make separator nice
         name = str_replace(name,  "_flag","__flag"),
         name = ifelse(str_detect(name, "__"), name, paste0(name, "__spec")) # give measurement a name
         ) %>% 
  separate(name, c("name","type"), "__") %>% # split into name and type of values 
  pivot_wider(names_from = "type", values_from = "value") %>% # widen the three value types (measurement, uncertainty and flag)
  rename(value = spec) %>% # rename spec to value for semantics
  mutate(value = ifelse(flag == 0, value, NA), # NA all level 2 flagged values
         ship = str_trim(ship, "both"), # remove whitespace frome ship names
         case_bottle = interaction(case,bottle), # make unique bottle ID
         name = factor(name,
                       levels = colOrder)) #order in more sensible way


# make CO2 ratios
co2_ratio = voc_long_noratio %>% 
  select(case, bottle, name, value) %>% 
  pivot_wider() %>% 
  pivot_longer(cols = all_of(vocNames)) %>% 
  mutate(co2ratio = (value/carbon_dioxide)*1000) %>% # * 1000 for prettier numbers
  select(-value) %>% 
  pivot_wider(values_from = co2ratio) %>% 
  select(-carbon_dioxide) %>% 
  pivot_longer(cols = all_of(vocNames), values_to = "co2ratio")

voc_long = left_join(voc_long_noratio, co2_ratio, by = c("case", "bottle", "name")) # put them together



# Plot --------------------------------------------------------------------


#Selected flights by ship/species (matrix style)
voc_long %>% 
  filter(flight %in% c("C263")) %>% 
  #filter(name %in% c("ethene", "propene", "acetylene", "but_1_ene", "n_pentane","cis_2_pentene")) %>% 
  #filter(ship=="background") %>% 
  ggplot()+
  geom_bar(aes(case_bottle, value, fill = name), position = "stack", stat = "identity", width=0.2)+
  scale_fill_viridis(discrete=TRUE) +
  facet_wrap(~name, scales = "free_y")+ # "name" - by species, "ship" - by ship
  labs(x="SWAS case / bottle", y="VOC content")+
  theme_minimal() +
  theme(plot.title = element_blank(),  
        text = element_text(size=12, colour="black"),
        axis.text = element_text(colour="black"),
        legend.title = element_blank(),
        strip.text = element_text(colour = 'black'),
        panel.spacing.x = unit(0,"line"),
        panel.border = element_rect(color = "grey", fill = NA, size = 1), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())


#selected flight by ship (in one line, good for background comparison)
voc_long %>% 
  filter(flight %in% c("C261")) %>% 
  filter(name != "methane" & name != "carbon_dioxide") %>% 
  ggplot()+
  geom_bar(aes(case_bottle, co2ratio, fill = name), position = "stack", stat = "identity")+
  scale_fill_viridis(discrete=TRUE) +
  facet_grid(~ship, scales = "free_x", space='free')+ #sort by "ship" or "flight"
  labs(x="SWAS case / bottle", y="VOC content (ppb)")+
  theme_minimal() +
  theme(plot.title = element_blank(),  
        text = element_text(size=14, colour="black"),
        axis.text = element_text(colour="black"),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        strip.text = element_text(colour = 'black'),
        panel.spacing.x = unit(0,"line"),
        panel.border = element_rect(color = "grey", fill = NA, size = 1), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())


#all bottles ordered by overall concentration and labeled by flight
plot_data = voc_long %>% 
  filter(ship == "background") %>% 
  filter(name != "methane" & name != "carbon_dioxide") %>% 
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



# Map -------------------------------------------------


#get the swas latlon and flight track data
swas <- read.csv("G:/My Drive/ACRUISE/ACRUISE2/SWAS_ACRUISE2/swas_bottle_coord.csv", stringsAsFactors = F)
dm <- readRDS("G:/My Drive/ACRUISE/ACRUISE2/data_raw/core_for_stats/ACRUISE-2_merged_r0.RDS")

#pick flight in flight track data
flights <- dm %>% filter(flight == 261) %>% na.omit()

#wind direction
flights <- flights %>% mutate(wd = shonarrr::calc_wind_direction(flights$U_C,flights$V_C))

#make unique bottle ID and pick flight in swas data
bottles <-  swas2 %>% 
  mutate(case_bottle = interaction(Case,Bottle)) %>% 
  rename(LON_GIN = lon_start,
         LAT_GIN = lat_start) %>%
  filter(Flight == "C261")


#make map box
bbox_cropped=c(min(flights$LON_GIN-0.1),min(flights$LAT_GIN-0.1),max(flights$LON_GIN+0.1),max(flights$LAT_GIN+0.1))
#bbox_cropped=c(-7.3, 51, -5, 51.75)

#make a map background
mymap = ggmap::get_stamenmap(bbox_cropped, zoom = 7)

#plot swas & flight tracks on the map
ggmap(mymap)+
  geom_point(data = flights, 
             aes(LON_GIN,LAT_GIN),
             size = 1,
             alpha = .7,
             colour="goldenrod2") +
  geom_point(data=bottles,
             aes(LON_GIN,LAT_GIN),
             size = 2,
             shape=4,
             stroke=2,
             colour="deeppink4") +
  geom_label_repel(data=bottles,
             aes(x=LON_GIN,
                 y=LAT_GIN,
                 label=case_bottle))+
  theme_minimal() +
  theme(axis.title = element_blank())

























# Notes -------------------------------------------------


# widening again
voc_long %>% 
  pivot_wider(names_from = "name",
              values_from = c(value,uncertainty,flag),
              names_glue = "{name}__{.value}") %>% 
  rename_all(~str_remove(.x,"__value"))


# plot all by bottle
plot_lables = voc_long %>% 
  filter(ship %in% "background") %>% 
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
  #facet_wrap(~ship, scales = "free_x")+
  labs(x="SWAS case / bottle", y="Percentage")+
  theme_minimal() +
  theme(plot.title = element_blank(),  text = element_text(size=14, colour="black"), axis.text = element_text(colour = "black"))


