library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggrepel)
library(ggmap)
library(viridis)
library(shonarrr)

#compile me please
calc_background = function(dat){
  background = dat %>% 
    filter(ship == "background") %>% 
    select(name, 
           background = value,
           uncert_b = uncertainty) %>% 
    group_by(name) %>% 
    summarise_all(mean, na.rm = T)
  
  
  ships = dat %>% 
    filter(ship != "background") %>% 
    left_join(background, by = "name") %>% 
    mutate(enhancement = value-background,
           uncert_e = uncertainty + uncert_b)
  
  #
  ships
  
}




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

colNames <-  labeller(name =
           c("carbon_dioxide" = "carbon dioxide",
             "iso_butane" = "iso-butane",
             "n_butane" = "n-butane",
             "but_1_ene" = "but-1-ene",
             "iso_butene" = "iso-butene",
             "iso_pentane" = "iso-penetane",
             "n_pentane" = "n-pentane",
             "cis_2_pentene" = "cis-2-pentene",
             "p_xylene" = "p-xylene", 
             "m_xylene" = "m-xylene",
             "o_xylene" = "o-xylene",
             "methane" = "methane",
             "ethane" = "ethane",
             "ethene" = "ethene",
             "propane" = "propane",
             "propene" = "propene",
             "acetylene" = "acetylene",
             "ethylbenzene" = "ethylbenzene",
             "toluene" = "toluene"))
         
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


# # make toluene ratios (change line 49)
# toluene_ratio = voc_long_noratio %>% 
#   select(case, bottle, name, value) %>% 
#   pivot_wider() %>% 
#   pivot_longer(cols = all_of(vocNames)) %>% 
#   mutate(tolueneratio = (value/toluene)/50) %>% # 50 for prettier numbers
#   select(-value) %>% 
#   pivot_wider(values_from = tolueneratio) %>% 
#   select(-toluene) %>% 
#   pivot_longer(cols = all_of(vocNames), values_to = "tolueneratio")
# 
# voc_toluene = left_join(voc_long_noratio, toluene_ratio, by = c("case", "bottle", "name")) # put them together






# #subtract backgrounds
temp = voc_long %>%
  select(case_bottle, name, value, ship, uncertainty, co2ratio) %>%
  mutate(grp = case_when(
    # case_bottle %in% c("7.9", "7.14", "7.11", "7.12", "7.13", "7.10") ~ 1,
    #                      case_bottle %in% c("7.7", "7.6", "102.06", "102.07", "102.03") ~ 2,
    #                      case_bottle %in% c("7.04", "102.02", "102.05", "7.05", "102.04", "102.01") ~ 3,
                         # case_bottle %in% c("7.02", "6.16", "6.15", "7.03", "7.01", "101.8", "101.07", "101.6", "6.14", "101.05", "6.13", "101.4", "101.3") ~ 4,
      case_bottle %in% c("6.5", "6.10", "6.11", 
                         "101.1","101.2","6.4","6.6","6.7","6.8","6.9") ~ 5,
                         # case_bottle %in% c("4.15", "4.16", "4.10", "4.12", "4.13", "4.14", "4.11") ~ 6,
                         # case_bottle %in% c("3.15", "3.16", "4.1", "4.2", "4.3", "4.4", "4.5", "4.6", "4.7", "4.8", "4.9") ~ 7,
                         # case_bottle %in% c("3.1", "3.11") ~ 8,
                         # case_bottle %in% c("3.13", "3.12", "3.14") ~ 9,
                         # case_bottle %in% c("2.10", "2.09", "2.11", "2.12", "2.13") ~ 10,
                         # case_bottle %in% c("3.01", "2.14", "2.15", "2.16") ~ 11,
                         # case_bottle %in% c("3.05", "3.02", "3.03", "3.04", "3.06", "3.07", "3.08", "3.09") ~ 12,
                         # case_bottle %in% c() ~ 13,
                         # case_bottle %in% c() ~ 14,
                         # case_bottle %in% c() ~ 15,
                         # case_bottle %in% c() ~ 16,
                         # case_bottle %in% c() ~ 17,
                         # case_bottle %in% c() ~ 18,
                         # case_bottle %in% c() ~ 19,
                         # case_bottle %in% c() ~ 20,
                         TRUE ~ NA_real_)) %>%
  filter(!is.na(grp)) %>%
  split(., f = .$grp) %>%
  map_df(calc_background)


temp %>% 
  ggplot()+
  geom_bar(aes(case_bottle, enhancement, fill = name), stat = "identity", position = "stack")


positions <- c("101.1","101.2","6.6","6.7","6.8","6.9","6.4")



temp %>% 
  pivot_longer(c(value, background, enhancement), names_to = "type")  %>% 
  mutate(name = factor(name,
                       levels = colOrder)) %>%
  filter(type=="enhancement") %>% 
  filter(name!="ethylbenzene")%>% 
  ggplot()+
  geom_bar(aes(case_bottle, value, fill = name), stat = "identity", position = "stack")+
  geom_errorbar(aes(ymin=value-uncert_e, ymax=value+uncert_e,
                    x=case_bottle),
                width=.2,
                position=position_dodge(.9))+
  scale_x_discrete(limits = positions)+
  scale_fill_viridis(discrete=TRUE) +
  theme_bw()+
  theme(text = element_text(size=14), legend.title = element_blank())+
  facet_wrap(~name, 
             scales = "free_y",
             labeller = colNames)+
  labs(x= "Case.bottle", y="Enhancement (ppb)")+
  guides(fill="none")






# Plot --------------------------------------------------------------------


# selected flights by ship/species (matrix style)
voc_long %>% 
  filter(flight %in% c("C262")) %>% 
  filter(name %in% c("ethene", "propene", "acetylene", "but_1_ene", "n_pentane","cis_2_pentene")) %>% 
  filter(ship!="Gwn 2") %>% 
  ggplot()+
  scale_fill_viridis(discrete=TRUE) +
  geom_bar(aes(case_bottle, value, fill = name), position = "stack", stat = "identity", width=0.2)+
  scale_color_identity()+
  geom_point(aes(case_bottle, 0, colour=ifelse(ship=="background", "cornflowerblue", "darkorange1"), size=3))+
  facet_wrap(~name, scales = "free_y")+ # "name" - by species, "ship" - by ship
  labs(x="SWAS case / bottle", y="VOC content (ppb)")+
  theme_minimal() +
  theme(plot.title = element_blank(),  
        text = element_text(size=12, colour="black"),
        axis.text = element_text(colour="black"),
        legend.title = element_blank(),
        strip.text = element_text(colour = 'black'),
        panel.spacing.x = unit(0,"line"),
        panel.border = element_rect(color = "grey", fill = NA, size = 1), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  guides(size="none", fill="none")

###################################################################

# selected flight by ship (in one line, good for background comparison)
voc_long %>% 
  filter(flight %in% c("C262")) %>% 
  filter(name != "methane" & name != "carbon_dioxide") %>% 
  ggplot()+
  geom_bar(aes(case_bottle, value, fill = name), position = "stack", stat = "identity")+
  scale_fill_viridis(discrete=TRUE) +
  facet_grid(~ship, 
             scales = "free_x", 
             space='free', 
             labeller = labeller(ship = label_wrap_gen(10)) # make the ship names behave
             )+ #sort by "ship" or "flight"
  labs(x="SWAS case / bottle", y="VOC content (ppb)")+
  theme_minimal() +
  theme(plot.title = element_blank(),  
        text = element_text(size=10, colour="black"),
        axis.text = element_text(colour="black"),
        #axis.text.y = element_blank(),
        legend.title = element_blank(),
        strip.text = element_text(colour = 'black'),
        panel.spacing.x = unit(0,"line"),
        panel.border = element_rect(color = "grey", fill = NA, size = 1), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())


# selected flight by ship - CO2 and CH4 (in one line, good for background comparison)
voc_long %>% 
  filter(flight %in% c("C262")) %>% 
   filter(name == "methane") %>% 
  #filter(name == "carbon_dioxide") %>% 
  ggplot()+
   geom_bar(aes(case_bottle, value-1800), position = "stack", stat = "identity", fill="darkgreen")+
  #geom_bar(aes(case_bottle, value-390), position = "stack", stat = "identity", fill="darkblue")+
  scale_fill_viridis(discrete=TRUE) +
  facet_grid(~ship, 
             scales = "free_x", 
             space='free', 
             labeller = labeller(ship = label_wrap_gen(10)) # make the ship names behave
  )+ #sort by "ship" or "flight"
   labs(x="SWAS case / bottle", y="CH4 content (ppb - 1800)")+
  #labs(x="SWAS case / bottle", y="CO2 content (ppm - 390)")+
  theme_minimal() +
  theme(plot.title = element_blank(),  
        text = element_text(size=10, colour="black"),
        axis.text = element_text(colour="black"),
        #axis.text.y = element_blank(),
        legend.title = element_blank(),
        strip.text = element_text(colour = 'black'),
        panel.spacing.x = unit(0,"line"),
        panel.border = element_rect(color = "grey", fill = NA, size = 1), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  guides(scale="none")


###################################################################


# all bottles ordered by overall concentration and labeled by flight
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



# all bottles benzene/toluene ratio
voc_toluene %>% 
  filter(flight %in% c("C255")) %>% 
  filter(name == "toluene" | name == "benzene") %>% 
  ggplot()+
  geom_bar(aes(case_bottle, value, fill = name), position = "stack", stat = "identity")+
  geom_point(aes(case_bottle, tolueneratio), colour="white", fill="blue", size=5, pch=21)+
  scale_fill_viridis(discrete=TRUE) +
  facet_grid(~ship, 
             scales = "free_x", 
             space='free', 
             labeller = labeller(ship = label_wrap_gen(10)) # make the ship names behave
  )+ #sort by "ship" or "flight"
  labs(x="SWAS case / bottle", y="Benzene/toluene content (ppb) and ratio (/50)")+
  theme_minimal() +
  theme(plot.title = element_blank(),  
        text = element_text(size=10, colour="black"),
        axis.text = element_text(colour="black"),
        #axis.text.y = element_blank(),
        legend.title = element_blank(),
        strip.text = element_text(colour = 'black'),
        panel.spacing.x = unit(0,"line"),
        panel.border = element_rect(color = "grey", fill = NA, size = 1), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
                                                                                                                                      












# Map -------------------------------------------------


# get the swas latlon and flight track data
swas <- read.csv("G:/My Drive/ACRUISE/ACRUISE2/SWAS_ACRUISE2/swas_all_logs_r1.csv") %>% 
  select(-c("start","end")) %>%
  mutate(date=dmy_hms(date))
  
dm <- readRDS("G:/My Drive/ACRUISE/ACRUISE2/data_raw/core_for_stats/ACRUISE-2_merged_r0.RDS")

#pick flight
fn <- 261

# pick flight in flight track data
flights <- dm %>% filter(flight == fn) %>% na.omit()

# wind direction
flights$dirs = 180 + 180 * atan2(flights$V_C,flights$U_C) / pi


# make unique bottle ID and pick flight in swas data
bottles <-  swas %>% 
  filter(flight == fn) %>%
  mutate(bgrd_flag = ifelse(Ship=="background", 1, 0),
         bgrd_flag = ifelse(is.na(bgrd_flag),0,bgrd_flag))


# make map box
#bbox_cropped=c(min(flights$LON_GIN-0.1),min(flights$LAT_GIN-0.1),max(flights$LON_GIN+0.1),max(flights$LAT_GIN+0.1))
# bbox_cropped=c(-7.8,46.7,-5.2,48.5) #264
# bbox_cropped=c(-2.2, 49.8, 0, 50.7) #263
# bbox_cropped=c(-8,46.8,-5.5,48.5) #262
# bbox_cropped=c(-7.3,50.4,-5,51.3) #261
# bbox_cropped=c(-7.5,46.7,-5.5,49.1) #259
# bbox_cropped=c(-7.5,47,-5.5,49) #257
# bbox_cropped=c(-8,49.2,-4,50.3) #256
# bbox_cropped=c(-1.5,50,1.1,50.8) #255
# bbox_cropped=c(-7.5,47,-5.5,48.5) #254
# bbox_cropped=c(-2,50,0.75,50.7) #253
#bbox_cropped=c(-7.2,47.6,-5.8,48.3) #251

# make a map background
mymap = ggmap::get_stamenmap(bbox_cropped, zoom = 7)

# plot swas & flight tracks on the map
ggmap(mymap)+
  geom_point(data = flights, 
             aes(LON_GIN,
                 LAT_GIN, 
                 colour=dirs),
             size = 1,
             alpha = .7) +
  # geom_point(data=bottles,
  #            aes(LON_GIN,LAT_GIN),
  #            size = 2,
  #            shape=4,
  #            stroke=2,
  #            colour="deeppink4") +
  # geom_label_repel(data=bottles,
  #            aes(x=LON_GIN,
  #                y=LAT_GIN,
  #                label=bottle_id,
  #                colour=ifelse(bgrd_flag==1, "firebrick", "black")))+
  # scale_color_identity()+
  scale_colour_viridis()+
  theme_minimal() +
  theme(axis.title = element_blank())

#























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


