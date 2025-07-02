pacman::p_load(
  tidyverse,
  janitor,
  readxl,
  shonarrr,
  ggnewscale,
  RColorBrewer
)


# LOAD ----
file <- here::here('data/fleet_comparison', "ACRUISE and global fleet_wsd.xlsx")

# ACRUISE DATA ----

# read both sheets
df_fleet_acruise <- list(
  read_xlsx(
    file, sheet = "ACRUISE_2019_fleet") %>% 
    mutate(year = 2019
    ),
  read_xlsx(
    file, sheet = "ACRUISE_2021_fleet"
  ) %>% 
    mutate(year = 2021)
) %>% 
  bind_rows() 

# tidy 
df_fleet_acruise_tidy <- df_fleet_acruise %>% 
  clean_names() %>% 
  select(-percent_sox_of_total) %>% 
  rename(
    nox_kg = n_ox_kg,
    sox_kg = s_ox_kg
  )%>% 
  filter(!is.na(imo)) %>% 
  mutate(
    year_built_bin = case_when(
      year_built < 1900 ~ "Pre 1990",
      between(year_built, 1990, 1994) ~ "1990-1994",
      between(year_built, 1995, 1999) ~ "1995-1999",
      between(year_built, 2000, 2004) ~ "2000-2004",
      between(year_built, 2005, 2009) ~ "2005-2009",
      between(year_built, 2010, 2014) ~ "2010-2014",
      between(year_built, 2015, 2019) ~ "2015-2019",
      year_built >= 2020 ~ "2020-"
    ),
    tonnage_bin_linear = case_when(
      gross_tonnage < 50000 ~ "<50000",
      between(gross_tonnage, 50000, 100000) ~ "50001 - 100000",
      between(gross_tonnage, 100001, 150000) ~ "100001 - 150000",
      between(gross_tonnage, 150001, 200000) ~ "150001 - 200000",
      between(gross_tonnage, 200001, 250000) ~ "200001 - 250000",
      gross_tonnage > 250000 ~ ">250000"
    )
  )

# summarise by bin
df_fleet_acruise_bin <-  df_fleet_acruise_tidy %>%
  select(
    contains("bin"),
    sox_kg, 
    year
    ) %>%
  pivot_longer(
    -c(sox_kg,year),
    names_to = "bin_type",
    values_to = "bin"
  ) %>%
  group_by(bin, bin_type, year) %>%
  summarise_all(sum, na.rm = T) %>%
  group_by(year, bin_type) %>%
  mutate(
    percent = sox_kg/sum(sox_kg)*100,
    type = "ACRUISE"
  ) 

# GLOBAL DATA ----

# global fleet
df_fleet_global = list(
  read_xlsx(
    file, sheet = "global_fleet_build"
    ) %>% 
    pivot_longer(
      -`Buld year`,
      names_to = "year",
      values_to = "sox_kg" 
    ) %>% 
    rename(bin = `Buld year`) %>% 
    mutate(
      bin = case_when(
        bin == "pre1990" ~ "Pre 1990",
        T ~ bin
      ),
      bin_type = "year_built_bin"
    ),
  read_xlsx(
    file, sheet = "global_fleet_tonne")  %>% 
    pivot_longer(
      -DWT, names_to = "year",
      values_to = "sox_kg"
    ) %>% 
    rename(bin = DWT) %>% 
    mutate(
      bin_type = "tonnage_bin_linear",
      bin = case_when(
        bin == "100000" ~ "50001 - 100000",
        bin == "150000" ~ "100001 - 150000",
        bin == "200000" ~ "150001 - 200000",
        bin == "250000" ~ "200001 - 250000",
        TRUE ~ bin
      )
    )
) %>% 
  bind_rows() %>% 
  mutate(type = "Global") %>% 
  group_by(year, bin_type) %>% 
  mutate(
    percent = sox_kg/sum(sox_kg)*100,
    year = as.numeric(year)
    ) %>% 
  filter(bin != "Tot")


# COMBINE ----
df_fleet_all <-  bind_rows(
  df_fleet_global,
  df_fleet_acruise_bin
)


# PLOT ----

# tonnage ----
df_fleet_all %>% 
  filter(
    bin_type == "tonnage_bin_linear"
  ) %>% 
  mutate()%>% 
  mutate(
    bin = factor(
      bin, levels = c("<50000",
                      "50001 - 100000",
                      "100001 - 150000",
                      "150001 - 200000",
                      "200001 - 250000",
                      ">250000")
    )
  ) %>% 
  ggplot()+
  geom_bar(
    aes(type, percent, fill = forcats::fct_rev(bin)), 
    position = "stack", 
    stat = "identity"
  )+
  facet_grid(
    year~bin_type
  )+
  scale_y_continuous(
    expand = c(0,0), 
    name = "Percentage / %"
    ) +
  scale_fill_brewer(
    palette = "Reds",
    name = "Tonnage", 
    direction = -1, 
    guide = guide_legend(reverse = T)
  ) +
  theme(
    strip.text.x = element_blank(),
   # strip.text.y = element_blank()
  )


# year built ----
df_fleet_all %>% 
  filter(
    bin_type == "year_built_bin"
  ) %>% 
  mutate(
    bin = str_replace(bin, "-", " - "),
    bin = str_replace(bin, "pre", "pre "),
    bin = factor(
      bin,
      levels = c("Pre 1990",
                "1990 - 1994",
                "1995 - 1999",
                "2000 - 2004",
                "2005 - 2009",
                "2010 - 2014",
                "2015 - 2019",
                "2020 - ")
    )
  ) %>% 
  ggplot() +
  geom_bar(
    aes(type, percent, fill = forcats::fct_rev(bin)), 
    position = "stack", stat = "identity"
  ) +
  facet_grid(year~bin_type) +
  scale_y_continuous(
    expand = c(0,0), 
    name = ""
  ) +
  scale_fill_brewer(
    palette = "Blues", "Year Built", direction = -1, 
    guide = guide_legend(reverse = T) 
  ) +
  theme(
    strip.text.x = element_blank(),
    axis.text.y = element_blank()
  )


# PLOT TOGETHER ----
df_fleet_all_order <- df_fleet_all %>% 
  ungroup() %>% 
  nest_by(
    bin_type
  ) %>% 
  mutate(
    data = list(
      complete(data, bin, type, year)
    )
  ) %>% 
  unnest(data) %>% 
  mutate(
    percent = replace_na(percent, 0),
    bin = factor(
      bin,
      levels = c(
        "<50000",
        "50001 - 100000",
        "100001 - 150000",
        "150001 - 200000",
        "200001 - 250000",
        ">250000",
        "Pre 1990",
        "1990-1994",
        "1995-1999",
        "2000-2004",
        "2005-2009",
        "2010-2014",
        "2015-2019",
        "2020-"
      ) 
    )
  ) 

# plot
df_fleet_all_order %>% 
 # filter(!bin == ">250000") %>% 
  ungroup() %>% 
  ggplot() +
  geom_bar(
    data = . %>% 
      filter(
        bin_type == "year_built_bin"
      ),
    aes(type, percent, fill = (bin)), 
    position = "stack", 
    stat = "identity"
  ) +
  scale_fill_manual(
    values = brewer.pal(8, "YlGnBu"),
    name = "Year Built", 
    guide = guide_legend(order = 2)
  ) +
  new_scale_fill() +
  geom_bar(
    data = . %>% 
      filter(
        bin_type == "tonnage_bin_linear"
      ),
    aes(type, percent, fill = (bin)), 
    position = "stack", 
    stat = "identity"
  ) +
  scale_fill_manual(
    values = brewer.pal(6, "YlOrRd"),
       name = "Gross Tonnage", 
       guide = guide_legend(order = 1)
  ) +
  facet_grid(
    bin_type~year
  ) +
  scale_y_continuous(
    label = paste_symbol,
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_x_discrete(
    expand = expansion(add = 0.5)
  ) +
  labs(
    x = "Fleet",
    y = expression(Percentage~of~SO[2]~Emissions)
  ) +
#  coord_cartesian(expand = F) +
  theme_shona_white_journal() +
  theme(
    strip.text.y = element_blank(),
   # legend.spacing.y = unit(1.0, 'cm')
   panel.grid = element_blank()
  )


# save - png
ggsave(
  here::here('plots','fig07_fleet_comparison.png'), 
  width = 6,
  height = 5
)

# save - pdf
ggsave(
  here::here('plots','fig07_fleet_comparison.pdf'), 
  width = 6,
  height = 5
)


# SAVE ----

# export data
write_csv(
  df_fleet_all_order %>% 
    arrange(
      bin_type,
      year,
      type,
      desc(bin)
    ),
  here::here('data/figure_data','fig07_fleet_comparison.csv'), 
)

