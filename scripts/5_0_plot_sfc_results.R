pacman::p_load(
  tidyverse,
  shonarrr,
  janitor,
  geomtextpath,
  ggh4x
)

# LOAD ----

# SFC calculated results
df_results <- read_csv(
  here::here('data','sfc.csv')
)

# flight look-up table
df_flights <- read_csv(
  here::here('data/meta_data/','acruise_flights.csv')
) %>% 
  mutate(
    year = year(date),
    flight_no = str_to_lower(flight_no),
    campaign = str_to_lower(campaign) %>% 
      str_remove("-")
  ) 

# location look-up table
df_locations <- read_csv(
  here::here('data/meta_data/','ship_locations.csv')
) 



# TIDY DATA ----

# join flight information and location names
df_results_join <- df_results %>%
  rename(
    location = area,
    sfc = average_sfc
  ) %>% 
  mutate(
    location = case_when(
      location == "Port of Tyne" ~ "newcastle",
      location == "Port of Valencia" ~ "valencia",
      T ~ location
    )
  ) %>% 
  left_join(
    df_flights,
    by = join_by(campaign, flight == flight_no)
  ) %>% 
  left_join(
    df_locations,
    by = join_by(location)
  ) %>% 
  mutate(
    year = case_when(
      location == "newcastle" ~ 2023,
      location == "valencia" ~ 2022,
      T ~ year
    ),
    age = as.double(as.character(year)) - year_built,
    age = if_else(age < 0, 0, age)
  ) %>% 
  relocate(
    c(date, year), .after = campaign
  ) %>% 
  relocate(
    age, .after = year_built
  ) %>% 
  relocate(
    c(location, location_type, location_label), 
    .after = campaign
  )

  
# reshape data to make easier for plotting
df_results_long <- df_results_join %>% 
  pivot_longer(
    cols = c(sfc:nox_co2_model),
    names_to = "variable"
  ) %>% 
  mutate(
    value_type = str_extract(variable, "error|model") %>% 
      replace_na("value"),
    variable = str_remove(variable, "_error|_model")
  ) %>% 
  pivot_wider(
    values_from = value, 
    names_from = value_type
  ) 

# create variable groups and labels for plotting
df_results_groups <- df_results_long %>% 
  mutate(
    year = as.factor(year),
    age_bin = case_when(
      between(age, 0, 10) ~ "0-10",
      between(age, 11, 20) ~ "11-20",
      between(age, 21, 30) ~ "21-30",
      age > 30 ~ ">30",
      T ~ NA
    ),
    tonnage_bin = case_when(
      tonnage < 10000 ~ "<10000",
      between(tonnage, 10000, 50000) ~ "10,000-50,000",
      between(tonnage, 50001, 100000) ~ "50,001-100,000",
      between(tonnage, 100001, 200000) ~ "100,001-200,000",
      tonnage > 200001 ~ ">200,000"
    ),
    tonnage_bin = factor(
      tonnage_bin,
      levels = c("<10000", "10,000-50,000", "50,001-100,000", "100,001-200,000", ">200,000" )
    ),
    group = case_when(
      variable == "sfc" & year == 2019 & location_type %in% c("Open Ocean") ~ "Ocean 2019",
      variable == "sfc" & year %in% c(2021, 2022) & location_type %in% c("Open Ocean", "Port of Valencia") ~ "Ocean 2021-2022",
      variable == "sfc" & location %in% c("EC", "newcastle") ~ "SECA 2021-2023",
      variable == "nox_co2" & location == "newcastle" ~ "Port of Tyne",
      variable == "nox_co2" & location == "valencia" ~ "Port of Valencia",
      variable == "nox_co2" & location_type == "Open Ocean" ~ "Open Ocean",
      variable == "nox_co2" & location_type == "SECA" ~ "Open Ocean",
      T ~ NA 
    )
  ) 

# check  
df_results_groups %>% 
  distinct(variable, location_type, year, location, group) %>% 
  arrange(variable) %>% 
  print_all()



# PLOT ----

# palettes ----

# discrete
pal <- c("#014961", "#009CA8", "#C2DACC", "#DC7467", "#EB0132", "gray50")
pal2 <- c("#173F5F", "#20639B", "#3CAEA3", "#F6D55c", "#ED553B")
pal_blue <- c("#014961", "#009CA8", "darkcyan", "lightblue")
pal_primary <- c("#5C9AC5","#82BD4E","#EDD054","#EE6A64")


# continuous
pal_contin <- colorRampPalette(pal)(40)

# custom palette for each group
pal_groups <- c(
  pal2[c(3,2,1,4)], "#fb8500", pal2[5]
) %>% 
  set_names(
    c(
      "Ocean 2019",
      "Ocean 2021-2022",
      "SECA 2021-2023",
      "Open Ocean",
      "Port of Tyne",
      "Port of Valencia"
    )
  )


# formatted variable labels ----
var_labs <- c(
  nox_co2 = "Delta*NO[x]/Delta*CO[2]~(by~vol.)",
  sfc = "aFSC~('%')",
  "Open Ocean" = "Open~Ocean",
  "Port of Valencia" = "Port~of~Valencia",
  "Port of Tyne" = "Port~of~Tyne",
  "Ocean 2019" = "Ocean~2019",
  "Ocean 2021-2022" = "Ocean~2021-2022",
  "SECA 2021-2023" = "SECA~2021-2023",
  "2021" = "2021",
  "2019" = "2019"
)


## SFC and NOx/CO2 by area ----

# limits
df_limits <- tribble(
  ~variable, ~value, ~label,
  "sfc", 0.1, "SECA limit",
  "sfc", 0.5, "Global limit 2020",
  "sfc", 3.5, "Global limit pre-2020",
  "nox_co2", 0.0022, "Euro 6 diesel vehicles"
) %>% 
  mutate(
    variable = factor(variable, levels = c("sfc", "nox_co2"))
  ) 

# build plot
df_results_groups %>% 
  mutate(
    location_type = factor(
      location_type,
      levels = c("Open Ocean",
                 "SECA",
                 "Port of Valencia",
                 "Port of Tyne"),
      labels = c("Open Ocean",
                 "SECA",
                 "Port of \nValencia",
                 "Port of \nTyne")
    )
  ) %>% 
  mutate(
    variable = factor(variable, levels = c("sfc", "nox_co2"))
  ) %>% 
  ggplot() +
  geom_boxplot(
    aes(location_type, value, col = year, fill = year),
    alpha = 0.5,
    outlier.alpha = 1,
    key_glyph = draw_key_point,
    position = position_dodge(preserve = "single")
  ) +
  geom_texthline(
    data = df_limits,
    aes(yintercept = value, label = label),
    linetype ="dashed",
    colour = "gray20",
    # vjust = -0.3,
    vjust = 1.4,
    hjust = 0.975
  ) +
  facet_wrap(
    ~variable,
    scales ="free",
    labeller = as_labeller(var_labs, label_parsed)
  ) +
  scale_color_manual(
    values = pal2[-1],
    name = "Measurement Year"
  ) +
  scale_fill_manual(
    values = pal2[-1],
    name = "Measurement Year"
  ) +
  facetted_pos_scales(
    x = NULL, 
    y = list(
      scale_y_log10(
        labels = scales::label_comma(drop0trailing = TRUE, suffix = "%"),
      ),
      NULL
    )
  ) +
  labs(
    x = "Location",
    y = "Value"
  ) +
  theme_shona_white_journal("top") +
  theme(
 #   axis.text = element_text(size = 11),
    axis.title.y = element_blank(),
    legend.margin=margin(c(1,1,1,1)),
  #  plot.margin = margin(0, 0.5, 0, 0, "cm"),
  ) +
  guides(
    fill = guide_legend(override.aes = list(alpha = 1, size = 2))
  ) 


# save - png
ggsave(
  here::here('plots','fig03_boxplot_sfc_nox.png'), 
  width = 8,
  height = 5
)

# save - pdf
ggsave(
  here::here('plots','fig03_boxplot_sfc_nox.pdf'), 
  width = 8,
  height = 5
)


## SFC and NOx/CO2 by age ----

# build plot
df_results_groups %>% 
  filter(
    !is.na(age_bin),
  ) %>% 
  mutate(
    variable = factor(
      variable,
      levels = c("sfc", "nox_co2")
    ),
    group = if_else(variable == "nox_co2" & location_type == "SECA", "Open Ocean", group),
    age_bin = factor(
      age_bin,
      levels = c("0-10",
                 "11-20",
                 "21-30",
                 ">30")
    )
  ) %>% 
  ggplot() +
  geom_boxplot(
    aes(value, age_bin, fill = group, col = group),
    show.legend = F,
    quantile_lines = T,
    quantiles = 2,
    alpha = 0.6
  ) +
  facet_nested_wrap(
    ~variable+group,
    scales = "free_x",
    labeller = as_labeller(var_labs, label_parsed)
  ) +
  facetted_pos_scales(
    x = list(
      scale_x_continuous(labels = function(x) paste0(x, "%")),
      scale_x_continuous(labels = function(x) paste0(x, "%")),
      scale_x_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 0.5)),
      NULL,
      NULL,
      NULL
    )
  ) +
  scale_color_manual(
    values = pal_groups,
  ) +
  scale_fill_manual(
    values = pal_groups,
  ) +
  labs(
    x = "Value",
    y = "Vessel Age (Years)"
  ) +
  theme_shona_white_journal("top") +
  theme(
    #   axis.text = element_text(size = 11),
    legend.margin=margin(c(1,1,1,1)),
    #  plot.margin = margin(0, 0.5, 0, 0, "cm"),
  ) 

# save - png
ggsave(
  here::here('plots','fig04_boxplot_sfc_nox_age.png'), 
  width = 8,
  height = 6.5
)

# save - pdf
ggsave(
  here::here('plots','fig04_boxplot_sfc_nox_age.pdf'), 
  width = 8,
  height = 6.5
)


## SFC and NOx/CO2 by tonnage ----

# build plot
df_results_groups %>% 
  mutate(
    variable = factor(
      variable,
      levels = c("sfc", "nox_co2")
    ),
    group = if_else(variable == "nox_co2" & location_type == "SECA", "Open Ocean", group),
    tonnage_bin = case_when(
      tonnage < 10000 ~ "<10000",
      between(tonnage, 10000, 50000) ~ "10,000-50,000",
      between(tonnage, 50001, 100000) ~ "50,001-100,000",
      between(tonnage, 100001, 200000) ~ "100,001-200,000",
      tonnage > 200001 ~ ">200,000"
    ),
    tonnage_bin = factor(
      tonnage_bin,
      levels = c("<10000", "10,000-50,000", "50,001-100,000", "100,001-200,000", ">200,000" )
    )
  ) %>%  
  ggplot() +
  geom_boxplot(
    aes(value, tonnage_bin, fill = group, col = group),
    show.legend = F,
    alpha = 0.6
  ) +
  facet_nested_wrap(
    ~variable+group,
    scales = "free_x",
    labeller = as_labeller(var_labs, label_parsed)
  ) +
  facetted_pos_scales(
    x = list(
      scale_x_continuous(labels = function(x) paste0(x, "%")),
      scale_x_continuous(labels = function(x) paste0(x, "%")),
      scale_x_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 0.5)),
      NULL,
      NULL,
      NULL
    )
  ) +
  scale_color_manual(
    values = pal_groups
  ) +
  scale_fill_manual(
    values = pal_groups
  ) +
  labs(
    x = "Value",
    y = "Vessel Gross Tonnage"
  ) +
  theme_shona_white_journal("top") +
  theme(
    #   axis.text = element_text(size = 11),
    legend.margin=margin(c(1,1,1,1)),
    #  plot.margin = margin(0, 0.5, 0, 0, "cm"),
  ) 


# save - png
ggsave(
  here::here('plots','fig05_boxplot_sfc_nox_tonnage.png'), 
  width = 8.5,
  height = 6.5
)

# save - pdf
ggsave(
  here::here('plots','fig05_boxplot_sfc_nox_tonnage.pdf'), 
  width = 8.5,
  height = 6.5
)


## scrubber comparison ----

# build plot
df_results_groups %>% 
  filter(
    variable == "sfc",
    !is.na(scrubber)
  ) %>% 
  mutate(
    scrubber = case_when(
      scrubber ~ "Yes",
      !scrubber ~ "No"
    ),
    scrubber = factor(
      scrubber,
      levels = c("Yes", "No")
    )
  ) %>% 
  ggplot() +
  geom_boxplot(
    aes(group, value, fill = scrubber, col = scrubber),
    show.legend = T,
    alpha = 0.6,
    key_glyph = draw_key_point
  ) +
  scale_fill_manual(
    values = pal2[c(2, 5)],
    name = "Exhaust Gas Scrubber?"
  ) +
  scale_colour_manual(
    values = pal2[c(2, 5)],
    name = "Exhaust Gas Scrubber?"
) +
  scale_y_continuous(
    labels = paste_symbol
  ) +
  labs(
    y = "aFSC",
    x = "Location"
  ) +
  theme_shona_white_journal("top") +
  theme(
    legend.margin=margin(c(0,0,-10,0)),
  ) + 
  guides(
    fill = guide_legend(override.aes = list(alpha = 1, size = 2))
  )

# save 
ggsave(
  here::here('plots','fig06_boxplot_sfc_scrubber.png'), 
  width = 4.5,
  height = 4
)

# save 
ggsave(
  here::here('plots','fig06_boxplot_sfc_scrubber.pdf'), 
  width = 4.5,
  height = 4
)

## model comparison ----

# filter results for those with modelled values only
df_model_compare <- df_results_groups %>% 
  filter(
    !is.na(model)& !is.na(value)
  ) %>% 
  nest_by(variable) %>% 
  mutate(
    data = list(
      data %>% 
        arrange(year, value) %>% 
        rowid_to_column("ship_index")
    )
  ) %>% 
  unnest(data) %>% 
  relocate(
    variable,
    .before = value
  ) %>% 
  filter(
    !(variable == "nox_co2" & year == 2021)
  ) %>% 
  mutate(
    variable = factor(
      variable, 
      levels = c("nox_co2", "sfc")
    )
  ) 

# calc mean measured and modelled values for each year
df_model_mean <- df_model_compare %>% 
  pivot_longer(
    cols = c(value, model),
    names_to = "value_type"
  ) %>% 
  group_by(
    value_type, variable, year
  ) %>% 
  group_modify(
    ~calculate_errors(.$value)
  ) %>% 
  mutate(
    variable_label = case_when(
      value_type == "value" ~ "Measured",
      value_type == "model" ~ "Modelled"
    )
  ) %>% 
  arrange(variable, year)

# build plot  
df_model_compare %>% 
  ggplot() +
  geom_segment(
    aes(value, ship_index, xend = model, yend = ship_index),
    col = "gray50"
  ) +
  geom_point(
    aes(value, ship_index, col = "Measured"),
    size = 1
  ) +
  geom_point(
    aes(model, ship_index, col = "Modelled"),
    size = 1
  ) +
  geom_vline(
    data = df_model_mean,
    aes(
      xintercept = mean,
      col = variable_label
    ),
    linetype = "dashed",
    show.legend = F
  ) +
  geom_rect(
    data = df_model_mean,
    inherit.aes = FALSE,
    aes(
      xmin = lower, 
      xmax = upper, 
      ymin = -Inf, 
      ymax = Inf, 
      fill = variable_label
    ),
    alpha = 0.3,
    show.legend = F
  ) +
  facet_nested_wrap(
    ~variable+year,
    scales = "free",
    labeller = as_labeller(var_labs, label_parsed)
  ) +
  scale_color_manual(
    values = pal2[c(3, 5)]
  ) +
  scale_fill_manual(
    values = pal2[c(3, 5)]
  ) +
  facetted_pos_scales(
    y = NULL, 
    x = list(
      scale_x_continuous(
        limits = c(0, 0.04),
        labels = c(0, 0.01, 0.02, 0.03, 0.04)
      ),
      scale_x_continuous(
        limits = c(0, 5),
        labels = scales::label_comma(drop0trailing = TRUE, suffix = "%"),
      ),
      scale_x_continuous(
        limits = c(0, 5),
        labels = scales::label_comma(drop0trailing = TRUE, suffix = "%"),
      )
    )
  ) +
  labs(
    y = "Ship Index"
  ) +
  theme_shona_white_journal("top") +
  theme(
    legend.margin=margin(c(0,0,-10,0)),
    legend.title = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_blank()
  ) +
  guides(
    colour = guide_legend(
      override.aes = list(size = 2)
    )
  )

# save 
ggsave(
  here::here('plots','fig08_meas_vs_mod.png'), 
  width = 8.5,
  height = 6
)

# save 
ggsave(
  here::here('plots','fig08_meas_vs_mod.pdf'), 
  width = 8.5,
  height = 6
)


# EXTRACT FIGURE DATA ----

## SFC and NOx/CO2 by area ----
df_sfc_nox_area <- df_results_groups %>% 
  mutate(
    location_type = factor(
      location_type,
      levels = c("Open Ocean",
                 "SECA",
                 "Port of Valencia",
                 "Port of Tyne")
    )
  ) %>% 
  filter(
    variable %in% c("sfc", "nox_co2"),
    !is.na(value)
  ) %>% 
  group_by(
    year, location_type, variable
  ) %>% 
  group_modify(
    ~calculate_percentiles(.$value, probs = c(.25,.5,.75))
  ) %>% 
  arrange(
    variable, year, location_type
  ) %>% 
  rename_with(
    .cols = contains("%"),
    .fn = ~str_c("Q",str_remove(., "%"))
  )

# save 
write_csv(
  df_sfc_nox_area,
  here::here('data/figure_data','fig03_boxplot_sfc_nox.csv')
)


## SFC and NOx/CO2 by age ----
df_sfc_nox_age <- df_results_groups %>% 
  group_by(
    variable, 
    age_bin,
    group
  ) %>% 
  group_modify(
    ~calculate_percentiles(.$value, probs = c(0.25, 0.5, 0.75))
  ) %>% 
  arrange(
    desc(variable),
    group, 
    desc(age_bin)
  ) %>% 
  rename_with(
    .cols = contains("%"),
    .fn = ~str_c("Q",str_remove(., "%"))
  ) 

# save 
write_csv(
  df_sfc_nox_age,
  here::here('data/figure_data','fig04_boxplot_sfc_nox_age.csv')
)



## SFC and NOx/CO2 by tonnage ----
df_sfc_nox_tonnage <- df_results_groups %>% 
  group_by(
    variable, 
    tonnage_bin,
    group
  ) %>% 
  group_modify(
    ~calculate_percentiles(.$value, probs = c(0.25, 0.5, 0.75))
  ) %>% 
  arrange(
    desc(variable),
    group, 
    desc(tonnage_bin)
  ) %>% 
  rename_with(
    .cols = contains("%"),
    .fn = ~str_c("Q",str_remove(., "%"))
  ) %>% 
  print_all()

# save 
write_csv(
  df_sfc_nox_tonnage,
  here::here('data/figure_data','fig05_boxplot_sfc_nox_tonnage.csv')
)


## scrubber comparison ----
df_sfc_scrubber <- df_results_groups %>% 
  filter(
    variable == "sfc",
    !is.na(scrubber)
  ) %>% 
  mutate(
    scrubber = case_when(
      scrubber ~ "Yes",
      !scrubber ~ "No"
    ),
    scrubber = factor(
      scrubber,
      levels = c("Yes", "No")
    )
  ) %>% 
  group_by(
    variable,
    group,
    scrubber
  ) %>% 
  group_modify(
    ~calculate_percentiles(.$value, probs = c(0.25, 0.5, 0.75))
  ) %>% 
  arrange(
    desc(variable),
    group, 
    desc(scrubber)
  ) %>% 
  rename_with(
    .cols = contains("%"),
    .fn = ~str_c("Q",str_remove(., "%"))
  ) %>% 
  print_all()

# save 
write_csv(
  df_sfc_scrubber,
  here::here('data/figure_data','fig06_boxplot_sfc_scrubber.csv')
)


## model comparison ----

# save 
write_csv(
  df_model_mean,
  here::here('data/figure_data','fig08_meas_vs_mod_mean.csv')
)

# save 
write_csv(
  df_model_compare,
  here::here('data/figure_data','fig08_meas_vs_mod.csv')
)

