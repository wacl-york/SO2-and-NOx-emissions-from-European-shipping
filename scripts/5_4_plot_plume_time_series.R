pacman::p_load(
  tidyverse,
  shonarrr,
  janitor,
  ggpubr
)


# LOAD ----

# point sampling data
df_van <- read_csv(
  here::here('data/van_merge/','newcastle_van_merge.csv')
  )

# van plumes
df_van_plumes <- read_csv(
  here::here('data/van_merge/','df_van_plumes.csv')
)


# flight data
df_flights <- read_csv(
  here::here('data/faam_merge/','c181_merge.csv')
) %>% 
  arrange(date) %>% 
  as_tibble() %>% 
  clean_names() %>% 
  mutate(
    across(
      c(no_mr, no2_mr), ppt_to_ppb
    ),
    nox_ppb = no_mr+no2_mr
  ) %>% 
  rename(so2 = so2_teco) 

# PLOT ----

# palettes ----

# discrete
pal <- c("#014961", "#009CA8", "#C2DACC", "#DC7467", "#EB0132", "gray50")
pal2 <- c("#173F5F", "#20639B", "#3CAEA3", "#F6D55c", "#ED553B")


## flight time series ----

# labels
var_labs <- c(
  so2 = "Mixing~Ratio~(ppb)",
  co2 = "Mixing~Ratio~(ppm)",
  nox_ppb = "Mixing~Ratio~(ppb)"
)

df_text <- data.frame(
  label = c("CO[2]", "NO[x]", "SO[2]"),
  variable = c("co2", "nox_ppb", "so2")
) %>% 
  mutate(
    variable = factor(
      variable,
      levels = c("so2", "nox_ppb", "co2") 
    )
  )


# plot
a <- df_flights %>% 
  filter(
    between(date, ymd_hms("2019-07-12 11:00:00"), ymd_hms("2019-07-12 11:11:00")),
    co2_flag == 0,
    nox_ppb > 0
  ) %>% 
  pivot_longer(
    cols = c(so2, co2, nox_ppb),
    names_to = "variable"
  ) %>% 
  mutate(
    variable = factor(
      variable,
      levels = c("so2", "nox_ppb", "co2") 
    )
  ) %>% 
  ggplot() +
  geom_line(
    aes(date, value, col = variable),
    show.legend = F
  ) +
  annotate(
    geom = "rect",
    xmin = ymd_hms("2019-07-12 11:06:10"),
    xmax = ymd_hms("2019-07-12 11:07:00"),
    ymin = -Inf,
    ymax = Inf,
    alpha = 0.25,
    col = "gray95"
  ) +
  annotate(
    geom = "rect",
    xmin = ymd_hms("2019-07-12 11:01:40"),
    xmax = ymd_hms("2019-07-12 11:02:40"),
    ymin = -Inf,
    ymax = Inf,
    alpha = 0.25,
    col = "gray95"
  ) +
  annotate(
    geom = "rect",
    xmin = ymd_hms("2019-07-12 11:09:50"),
    xmax = ymd_hms("2019-07-12 11:10:50"),
    ymin = -Inf,
    ymax = Inf,
    alpha = 0.25,
    col = "gray95"
  ) +
  geom_label(
    data = df_text,
    aes(x = ymd_hms("2019-07-12 11:11:00"), y = Inf, label = label, fill = variable),
    col = "white",
    show.legend = F,
    hjust = 1.05,
    vjust = 1.05,
    parse = T,
    size = 4,
    label.padding = unit(0.5, "lines"),
  ) +
  facet_wrap(
    ~variable, 
    scales = "free_y",
    ncol = 1,
    strip.position = "left",
    labeller = as_labeller(var_labs, label_parsed)
  ) +
  scale_color_manual(
    values = pal2[c(5,3,1)]
    #  values = pal2[c(3,2,1)]
    #  values = pal2[c(1,3, 5)]
    #    values = pal[c(5, 2, 6)]
  ) +
  scale_fill_manual(
    values = pal2[c(5,3,1)]
    
    # values = pal2[c(3,2,1)]
    #    values = pal[c(5, 2, 6)]
  ) +
  scale_y_continuous(
    expand = c(0.02, 0)
  ) +
  scale_x_datetime(
    expand = c(0, 0)
  ) +
  labs(
    x = "Time",
    y = "Mixing Ratio",
    title = "Aerial Measurements"
  ) +
  #  coord_cartesian(expand = F) +
  theme_shona_white_journal() +
  theme(
    strip.placement = "outside",
    strip.background = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_rect(colour = "black", linewidth = 1, fill = NA),
    legend.margin=margin(c(1,1,1,1)),
    plot.margin = margin(0.1, 0.8, 0.1, 0.1, "cm"),
    axis.title.x = element_text(margin = margin(t = 1.5)),
  )

a



## van time series ----
b <- df_van %>% 
  filter(
    between(date, ymd_hms("2023-05-15 09:30:00"), ymd_hms("2023-05-15 11:30:00"))
  ) %>% 
  pivot_longer(
    cols = c(so2, co2, nox_ppb),
    names_to = "variable"
  ) %>% 
  mutate(
    variable = factor(
      variable,
      levels = c("so2", "nox_ppb", "co2") 
    )
  ) %>% 
  ggplot() +
  geom_line(
    aes(date, value, col = variable),
    show.legend = F
  ) +
  geom_rect(
    data = df_van_plumes %>%
      filter(
        between(plume_start, ymd_hms("2023-05-15 09:30:00"), ymd_hms("2023-05-15 11:30:00"))
      ),
    aes(xmin = plume_start, xmax = plume_end, ymin = -Inf, ymax = Inf),
    alpha = 0.25,
    col = "gray95"
  ) +
  geom_label(
    data = df_text,
    aes(x = ymd_hms("2023-05-15 11:30:00"), y = Inf, label = label, fill = variable),
    col = "white",
    show.legend = F,
    hjust = 1.05,
    vjust = 1.05,
    parse = T,
    size = 4,
    label.padding = unit(0.5, "lines"),
  ) +
  facet_wrap(
    ~variable, 
    scales = "free_y",
    ncol = 1,
    strip.position = "left",
    labeller = as_labeller(var_labs, label_parsed)
  ) +
  scale_color_manual(
    values = pal2[c(5,3,1)]
    #    values = pal[c(5, 2, 6)]
  ) +
  scale_fill_manual(
    values = pal2[c(5,3,1)]
    #    values = pal[c(5, 2, 6)]
  ) +
  scale_y_continuous(
    expand = c(0.02, 0)
  ) +
  scale_x_datetime(
    expand = c(0, 0)
  ) +
  labs(
    x = "Time",
    y = "Mixing Ratio",
    title = "Ground-Based Point Sampling"
  ) +
  #  coord_cartesian(expand = F) +
  theme_shona_white_journal() +
  theme(
    legend.margin=margin(c(1,1,1,1)),
    plot.margin = margin(0.1, 0.8, 0.1, 0.1, "cm"),
    axis.title.x = element_text(margin = margin(t = 1.5)),
    strip.placement = "outside",
    strip.background = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_rect(colour = "black", linewidth = 1, fill = NA)
  )

b


# arrange
ggarrange(
  a,b,
  ncol = 2,
 labels = c("a)", "b)"),
 font.label = list(size = 9, color = "black", face = "bold", family = NULL)
) 


# save 
ggsave(
  here::here('plots', "fig02_time_series_plumes.png"),
  width = 11,
  height = 6
)

# save 
ggsave(
  here::here('plots', "fig02_time_series_plumes.pdf"),
  width = 11,
  height = 6
)
