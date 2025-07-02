pacman::p_load(
  tidyverse,
  shonarrr,
  janitor,
  geomtextpath
)

# LOAD -----

# penlee point time series
df_penlee <- read_delim(
  file <- here::here('data/penlee_point', "penlee_so2.txt"),
  show_col_types = F
) %>% 
  set_names(
    c("date", "so2", "so2_sw")
  )

# SUMMARISE ----

# reshape
df_penlee_long <- df_penlee %>% 
  pivot_longer(
    cols = -date,
    names_to = "variable"
  ) 

# annual means
df_summary <- df_penlee_long %>% 
  mutate(
    year = year(date)
  ) %>% 
  group_by(
    year, variable
  ) %>% 
  group_modify(
    ~calculate_ci(.$value)
  )



# PLOT ----

# palettes
pal <- c("#014961", "#009CA8", "#C2DACC", "#DC7467", "#EB0132", "gray50")
pal2 <- c("#173F5F", "#20639B", "#3CAEA3", "#F6D55c", "#ED553B")

## trend ----
# define trend function
theil_sen <- function(..., weights = NULL) {
  mblm::mblm(...)
}

# plot
df_penlee_long %>% 
  filter(
    #   year(date) > 2014
  ) %>% 
  ggplot(
    aes(date, value, col = variable, fill = variable)
  ) +
  geom_line(
    linewidth = 0.5,
    show.legend = T
  ) +
  geom_smooth(
    method = "lm",
    alpha = 0.2,
    linewidth = 0.75,
    linetype = "dashed",
    show.legend = F
  ) +
  scale_colour_manual(
    values = pal[c(5, 2)],
    name = "Wind direction",
    labels = c(so2 = "All",
               so2_sw = "SW")
  ) +
  scale_fill_manual(
    values = pal[c(5, 2)],
    name = "Wind direction",
    labels = c(so2 = "All",
               so2_sw = "SW")
  ) +
  labs(
    x = "Date",
    y = expression(SO[2]~(ppb))
  ) +
  theme_shona_white_smart() +
  theme(
    legend.position = c(0.9, 0.9)
  )


## annual means ----
df_summary %>% 
  filter(year > 2014) %>% 
  ggplot() +
  geom_textvline(
    label = "Low sulfur regulations introduced",
    xintercept = 2020,
    linetype = "dashed",
    col = "grey30",
    vjust = 2,
    hjust = 0.9
  ) +
  geom_pointrange(
    aes(year, mean, ymin = lower, ymax = upper, col = variable)
  ) +
  scale_colour_manual(
    values = pal2[c(2, 5)],
    name = "Wind Direction",
    labels = c(so2 = "All",
               so2_sw = "Southwest")
  ) +
  labs(
    x = "Year",
    y = expression(Annual~Mean~SO[2]~(ppb))
  ) +
  scale_x_continuous(
    breaks = seq(2015, 2022, 1)
  ) +
  theme_shona_white_journal() +
  theme(
    legend.position = c(0.89, 0.88),
    legend.background = element_rect(fill = "white", linewidth = 0.2)
  )


# save 
ggsave(
  here::here('plots','fig09_penlee_point_so2.png'), 
  width = 5.5,
  height = 4
)

# save 
ggsave(
  here::here('plots','fig09_penlee_point_so2.pdf'), 
  width = 6.5,
  height = 5
)

# SAVE ----

# export data
write_csv(
  df_summary %>% 
    arrange(variable, year),
  here::here('data/figure_data','fig09_penlee_point_so2.csv'), 
)

