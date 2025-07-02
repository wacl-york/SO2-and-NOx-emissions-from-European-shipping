pacman::p_load(
  tidyverse,
  sf,
  shonarrr,
  leaflet,
  mobilemeasr,
  gissr,
  sspatialr,
  ggrepel,
  ggspatial,
  ggpubr,
  rnaturalearth,
  sp,
  ggpattern
)



# LOAD ----

# UK map
st_map <- map_as_st()

# CREATE BASE MAP ----

# define bbox coordinates
bbox <- c(-13.726721695874524, 12, 36.5, 62.516535224806276)

# create envelope
st_bbox <- sp_create_envelope(bbox) %>% 
  st_as_sf()

# check
st_bbox %>% 
  leaflet_plot()

# crop map
st_map_crop <- st_map %>% 
  st_crop(st_bbox) %>% 
  filter(
    !name %in% c("Algeria", " Faeroe Is", "Tunisia")
  )

# check
st_map_crop %>% 
  leaflet_plot()


# LOCATIONS TO PLOT ----

# df of locations
df_locations <- tribble(
  ~name, ~lat, ~long,
  "Penlee Point", 50.318000, -4.189167,
  "Port of Tyne", 54.9786, -1.4462,
  "Port of Valencia", 39.4481, -0.3169
)

# as spatial
st_locations <- df_locations %>% 
  st_from_df() %>% 
  st_transform(4326)

# check
st_locations %>% 
  leaflet_plot()


# REGIONS ----

# define region bounding boxes
bb <- c(-8.1518555, -5.6689453, 46.1950421, 48.8213325)
sw <- c(-9, -6.0205078, 49.5252083, 50.9)
pt <- c(-12, -9.75, 38.5, 43.5)

ec <- matrix(c(
  -4.9, 49.8521517,
  -4.6362305, 48.7344554,
  2.0214844, 50.9584267,
  1.7138672, 51.2344074,
  1.7138672, 51.2206474,
  -4.9, 49.8379825,
  -4.9, 49.8521517  # Close the polygon
), byrow = TRUE, ncol = 2)

# create polygons

# BB
bb_poly <- bb %>% 
  sp_create_envelope() %>% 
  st_as_sf(crs = st_crs(4326)) %>% 
  mutate(
    name = "Bay of Biscay (BB)"
  )

# SW
sw_poly <- sw %>% 
  sp_create_envelope() %>% 
  st_as_sf(crs = st_crs(4326)) %>% 
  mutate(
    name = "Southwest Approaches (SW)"
  )

# PT
pt_poly <- pt %>% 
  sp_create_envelope() %>% 
  st_as_sf(crs = st_crs(4326)) %>% 
  mutate(
    name = "Porto Coast (PT)"
  )

# EC
ec_poly <- st_sfc(
  st_polygon(list(ec)), 
  crs = 4326
) %>% 
  st_sf() %>% 
  mutate(name = "English Channel (EC)")

# bind
st_regions <- bind_rows(
  bb_poly,
  sw_poly,
  pt_poly,
  ec_poly
)

# plot
st_regions %>% 
  leaflet_plot_coloured("name")



# SECA BOUNDARY ----
st_seca <- st_read(
  here::here('data/SECA', "eca_reg14_sox_pm.shp")
)

# filter
st_seca_filt <- st_seca %>% 
  filter(area %in% c("North Sea area", "Baltic Sea area")) %>% 
  st_make_valid() %>% 
  st_simplify(dTolerance = 10000)

# crop to map size
st_seca_crop <- st_seca_filt %>% 
  st_crop(st_bbox) 

# plot
st_seca_crop %>% 
  leaflet_plot()

# define SECA line segements
line1 <- st_linestring(matrix(c(-5, 50, -5, 48.4), ncol = 2, byrow = TRUE))
line2 <- st_linestring(matrix(c(5.6, 62, -4, 62), ncol = 2, byrow = TRUE))
line3 <- st_linestring(matrix(c(-4, 58.4, -4, 62), ncol = 2, byrow = TRUE))
line4 <- st_linestring(matrix(c(10.5468750, 57.4408, 11.7993164, 57.4408), ncol = 2, byrow = T))

# combine
st_seca_lines <- st_sf(
  geometry = st_sfc(line1, line2, line3, line4, crs = 4326)
)

# check
st_seca_lines %>% 
  leaflet_plot()

# seca label
seca_point <- point <- st_point(c(3, 57.5))

# as sf
st_seca_point <- st_sf(
  geometry = st_sfc(seca_point, crs = 4326)
) %>% 
  mutate(
    name = "SECA"
  )


# PLOT ----

# palette ----
pal <- c("#173F5F", "#20639B", "#3CAEA3", "#F6D55c", "#ED553B")


# buiid plot
ggplot() +
  geom_sf(
    data = st_seca_lines,
    col = "darkblue",
    linewidth = 1
  ) +
  # geom_sf(
  #   data = st_seca_crop,
  #   fill = "darkblue",
  #   col = NA,
  #   alpha = 0.25
  # ) +
  geom_sf_pattern(
    data = st_seca_crop,
    aes(
      pattern_fill = "ght",
      pattern_alpha = 0.2,
      pattern_colour = "test"
    ),
    fill = "darkblue",
    alpha = 0.25,
    #colour = "darkblue",
    pattern_spacing = 0.05,
    pattern_alpha = 0.25,
    pattern = "circle"
  ) +
  geom_sf_text(
    data = st_seca_point,
    aes(
      label = name
    ),
    size = 5,
    col = "darkblue"
  ) +
  geom_sf(
    data = st_map_crop %>% 
      st_buffer(dist = 1000),
    fill = "gray60",
    col = "white",
    linewidth = 0.5
  ) +
  geom_sf(
    data = st_regions,
    aes(
      fill = name,
      col = name
    ),
    alpha = 0.9,
    linewidth = 0.5
  ) +
  geom_sf(
    data = st_locations,
    size = 3,
    col = "black"
  ) +
  geom_label_repel(
    data = st_locations,
    aes(label = name, geometry = geometry),
    stat = "sf_coordinates",
    direction = "y",
    nudge_y = 1,
    size = 4
  ) +
  scale_fill_manual(
    values = pal[c(-1)],
    name = "Measurement Region"
  ) +
  scale_colour_manual(
    values = pal[c(-1)],
    name = "Measurement Region"
  ) +
  scale_pattern_manual(
    values = c( "circle")
  ) +
  scale_pattern_fill_manual(
    values = "darkblue"
  ) +
  scale_pattern_colour_manual(
    values = NA
  ) +
  annotation_scale(
    location = "br",
    text_cex = 0.75,
  ) +
  theme_void() +
  theme(
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    legend.position = c(0.14, 0.88),
    panel.background = element_rect(fill = "white", colour = "white"),
    plot.background = element_rect(fill = "white", colour = "white")
  ) +
  guides(
    fill = guide_legend(
      override.aes = list(linewidth = 0.5, alpha = 0.9)
    ),
    pattern_fill = "none",
    pattern_colour = "none"
  )

# save
ggsave(
  here::here('plots', "figure01_shipping_regions_map.png"),
  width = 7,
  height = 9
)

# save
ggsave(
  here::here('plots', "figure01_shipping_regions_map.pdf"),
  width = 7,
  height = 9
)


