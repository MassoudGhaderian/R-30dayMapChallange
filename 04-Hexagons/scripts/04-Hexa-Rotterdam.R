# Title and author information (remove if this is not an R Markdown document)
# title: "04Nov_Hexagon"
# author: "Daniele Cannatella"
# date: "2024-11-01"
# format: html: default; pdf: default

# 1. Package Installation and Loading
# Load necessary libraries
packages <- c("ggplot2", "dplyr", "sf", "readr", "tidyr", "showtext", "here", 
              "stringr", "grid", "magick", "ggspatial", "cowplot", "png")

# Function to check if packages are installed and load them
load_packages <- function(pkgs) {
  missing_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
  if (length(missing_pkgs)) install.packages(missing_pkgs)
  invisible(lapply(pkgs, library, character.only = TRUE))
}

# Load the packages
load_packages(packages)

# Confirm package loading
cat("All specified packages have been loaded successfully!\n")

# 2. Import Files
# 2.1 Import trees dataset
trees <- st_read(here("04Nov_Hexagon/data/Bomen.shp"))

# 2.2 Import Rotterdam neighborhoods (buurten)
buurten <- st_read(here("04Nov_Hexagon/data/Buurten_Rotterdam_urban.shp"))
plot(st_geometry(buurten))

# 2.3 Import and clip OSM water features
water <- st_read(here("04Nov_Hexagon/data/gis_osm_water_a_free_1.shp")) %>%
  st_transform(crs = st_crs(buurten)) %>%
  st_intersection(buurten)

# Import and filter labels (e.g., parks) for labeling
labels <- st_read(here("04Nov_Hexagon/data/gis_osm_pois_a_free_1.shp")) %>%
  st_transform(crs = st_crs(buurten)) %>%
  st_intersection(buurten) %>%
  filter(fclass == "park")

# Generate labels with area category
labels <- labels %>%
  mutate(
    area_ha = as.numeric(st_area(.)) / 10000,
    area_cat = case_when(
      area_ha > 30 ~ "more than 30 ha",
      area_ha > 20 & area_ha <= 30 ~ "between 20 and 30 ha",
      TRUE ~ "more than 10 ha"
    ),
    centroid = st_centroid(geometry)
  )

# 2.5 Import Rbanism logo
rbanism_logo <- image_read(here("04Nov_Hexagon/fig/Logo_Rbanism_White.png"))
logo_grob <- rasterGrob(rbanism_logo, interpolate = TRUE)

# 3. Create  Hexagonal Fishnet
# Calculate side length for a 5-hectare hexagon
t_area <- 50000  # 5 hectares in square meters
s_length <- sqrt((2 * t_area) / (3 * sqrt(3)))

# Create the hexagonal grid
hex_grid <- st_make_grid(
  buurten,
  cellsize = s_length,
  square = FALSE
)

# Dissolve neighborhoods by "gemeenteco" attribute and create an SF object for the hex grid
rdm_bounds <- buurten %>%
  group_by(gemeenteco) %>%
  summarize(geometry = st_union(geometry))

# Convert hex grid to an sf object
hex_grid_sf <- st_sf(geometry = hex_grid)

# Select hexagons fully within each neighborhood
hex_within_buurten <- hex_grid_sf %>%
  filter(lengths(st_intersects(geometry, rdm_bounds)) > 0) %>%
  mutate(hex_id = row_number())

# Plot to verify hex grid within neighborhoods
plot(st_geometry(buurten), col = "lightgray")
plot(st_geometry(hex_within_buurten), add = TRUE, border = "blue")

# 4. Join Trees Data with Hexagons and Summarize
# Spatial join of trees to hexagons
trees_in_hex <- st_join(trees, hex_within_buurten, left = FALSE)

# Summarize unique species count per hexagon and find predominant species
species_summary <- trees_in_hex %>%
  group_by(hex_id) %>%
  summarize(
    unique_species_count = n_distinct(GESLACHT),
    predominant_species = GESLACHT[which.max(table(GESLACHT))]
  )

# 5. Plot Hex Map
# Import Google fonts for custom text
showtext_auto()
font_add_google(name = "Orbitron", family = "orbitron")
font_add_google(name = "Audiowide", family = "audiowide")

# Label size mapping
label_sizes <- c("more than 10 ha" = 3, "between 20 and 30 ha" = 5, "more than 30 ha" = 7)

# Join species summary data to hex grid for mapping
hex_data <- st_join(hex_within_buurten, species_summary)

# Plot hex map with ggplot2
hexmap <- ggplot() +
  geom_sf(data = hex_data, aes(fill = unique_species_count), color = "#2a2a2a", size = NA, alpha = 0.9) +
  scale_fill_viridis_c(option = "inferno", name = "Number of tree species", na.value = "#2d2d2d") +
  geom_sf(data = water, fill = "#2E2E2E", color = NA, alpha = 0.9) +
  geom_sf_text(data = labels, aes(label = name, size = area_cat),
               family = "audiowide", color = "white", alpha = 0.7,
               check_overlap = TRUE, show.legend = FALSE) +
  scale_size_manual(values = label_sizes) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#2d2d2d", color = NA),
    legend.title = element_text(family = "orbitron", size = 14, face = "bold", color = "white", hjust = 0.5),
    legend.text = element_text(family = "orbitron", size = 12, color = "white", hjust = 0.5),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.justification = "center",
    legend.background = element_rect(fill = "#2d2d2d", color = NA),
    legend.key = element_rect(fill = "#2d2d2d", color = NA),
    legend.key.height = unit(0.3, "cm")
  )

# Add title, subtitle, and caption
hexmap <- hexmap +
  labs(
    title = "Distribution of Tree Species Across Hexagons in Rotterdam",
    subtitle = "Hexagons representing areas of 5 Hectares with tree species counts",
    caption = paste("#30DayMapChallenge. Daniele Cannatella, 2024. Hexagon Map.\nData source: Gemeente Rotterdam")
  ) +
  theme(
    plot.title = element_text(family = "orbitron", size = 20, face = "bold", color = "white"),
    plot.subtitle = element_text(family = "orbitron", size = 16, color = "white"),
    plot.caption = element_text(family = "orbitron", size = 14, color = "white", lineheight = 0.3)
  )

# 7. Export the map
output_file <- "output/04Nov_hexmap.png"
ggsave(filename = output_file, plot = hexmap, device = "png", units = "in", width = 6, height = 6, dpi = 300)
cat("Map has been exported as", output_file, "with a 1:1 aspect ratio.\n")
