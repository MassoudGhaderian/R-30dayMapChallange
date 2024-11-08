```r
title: "04Nov_Hexagon"
author: "Daniele Cannatella"
date: "2024-11-01"
format:
  html: default
pdf: default

A Hexagon Map: Trees Diversity in Rotterdam

1. Package Installation and Loading
{r, echo=FALSE, warning=FALSE}
# Define the packages to be used
packages <- c("ggplot2", "dplyr", "sf", "readr", "tidyr", "showtext", "here", "stringr", "grid", "magick",
              "ggspatial", "cowplot")

# Function to check if packages are installed and load then
load_packages <- function(pkgs) {
}
# Check for missing packages
missing_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]

# Install missing packages
if (length(missing_pkgs)) {
}
install.packages(missing_pkgs)

# Load all packages
lapply(pkgs, library, character.only= TRUE)

# Load the packages
load_packages(packages)

# Print a message to confirm successful loading
cat("All specified packages have been loaded successfully!\n")

2. Import files
2.1 Import trees dataset
{r}
trees <- st_read(here("04Nov_Hexagon/data/Bomen.shp"))

2.2 Import Rotterdam buurten
{r}
buurten <- st_read(here("04Nov_Hexagon/data/Buurten_Rotterdam_urban.shp"))
{r}
plot(buurten)

2.3 Import features from OSM and clip
{r}
water <- st_read(here("04Nov_Hexagon/data/gis_osm_water_a_free_1.shp"))
{r}
water <- st_transform(water, crs = st_crs(buurten))
water <- st_intersection(water, buurten)

{r}
labels <- st_read(here("04Nov_Hexagon/data/gis_osm_pois_a_free_1.shp"))
{r}
labels <- st_transform(labels, crs = st_crs(buurten))
labels <- st_intersection(labels, buurten)
head(labels)

{r}
unique(labels$fclass)

{r}
labels <- labels %>%
  filter(fclass == "park") #Check the result
plot(st_geometry(labels))

2.4 Generate labels
{r}
labels <- labels %>%
  mutate(area_ha = as.numeric(st_area(.))/10000) %>%
  filter(area_ha >= 10)

{r}
labels <- labels %>%
  mutate(area_cat = case_when(
    area_ha > 30 ~ "more than 30 ha",
    area_ha > 20 & area_ha <= 30 ~ "between 20 and 30 ha",
    TRUE ~ "more than 10 ha"
  ))

{r}
labels <- labels %>%
  mutate(centroid = st_centroid(geometry))

2.5 Import Rbanism logo
{r}
# Download Rbanism logo
rbanism_logo <- image_read(here("04Nov_Hexagon/fig/Logo_Rbanism_White.png"))
logo_grob <- rasterGrob(rbanism_logo, interpolate = TRUE)

{r}
rbanism_logo2 <- image_read('https://rbanism.org/assets/imgs/about/vi_l.jpg')

{r}
get_png <- function(filename) {
  grid::rasterGrob(png::readPNG(filename), interpolate = TRUE)
}
l <- get_png(here("04Nov_Hexagon/fig/Logo_Rbanism_White.png"))

3. Create Hex fishnet
{r}
# Step 1: Calculate side length for a 5-hectare hexagon
t_area <- 50000 # 5 hectares in square meters
s_length <- sqrt((2*t_area) / (3*sqrt(3))) # Side length for 10 ha hex

# Step 2: Create the hexagonal grid
hex_grid <- st_make_grid(
  buurten,
  cellsize = s_length,
  square = FALSE
)

# Dissolve by the "gemeenteco" attribute
rdm_bounds <- buurten %>%
  group_by(gemeenteco) %>%
  summarize(geometry = st_union(geometry))

# Convert the hex grid to an sf object for spatial operations.
hex_grid_sf <- st_sf(geometry = hex_grid)

# Select only hexagons that are fully within each buurt
# This uses st_within() to check if each hexagon is fully contained within a buurt
hex_within_buurten <- hex_grid_sf %>%
  filter(lengths(st_intersects(geometry, rdm_bounds, sparse = FALSE)) > 0)

# Add an ID column to hex_grid_st
hex_within_buurten <- hex_within_buurten %>%
  mutate(hex_id = row_number()) # Creates a unique ID for each hexagon

# Verify the result
head(hex_within_buurten)

# Plot to verify
plot(st_geometry(buurten), col = "lightgray")
plot(hex_within_buurten$geometry, add = TRUE, border = "blue")

4. Join trees and hex and summarize
{r}
# Spatial join trees to hexagons
trees_in_hex <- st_join(trees, hex_within_buurten, left = FALSE)

{r}
# Group by hexagon and summarize
species_summary <- trees_in_hex %>%
  group_by(hex_id) %>%
  summarize(
    unique_species_count = n_distinct(GESLACHT), # Count unique species
    predominant_species = GESLACHT[which.max(table(GESLACHT))] # Find predominant species
  )

#View result
print(species_summary)

{r}
unique(species_summary$predominant_species)

5. Plot hex map!
  5.1 Import Google fonts
{r}
# Add Google Fonts to the system
showtext_auto() # Automatically use showtext for text rendering

#Add Orbitron
font_add_google(name = "Orbitron", family = "orbitron")

# Add Audiowide
font_add_google(name = "Audiowide", family = "audiowide")

{r}
# Define label size mapping for each category
label_sizes <- c("more than 10 ha" = 3, "between 20 and 30 ha" = 5, "more than 30 ha" = 7)

# Join species_sumary data to hex grid using st join for spatial join
hex_data <- st_join(hex_within_buurten, species_summary)

# Plot using ggplot2
hexmap <- ggplot() +
  geom_sf(data = hex_data, aes(fill = unique_species_count), color = "#2a2a2a", size = NA, alpha = 0.9) +
  scale_fill_viridis_c(option = "inferno", name = "Number of tree species", na.value = "#2d2d2d") +
  geom_sf(data = water, fill = "#2E2E2E", color = NA, alpha = 0.9) +
  geom_sf_text(data = labels, aes(label = name, size = area_cat),
               family = "audiowide",
               color = "white", alpha = 0.7,
               check_overlap = TRUE,
               show.legend = FALSE) + # Hide legend for labels
  scale_size_manual(values = label_sizes)

hexmap

6. Further style the map and add logo
{r}
hexmap <- hexmap +
  theme_void() +
  theme(plot.background = element_rect(fill = "#2d2d2d",
                                       color = NA),
        legend.title = element_text(family = "orbitron",
                                    size = 14, face = "bold",
                                    color = "white",
                                    hjust = 0.5),
        legend.text = element_text(family = "orbitron",
                                   size = 12,
                                   color = "white",
                                   hjust = 0.5),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.justification = "center",
        legend.background = element_rect(fill = "#2d2d2d", color = NA),
        legend.key = element_rect(fill = "#2d2d2d", color = NA),
        legend.key.height = unit(0.3, "cm"))

hexmap

{r}
library(cowplot)
# Combine plot and logo
a <- cowplot::ggdraw(hexmap) +
  cowplot::draw_image(rbanism_logo, x = 0, y = 0, width = 0.1, height = 0.1, hjust = 0, vjust = 0)

a <- a +
  # Control legend appearance
  labs(
    title = "Distribution of tree species across hexagons in Rotterdam",
    subtitle = "Hexagons representing areas of 5 Hectares with tree species counts",
    caption = paste("#30DayMapChallenge. Daniele Cannatella, 2024. Hexagon Map.\n data source: Gemeente\nRotterdam")
  ) +
  theme_void() +
  theme(plot.margin = margin(5, 5, 5, 5),
        plot.background = element_rect(fill = "#2d2d2d",
                                       color = NA),
        plot.title = element_text(family = "orbitron",
                                  size = 20,
                                  face = "bold",
                                  color = "white"), # Title styling
        plot.subtitle = element_text(family = "orbitron",
                                     size = 16,
                                     color = "white"), # Subtitle styling
        plot.caption = element_text(family = "orbitron",
                                    size = 14,
                                    color = "white",
                                    lineheight = 0.3))

7. Export the map
{r}
output_file <- "output/04Nov_hexmap.png"

# Export the map as a PNG with 1:1 aspect ratio
ggsave(filename = output_file, plot = a, device = "png",
       units = "in", width = 6, height = 6, dpi = 300)

# Print a message to confirm export
cat("Map has been exported as", output_file, "with a 1:1 aspect ratio.\n")

```