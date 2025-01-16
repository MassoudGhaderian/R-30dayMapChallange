# Install if not already installed
install.packages(c("sf", "dplyr", "ggplot2", "ggspatial", "spatstat", "hexbin"))

# Load libraries

library(sf)
library(dplyr)
library(ggplot2)
library(ggspatial)
library(spatstat)
library(hexbin)

# Replace with your file path
setwd("F:/R-WorkSpaces")
netherlands_water <- st_read("R-30dayMapChallange/04-Hexagons/data/waterINholland.shp")
netherlands_land <- st_read("R-30dayMapChallange/04-Hexagons/data/NLBorder.shp")

# Transform both datasets to the same CRS if needed
netherlands_water <- st_transform(netherlands_water, 28992)
netherlands_land <- st_transform(netherlands_land, 28992)

# Define grid size and create hexagons
hex_grid <- st_make_grid(netherlands_land, cellsize = 10000, square = FALSE) %>%
  st_as_sf() %>%
  st_cast("POLYGON")

# Calculate intersection areas
hex_water <- st_intersection(hex_grid, netherlands_water) %>%
  mutate(area_water = st_area(.))

hex_land <- st_intersection(hex_grid, netherlands_land) %>%
  mutate(area_land = st_area(.))

# Join the data back to hex_grid and calculate the proportion of water/land
hex_grid <- hex_grid %>%
  st_join(hex_water) %>%
  st_join(hex_land) %>%
  mutate(
    area_water = as.numeric(area_water),  # Convert to numeric
    area_land = as.numeric(area_land),    # Convert to numeric
    proportion_water = area_water / (area_water + area_land),
    type = ifelse(proportion_water > 0.5, "Water", "Land")
  )

# Plot the Hexagon Map
ggplot() +
  geom_sf(data = hex_grid, aes(fill = type), color = NA) +
  scale_fill_manual(values = c("Water" = "blue", "Land" = "green")) +
  theme_minimal() +
  labs(title = "Hexagon Map of Water and Land in the Netherlands",
       fill = "Type") +
  theme(legend.position = "bottom")

# Save the Map
ggsave("R-30dayMapChallange/04-Hexagons/outputs/hexagon_map_netherlands.jpg", width = 10, height = 8)

