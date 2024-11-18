# Load libraries
library(ggplot2)
library(sf)      # For working with spatial data
library(tmap)    # For thematic maps
library(here)    # For managing file paths

# Define paths to shapefiles
disappeared_molen <- ("23Nov_Memory/verdwenenmolensPoint.shp")


# Read shapefiles
ex_molen <- st_read(here(disappeared_molen)) 
head(ex_molen)


ex_molen |> st_graticule()
# Inspect data
str(ex_molen)

st_bbox(ex_molen)
plot(ex_molen)

# Plot the ex_molens

ggplot() +
  geom_sf(data = ex_molen) +
  theme_minimal() + 
  coord_sf(datum = NA) +
  labs(title = "Basic Map Of Molens")

