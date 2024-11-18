# Load libraries
library(ggplot2)
library(sf)      # For working with spatial data
library(tmap)    # For thematic maps
library(here)    # For managing file paths

# Define paths to shapefiles
disappeared_molen <- ("F:/R-WorkSpaces/R-30dayMapChallange/23-Memory/data/shp/1/verdwenenmolens/verdwenenmolensPoint.shp")


# Read shapefiles
ex_molen <- st_read(disappeared_molen)

# Inspect data
colnames(ex_molen)

head(ex_molen)

# Plot the ex_molens

ggplot(data = ex_molen) +
  geom_sf () +
  theme_minimal() + 
  labs(title = "Basic Map Of Molens")

