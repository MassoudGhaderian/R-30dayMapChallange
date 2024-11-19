# Load libraries
library(ggplot2)
library(sf)      # For working with spatial data
library(tmap)    # For thematic maps
library(here)    # For managing file paths

# Define paths to shapefiles
disappeared_molen <- ("23-Memory/data/shp/verdwenenmolens.shp")

nl <- st_read("23-Memory/data/shp/gadm41_NLD_shp/gadm41_NLD_0.shp")
head(nl)
ex_molen <- st_read(here(disappeared_molen)) |> 
  st_transform(crs = st_crs(nl)) |> 
  st_crop(sf::st_bbox(nl))

ggplot() +
  geom_sf(data = ex_molen) +
  theme_minimal() + 
  coord_sf(datum = NA) +
  labs(title = "Basic Map Of Molens")


#exploring type of disappeared mils
ggplot(data = ex_molen) +
  geom_sf(aes(color = type), size = 1) +
  labs(title = "Existing and Disappeared Mills in the Netherlands",
       subtitle = "With locations of existing and disappeared mills") +
  theme_minimal()
