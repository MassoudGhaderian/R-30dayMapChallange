# Load libraries
library(ggplot2)
library(sf)      # For working with spatial data
library(tmap)    # For thematic maps
library(here)    # For managing file paths
## for logo
library(magick)
library(grid)


setwd("F:/R-WorkSpaces/R-30dayMapChallange/")

# Define paths to shapefiles
disappeared_molen <- ("23-Memory/data/shp/verdwenenmolens.shp")
existing_molen <- ("23-Memory/data/shp/Molens.shp")

nl <- st_read("23-Memory/data/shp/gadm41_NLD_shp/gadm41_NLD_0.shp")


ex_molen <- st_read(here(disappeared_molen)) |> 
  st_transform(crs = st_crs(nl)) |> 
  st_crop(sf::st_bbox(nl))

molen <- st_read(existing_molen)


#inspect data
head(molen)
head(ex_molen)

colnames(molen)
colnames(ex_molen)

summary(molen)
summary(ex_molen)

nrow(ex_molen)
nrow(molen)

ncol(ex_molen)
ncol(molen)


ggplot() +
  geom_sf(data = ex_molen) +
  theme_minimal() + 
  coord_sf(datum = NA) +
  labs(title = "Basic Map Of Disappeared Molens")

ggplot() +
  geom_sf(data =molen) +
  theme_minimal() + 
  coord_sf(datum = NA) +
  labs(title = "Basic Map Of existing Molens")

#exploring type of disappeared mils
ggplot(data = ex_molen) +
  geom_sf(aes(color = type), size = 1) +
  labs(title = "Disappeared Mills in the Netherlands",
       subtitle = "With locations of existing and disappeared mills") +
  theme_minimal()



#exploring existing and disappeared mils

ggplot() +
  geom_sf(data = nl, fill = "lightblue", color = "black", alpha = 0.3) +
  geom_sf(data = molen, color = "blue", size = 0.5) +
  geom_sf(data = ex_molen , color ="brown", size = 0.5) +
  theme_minimal() +
  labs(
    title = "Existing and disappeared Mills in the Netherlands",
    subtitle = "Only windmills within the Netherlands boundary"
  )


rbanism_logo <- image_read('https://rbanism.org/assets/imgs/about/vi_l.jpg') # Download our logo

ggplot() +
  geom_sf(data = nl, fill = "lightblue", color = "black", alpha = 0.3) +
  geom_sf(data = molen, color = "blue", size = 0.5) +
  geom_sf(data = ex_molen , color ="brown", size = 0.5) +
  theme_minimal() +
  labs(
    title = "Existing and disappeared Mills in the Netherlands",
    subtitle = "Only windmills within the Netherlands boundary"
  )

grid.raster(rbanism_logo, x = 0.9, y=0.9,  # x and y determine the position of the logo (top right)
            width = unit(100, "points"))   # width determines the size of the logo


# Save the plot as a PNG file
ggsave("Molen.pdf", width =  8.27 , height = 10, dpi = 600 , path="/R-WorkSpaces/R-30dayMapChallange/23-Memory/outputs/")


