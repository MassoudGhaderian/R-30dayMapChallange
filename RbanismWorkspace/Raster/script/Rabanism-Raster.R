#install libraries
install.packages(c("tidyverse", "terra", "viridis"))   
library(tidyverse)
library(terra)
library(viridis)


# View Raster File Attributes ---------------------------------------------
describe("D:/R-WorkSpace/R-30dayMapChallange/RbanismWorkspace/Raster/data/DSM5m.tif")



# Open a Raster in R ------------------------------------------------------

DSM_TUD <- rast("D:/R-WorkSpace/R-30dayMapChallange/RbanismWorkspace/Raster/data/DSM5m.tif")
DSM_TUD


summary(DSM_TUD)


summary(values(DSM_TUD))

# Reproject if needed
DSM_TUD <- project(DSM_TUD, "EPSG:4326")

DSM_TUD_df <- as.data.frame(DSM_TUD, xy=TRUE)
str(DSM_TUD_df)
colnames(DSM_TUD_df) <- c("x", "y", "elevation")
colnames(DSM_TUD_df)



ggplot() +
  geom_raster(data = DSM_TUD_df , aes(x = x, y = y, fill = `elevation`)) + # Use actual column name if different
  scale_fill_viridis_c(option = "H") +
  coord_quickmap()


DSM_TUD_df <- DSM_TUD_df %>%
  mutate(fct_elevation = cut(`elevation`, breaks = 3))

ggplot() +
  geom_bar(data = DSM_TUD_df, aes(fct_elevation))

levels(DSM_TUD_df$fct_elevation)
DSM_TUD_df %>% 
  count(fct_elevation)

custom_bins <- c(-10, 0, 5, 100)

DSM_TUD_df <- DSM_TUD_df %>%
  mutate(fct_elevation_cb = cut(`elevation`, breaks = custom_bins))

levels(DSM_TUD_df$fct_elevation_cb)

ggplot() +
  geom_bar(data = DSM_TUD_df, aes(fct_elevation_cb))


DSM_TUD_df %>% 
  count(fct_elevation_cb)

terrain.colors(3)

ggplot() +
  geom_raster(data = DSM_TUD_df , aes(x = x, y = y, fill = fct_elevation_cb)) + 
  scale_fill_manual(values = terrain.colors(3)) + 
  coord_quickmap()


my_col <- terrain.colors(3)

ggplot() +
  geom_raster(data = DSM_TUD_df , aes(x = x, y = y,
                                      fill = fct_elevation_cb)) + 
  scale_fill_manual(values = my_col, name = "Elevation") + 
  coord_quickmap()


ggplot() +
  geom_raster(data = DSM_TUD_df , aes(x = x, y = y,
                                      fill = fct_elevation_cb)) + 
  scale_fill_manual(values = my_col, name = "Elevation") +
  theme(axis.title = element_blank()) + 
  coord_quickmap()
