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
colnames(DSM_TUD_df)


ggplot() +
  geom_raster(data = DSM_TUD_df , aes(x = x, y = y, fill = `DSM5m`)) + # Use actual column name if different
  scale_fill_viridis_c(option = "H") +
  coord_quickmap()

