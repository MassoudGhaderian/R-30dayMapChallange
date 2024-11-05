#install libraries
install.packages(c("tidyverse","terra"))   

library(tidyverse)
library(terra)


# View Raster File Attributes ---------------------------------------------
describe("D:/R-WorkSpace/30DayMapChallenge2024 by Rbanism/RbanismWorkspace/Raster/data/DSM5m.tif")



# Open a Raster in R ------------------------------------------------------

DSM_TUD <- rast("D:/R-WorkSpace/30DayMapChallenge2024 by Rbanism/RbanismWorkspace/Raster/data/DSM5m.tif")
DSM_TUD


summary(DSM_TUD)


summary(values(DSM_TUD))


DSM_TUD_df <- as.data.frame(DSM_TUD, xy=TRUE)
str(DSM_TUD_df)


ggplot() +
  geom_raster(data = DSM_TUD_df , aes(x = x, y = y, fill = `DSM5m`)) +
  scale_fill_viridis_c(option = "H") +  # `option = "H"` provides a contrasting colour scale
  coord_quickmap() 
