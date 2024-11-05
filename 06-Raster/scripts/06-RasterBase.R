

# Step 1: Load Required Libraries ----------------------------------------
#install libraries
install.packages(c("raster","ggplot2","magick","grid"))   

#loading libraries
## for raster mapping
library(raster)
library(ggplot2)
## for logo
library(magick)
library(grid)

# Step 2: Load the DSM Raster Data ----------------------------------------


# Load DSM file
dsm <- raster("D:/R-WorkSpace/R-30dayMapChallange/06-Raster/data/DSM5m.tif")

#review and check DSM file
# Review Raster -----------------------------------------------------------

print(res(dsm)) # Cell size
print(dim(dsm))# Dimensions
print(extent(dsm)) # Extent
print(crs(dsm))# Coordinate Reference System
print(ncell(dsm))# Number of cells

# Step 3: Plot a Basic Map ------------------------------------------------


#Using plot:
plot(dsm, main = "DSM - Elevation Map", col = terrain.colors(100))


#Using ggplot2:
# Convert DSM raster to a data frame for ggplot
dsm_df <- as.data.frame(dsm, xy = TRUE)
colnames(dsm_df) <- c("x", "y", "elevation")

# Plot with ggplot2

rbanism_logo <- image_read('https://rbanism.org/assets/imgs/about/vi_l.jpg') # Download our logo

ggplot(dsm_df, aes(x = x, y = y, fill = elevation)) +
  geom_raster() +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(title = "Digital Surface Model (DSM)", fill = "Elevation (m)")

grid.raster(rbanism_logo, x = 0.9, y=0.9,  # x and y determine the position of the logo (top right)
            width = unit(100, "points"))   # width determines the size of the logo


# Save the plot as a PNG file
ggsave("TUDelftDSM.png", width = 10, height = 8, dpi = 120)
