
# install libraries

install.packages(c("ggplot2","rasterVis")) # for mapping
install.packages(c("magick","grid")) # for logo

# Load necessary libraries
library(raster)
library(ggplot2)
library(rasterVis)

library(magick)
library(grid)

# Load the DSM raster
raster_data <- raster("f:/R-WorkSpaces/R-30dayMapChallange/06-Raster/data/DSM5m.tif")

# Review Raster -----------------------------------------------------------

print(res(raster_data)) # Cell size
print(dim(raster_data))# Dimensions
print(extent(raster_data)) # Extent
print(crs(raster_data))# Coordinate Reference System
print(ncell(raster_data))# Number of cells

# Set 0 value and coloring ------------------------------------------------

# Set values below 0 to 0
dsm_corrected <- calc(raster_data, fun = function(x) ifelse(x < 0, 0, x))

# Define a color palette with a gradient from blue to green
color_palette <- c("blue", colorRampPalette(c("red", "orange", "yellow", "green"))(99))

# save and print ----------------------------------------------------------

# Save the new raster
writeRaster(dsm_corrected, "06-Raster/outputs/TUDSM", format = "GTiff", overwrite = TRUE)

# Convert the raster to a data frame for ggplot
dsm_df <- as.data.frame(dsm_corrected, xy = TRUE)
names(dsm_df)[3] <- "elevation"
str(dsm_df)

rbanism_logo <- image_read('https://rbanism.org/assets/imgs/about/vi_l.jpg') # Download  logo


# Plot using ggplot2
p <- ggplot(dsm_df, aes(x = x, y = y, fill = elevation)) +
  geom_raster() +
  scale_fill_gradientn(colors = color_palette, na.value = "white") +
  labs(title = "Flooded TU Delft Campus",
       subtitle= "# 30𝐃a𝐲𝐌𝐚𝐩𝐂𝐡𝐚𝐥𝐥𝐞𝐧𝐠𝐞 with R-06(Raster) ",
       fill = "Elevation") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"), # Center title and adjust size
    plot.subtitle = element_text(hjust = 0.5, size = 16), # Center subtitle and adjust size
    plot.margin = margin(t = 20, b = 20) # Add margin for spacing
  ) +
  coord_equal()

p

grid.raster(rbanism_logo, x = 0.9, y=0.9,  # x and y determine the position of the logo (top right)
            width = unit(100, "points"))   # width determines the size of 

# Save the plot as a PNG file
ggsave("tu_delft_campus.png", plot = p, width = 10, height = 8, dpi = 50, path=here("06-Raster/outputs/"))


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
dsm <- raster("f:/R-WorkSpace/R-30dayMapChallange/06-Raster/data/DSM5m.tif")

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
ggsave("TUDelftDSM.png", width = 10, height = 8, dpi = 120, path = here("06-Raster/outputs/"))

