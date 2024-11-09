# Load necessary library
library(raster)

# Load the DSM raster
raster_data <- raster("/R-WorkSpace/R-30dayMapChallange/06-Raster/data/Raster/TUDelft/DSM.tif")
# raster_data <- raster("/R-WorkSpace/R-30dayMapChallange/06-Raster/data/Raster/TUDelft/DTM.tif")

# Review Raster -----------------------------------------------------------

# Cell size (in the units of the raster's coordinate reference system)
res(raster_data)

# Dimensions (number of rows and columns)
dim(raster_data)

# Extent (minimum and maximum coordinates in x and y directions)
extent(raster_data)

# Coordinate Reference System
crs(raster_data)

# Number of cells
ncell(raster_data)



# Set 0 value and coloring ------------------------------------------------


# Set values below 0 to 0
dsm_corrected <- calc(raster_data, fun = function(x) ifelse(x < 0, 0, x))

# Define a color palette with light blue for 0 and a gradient for higher values
# color_palette <- colorRampPalette(c("lightblue", "yellow", "orange", "red"))(100)
# color_palette <- c("blue", colorRampPalette(c("yellow", "orange", "red"))(99))
color_palette <- c("blue", colorRampPalette(c("red", "orange","yellow", "green"))(99))

# save and print ----------------------------------------------------------


# Save the new raster
writeRaster(dsm_corrected, "06-Raster/outputs/TUDSM", format = "GTiff", overwrite = TRUE)

# Plot to verify with custom colors
plot(dsm_corrected, main = "Flooded TU Delft Campus", col = color_palette)

