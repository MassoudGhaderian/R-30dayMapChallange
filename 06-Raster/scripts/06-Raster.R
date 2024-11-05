

# Step 1: Load Required Libraries ----------------------------------------
#install libraries
install.packages(c("raster","ggplot2","magick","grid"))   

#loading libraries
## for raster mapping
library(raster)
library(ggplot2)
library(dplyr)

## for logo
library(magick)
library(grid)

# Step 2: Load the DSM Raster Data ----------------------------------------

dsm <- raster("D:/R-WorkSpace/R-30dayMapChallange/06-Raster/data/DSM5m.tif")

# Review Raster -----------------------------------------------------------

res(dsm) # Cell size
dim(dsm)# Dimensions
extent(dsm) # Extent
crs(dsm)# Coordinate Reference System
ncell(dsm)# Number of cells
summary(dsm)

# Step 3: Plot  Map ------------------------------------------------

# Convert DSM raster to a data frame for ggplot
dsm_df <- as.data.frame(dsm, xy = TRUE)
colnames(dsm_df) <- c("x", "y", "elevation")


# Define custom color palette and bins
custom_colors <- c("black","white", "lightblue","deepskyblue","blue",
                    "orange", "red", "darkred")         
custom_bins <- c(100,50,30,20,10,5,0,-5,-10)

dsm_df <- dsm_df %>%
  mutate(fct_elevation_cb = cut(`elevation`, breaks = custom_bins))

levels(dsm_df$fct_elevation_cb)


rbanism_logo <- image_read('https://rbanism.org/assets/imgs/about/vi_l.jpg') # Download our logo

#plotting by ggplot
ggplot(dsm_df, aes(x = x, y = y, fill = fct_elevation_cb)) +
  scale_fill_manual(values = custom_colors) +
  geom_raster() +
  theme_minimal() +
  labs( title = "Elevation Map of TU Delft by Digital Surface Model (DSM) ",
        fill = "Elevation (m)",
        x = NULL,  # Remove x-axis title
        y = NULL,  # Remove y-axis title
        caption = "#30DayMapChallenge| Data Source: PDOK-DSM | Map by Massoud Ghaderian, 2024")+
  theme(
    legend.position = "right", # Move legend below the plot
    axis.text = element_text(size = 8), # Customize axis text
    panel.grid = element_blank(), # Remove grid lines
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5, margin = margin(b = 0.5)), # Center title
    plot.caption = element_text(hjust = 0.5, size = 10, color = "black"), # Center caption
    plot.margin = margin(t = 5, r = 5, b = 5, l = 5) # Adjust margins
  )

grid.raster(rbanism_logo, x = 0.9, y=0.9,  # x and y determine the position of the logo (top right)
            width = unit(100, "points"))   # width determines the size of the logo


# Save the plot as a PNG file
ggsave("TUDelftDSM.pdf", width =  8.27 , height = 10, dpi = 600)

