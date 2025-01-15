# Load necessary libraries
library(raster)
library(dplyr)
library(ggplot2)

# Load the DSM raster
dsm <- raster("F:/R-WorkSpaces/R-30dayMapChallange/06-Raster/data/DSM5m.tif")

# Convert DSM raster to a data frame for ggplot
dsm_df <- as.data.frame(dsm, xy = TRUE)
colnames(dsm_df) <- c("x", "y", "elevation")

# Define custom color palette and bins
custom_colors <- c("deepskyblue", "lightblue", "white", "#F9DDB1", "#F1B04C",
                   "#F7931E", "red", "darkred")
custom_bins <- c(-10, -5, 0, 5, 10, 20, 30, 50, 100)  # Ensure bins are in ascending order

# Apply binning to elevation values
dsm_df <- dsm_df %>%
  mutate(fct_elevation_cb = cut(elevation, breaks = custom_bins, include.lowest = TRUE))

# Plotting with ggplot
ggplot(dsm_df, aes(x = x, y = y, fill = fct_elevation_cb)) +
  geom_raster() +
  scale_fill_manual(values = custom_colors, name = "Elevation (m)") +  # Set legend title
  theme_minimal() +
  labs(
    title = "Elevation Map of TU Delft by Digital Surface Model (DSM)",
    x = NULL,  # Remove x- axis title
    y = NULL,  # Remove y- axis title
    caption = "#30DayMapChallenge | Data Source: PDOK-DSM | Map by Massoud Ghaderian, 2024"
  ) +
  theme(
    legend.position = "right",
    axis.text = element_text(size = 8),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5, margin = margin(b = 10)),
    plot.caption = element_text(hjust = 0.5, size = 10, color = "black"),
    plot.margin = margin(t = 5, r = 5, b = 5, l = 5)
  )

