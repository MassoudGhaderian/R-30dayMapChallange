# Load libraries
library(ggplot2)
library(sf)      # For working with spatial data
library(tmap)    # For thematic maps
library(here)    # For managing file paths
## for logo
library(magick)
library(grid)
library(cowplot)

# Define paths to shapefiles
setwd("F:/R-WorkSpaces/R-30dayMapChallange/")

disappeared_molen <- ("23-Memory/data/shp/verdwenenmolens.shp")
existing_molen <- ("23-Memory/data/shp/Molens.shp")

nl <- st_read("23-Memory/data/shp/gadm41_NLD_shp/gadm41_NLD_0.shp")
NorthSea <- st_read("F:/R-WorkSpaces/R-30dayMapChallange/01-Points/data/shp/NorthSea.shp")

ex_molen <- st_read(here(disappeared_molen)) |> 
  st_transform(crs = st_crs(nl)) |> 
  st_crop(sf::st_bbox(nl))

molen <- st_read(existing_molen)


#inspect data
head(molen)
head(ex_molen)

nrow(ex_molen)
nrow(molen)

#plotting 

main_plot <- ggplot() +
  geom_sf(data = nl, color = "black", alpha = 0.3) +
  geom_sf(data = molen, aes(color = "Existing Mills"), size = 0.5) +
  geom_sf(data = ex_molen , aes(color = "Disappeared Mills"), size = 0.5) +
  scale_color_manual(
    name = "Mills Legend",  # Legend title
    values = c("Existing Mills" = "blue", "Disappeared Mills" = "brown")
  ) +
  labs(
    title = "Existing and disappeared Mills in the Netherlands",
    subtitle = "Only windmills within the Netherlands boundary",
    caption = "#30DayMapChallenge| Data Source: PDOK-DSM | Map by Massoud Ghaderian, 2024",
    x = NULL,  # Remove x-axis title
    y = NULL,  # Remove y-axis title
  )+
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold", margin = margin(b = 0)),
    plot.subtitle = element_text(hjust = 0.5, size = 12, margin = margin(t =0)),
    plot.caption = element_text(hjust = 0, size = 10, face = "italic", margin = margin(t = 15)),
    plot.margin = margin(t = 30, r = 20, b = 50, l = 20),
    # legend.position = c(1, 0.1),   # Position legend at top-left (relative coordinates)
    legend.justification = c("left", "bottom"),  # Adjust alignment of the legend box
    # legend.box = "horizontal"   # Legend items in horizontal layout (optional)
    # Customizing major grid lines (for latitude and longitude)
    panel.grid.major = element_line(color = "lightgray", size = .5),  # Major grid lines: gray color, thickness 0.5
    
    # Customizing minor grid lines (for finer latitude and longitude)
    panel.grid.minor = element_line(color = "lightgray", size = 0.5),  # Minor grid lines: light gray, thinner
    
    # Optionally customize grid lines for x and y separately (longitude/latitude)
    panel.grid.major.x = element_line(color = "blue", size = 0.7),    # Major grid lines for x-axis (longitude)
    panel.grid.major.y = element_line(color = "green", size = 0.7),   # Major grid lines for y-axis (latitude)
    
    # Ticks for axis (optional)
    axis.ticks = element_line(color = "black", size = 1),  # Customize ticks
    axis.text = element_text(
      size = 7,  # Change font size of numbers
      color = "gray",  # Change font color
      face = "italic",  # Make numbers bold (optional)
      family = "Arial"  # Set font family (optional)
    ),
    # Moving axis labels inside the plot
    axis.text.x = element_text(
      size = 7, hjust = 0.5, vjust = 10  # Move x-axis labels to the right (hjust = 1)
    ),
    axis.text.y = element_text(
      size = 7, hjust = 1, vjust = 0.5  # Move y-axis labels up (vjust = 1.5)
    ),

    
  )


main_plot

# Convert the logo to a raster object for cowplot
rbanism_logo <- image_read('https://rbanism.org/assets/imgs/about/vi_l.jpg') # Download our logo
rbanism_logo_raster <- grid::rasterGrob(rbanism_logo, interpolate = TRUE)


# Combine the plot and logo using cowplot::ggdraw
final_plot <- ggdraw(main_plot) +
  draw_grob(rbanism_logo_raster, x = 0.75, y = 0.65, width = 0.20, height = 0.20)

final_plot

# Save the combined plot
ggsave("Molen_with_logo.pdf", plot = final_plot, 
       width = 8.27, height = 10, dpi = 600, 
       path = "/R-WorkSpaces/R-30dayMapChallange/23-Memory/outputs/")

