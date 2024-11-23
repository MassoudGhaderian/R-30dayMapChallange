# Install necessary package
install.packages("ggspatial")  # For adding north arrows and scale bars to maps

# Load required libraries
library(ggplot2)       # For creating plots
library(sf)            # For working with spatial data
library(tmap)          # For thematic mapping
library(here)          # For managing file paths
library(magick)        # For image manipulation (logo)
library(grid)          # For working with grid graphics
library(cowplot)       # For combining plots and adding elements (e.g., logos)
library(ggspatial)     # For scale bars and north arrows in ggplot maps

# Set working directory (modify as needed)
setwd("F:/R-WorkSpaces/R-30dayMapChallange/")

# SECTION 1: DEFINE PATHS AND LOAD SPATIAL DATA
# Paths to shapefiles
disappeared_molen <- "23-Memory/data/shp/verdwenenmolens.shp"  # Disappeared mills
existing_molen <- "23-Memory/data/shp/Molens.shp"             # Existing mills

# Read shapefiles for various spatial features
north_sea <- st_read("F:/R-WorkSpaces/R-30dayMapChallange/23-Memory/data/shp/NorthSea.shp")
gr_border <- st_read("23-Memory/data/shp/GR.shp")            # Groningen border
bl_border <- st_read("23-Memory/data/shp/BG.shp")            # Friesland border
nl_border <- st_read("23-Memory/data/shp/gadm41_NLD_shp/gadm41_NLD_0.shp")  # Netherlands national border
nl_stats_border <- st_read("F:/R-WorkSpaces/R-30dayMapChallange/23-Memory/data/shp/gadm41_NLD_shp/gadm41_NLD_1.shp")  # Province borders
nl_cities_border <- st_read("F:/R-WorkSpaces/R-30dayMapChallange/23-Memory/data/shp/gadm41_NLD_shp/gadm41_NLD_2.shp")  # City borders
oppervlaktewater <- st_read("F:/R-WorkSpaces/R-30dayMapChallange/23-Memory/data/shp/oppervlaktewater.shp")  # Surface water
nl_populated_palces <- st_read("F:/R-WorkSpaces/R-30dayMapChallange/23-Memory/data/shp/populated_places.shp")  # Populated places

# Inspect the first few rows of the populated places data
head(nl_populated_palces)

# Load and preprocess shapefiles for disappeared mills
ex_molen <- st_read(here(disappeared_molen)) |> 
  st_transform(crs = st_crs(nl_border)) |> 
  st_crop(sf::st_bbox(nl_border))  # Crop to Netherlands' bounding box

# Load existing mills shapefile
molen <- st_read(existing_molen)

# SECTION 2: DATA INSPECTION
# Inspect first few rows of each dataset
head(molen)
head(ex_molen)

# Summarize datasets
summary(molen)
summary(ex_molen)

# Count number of features in each dataset
nrow(ex_molen)  # Count of disappeared mills
nrow(molen)     # Count of existing mills

# Get the bounding box of the Netherlands shapefile
bbox <- st_bbox(nl_border)

# SECTION 3: CREATE MAIN PLOT
main_plot <- ggplot() +
  # Add geographic features
  geom_sf(data = north_sea, fill = "lightblue", color = NA, alpha = 0.5) +  # North Sea
  geom_sf(data = gr_border, color = NA, alpha = 0.5) +                     # Groningen border
  geom_sf(data = bl_border, color = NA, alpha = 0.5) +                     # Friesland border
  geom_sf(data = nl_stats_border, fill = NA, color = "white") +            # Province borders
  geom_sf(data = nl_border, fill = "black", color = NA, alpha = 0.3) +     # Netherlands national border
  geom_sf(data = oppervlaktewater, fill = "lightblue", color = NA) +       # Surface water
  
  # Add mills data
  geom_sf(data = ex_molen, aes(color = "Disappeared Mills"), size = 0.5) +  # Disappeared mills
  geom_sf(data = molen, aes(color = "Existing Mills"), size = 0.5) +        # Existing mills
  
  # Add populated places
  geom_sf(data = nl_populated_palces, aes(shape = "circle"), size = 2, show.legend = FALSE) +
  
  # Customize color legend for mills
  scale_color_manual(
    name = "Mills Legend",  # Legend title
    values = c("Existing Mills" = "#993404", "Disappeared Mills" = "#fec44f")
  ) +
  guides(
    color = guide_legend(override.aes = list(size = 3))  # Customize legend symbols
  ) +
  
  # Add labels, title, and captions
  labs(
    title = "▪ Mills in the Netherlands",
    subtitle = "▪ Existing and disappeared",
    caption = "▪ #30DayMapChallenge| Data Source: www.molendatabase.org | Map by Massoud Ghaderian, 2024",
    x = NULL,  # Remove x-axis label
    y = NULL   # Remove y-axis label
  ) +
  
  # Set the map extent to the bounding box
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), 
           ylim = c(bbox["ymin"], bbox["ymax"])) +
  
  # Apply minimal theme with customizations
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = -0.01, size = 16, face = "bold", margin = margin(b = 0)),
    plot.subtitle = element_text(hjust = -0.01, size = 12, margin = margin(t = 0)),
    plot.caption = element_text(hjust = -0.01, size = 10, face = "italic", margin = margin(t = 15)),
    plot.margin = margin(t = 30, r = 20, b = 50, l = 20)
  ) +
  
  # Add a north arrow
  annotation_north_arrow(
    location = "bl",
    which_north = "true",
    style = north_arrow_fancy_orienteering(fill = c("white", "white"), line_col = "black"),
    height = unit(1, "cm"),
    width = unit(1, "cm"),
    pad_x = unit(1.7, "cm"),
    pad_y = unit(1, "cm")
  ) +
  
  # Add a scale bar
  annotation_scale(
    location = "bl",
    width_hint = 0.2,
    line_width = 1,
    height = unit(0.1, "cm"),
    pad_x = unit(1.25, "cm"),
    pad_y = unit(.75, "cm"),
    bar_cols = c("white", "white")
  )

# Display the main plot
main_plot

# SECTION 4: ADD LOGO AND EXPORT FINAL PLOT
# Read and convert logo to raster
rbanism_logo <- image_read('https://rbanism.org/assets/imgs/about/vi_l.jpg')  # Download logo
rbanism_logo_raster <- grid::rasterGrob(rbanism_logo, interpolate = TRUE)

# Combine main plot with logo using cowplot
final_plot <- ggdraw(main_plot) +
  draw_grob(rbanism_logo_raster, x = 0.76, y = 0.75, width = 0.20, height = 0.20)

# Display the final plot
final_plot

# Save the final plot as a PDF
ggsave("Molen_ex.pdf", plot = final_plot, 
       width = 8.27, height = 10, dpi = 600, 
       path = "/R-WorkSpaces/R-30dayMapChallange/23-Memory/outputs/")
