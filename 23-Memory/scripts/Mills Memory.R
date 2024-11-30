

# SECTION 0:  Necessary Packages -----------------------------------

# List of required packages
required_packages <- c("ggspatial", "ggplot2", "sf", "tmap", "here", "magick",
                       "grid", "cowplot" , "gganimate","gifski","leaflet",
                       "leaflet.extras", "viridis" ,"dplyr")

# Install packages that are not already installed
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}
# Load required libraries
library(ggplot2)       # For creating plots
library(sf)            # For working with spatial data
library(tmap)          # For thematic mapping
library(here)          # For managing file paths
library(magick)        # For image manipulation 
library(grid)          # For working with grid graphics
library(cowplot)       # For combining plots and adding elements (e.g., logos)
library(ggspatial)     # For scale bars and north arrows in ggplot maps
library(leaflet)       # For building interactive maps
library(leaflet.extras)# For Extends the capabilities of leaflet 
library(viridis)       # For a visually appealing colors
library(dplyr)


# SECTION 00: Load Spatial Data ---------------------------


#  Disappeared (water and polder) mills shape file path
disappeared_mills <- here("23-Memory", "data", "shp", "verdwenenmolens.shp")  
#  Existing (water and Polder) mills shape file  path
existing_mills <- here("23-Memory", "data", "shp", "Molens.shp")  
#  Existing (water and Polder) mills shape file path
other_mills <- here("23-Memory", "data", "shp", "weidemolens en windmotoren.shp") 

# Read shape files for various spatial features
north_sea <- st_read(here("23-Memory", "data", "shp", "NorthSea.shp"))
# Netherlands, Germany and Belgium border shape file reading
gr_border <- st_read(here("23-Memory", "data", "shp", "GR.shp"))
bl_border <- st_read(here("23-Memory", "data", "shp", "BG.shp"))
nl_border <- st_read(here("23-Memory", "data", "shp", "gadm41_NLD_0.shp"))
#border of stats and cities in Netherlands shape file reading
nl_stats_border <- st_read(here("23-Memory", "data", "shp", "gadm41_NLD_1.shp"))
nl_cities_border <- st_read(here("23-Memory", "data", "shp", "gadm41_NLD_2.shp"))
#Surface water and Populated places shape file in Netherlands shape file reading
oppervlaktewater <- st_read(here("23-Memory", "data", "shp", "oppervlaktewater.shp"))
nl_populated_palces <- st_read(here("23-Memory", "data", "shp", "populated_places.shp"))


# Load and preprocess shape files for disappeared mills
ex_mills <- st_read(here(disappeared_mills)) |> 
  st_transform(crs = st_crs(nl_border)) |> 
  st_crop(sf::st_bbox(nl_border))  # Crop to Netherlands' bounding box

# Load existing mills shapefile
mills <- st_read(existing_mills)
other_mills <- st_read(other_mills)

# Calculate centroids for the borders for labling and other..
nl_stats_border <- nl_stats_border %>%
  mutate(
    x = st_coordinates(st_centroid(geometry))[, 1],
    y = st_coordinates(st_centroid(geometry))[, 2]
  )

# SECTION 000: Data Inspection ----------------------------------------------

# Inspect first few rows of each data set
head(mills)
head(ex_mills)
head(other_mills)

print(nl_populated_palces)
head(nl_stats_border)

# Summarize datasets
summary(mills)
summary(ex_mills)
summary(other_mills)

# Count number of features in each data set
num_disappeared_mills <- nrow(ex_mills)
cat("Total disappeared mills:", num_disappeared_mills, "\n")
num_existing_mills <- nrow(mills)
cat("Total existing mills:", num_existing_mills, "\n")
num_other_mills <- nrow(other_mills)
cat("Total other mills:", num_other_mills, "\n")

bbox <- st_bbox(nl_border)
bbox
# SECTION 1: Existing and  Disappeared Mills Maps ---------------------------------------------

# Get the bounding box of the Netherlands shape file


main_plot <- ggplot() +
  # Add geographic background features
  geom_sf(data = north_sea, fill = "lightblue", color = NA, alpha = 0.5) + # North Sea
  geom_sf(data = gr_border, color = NA, alpha = 0.5) +                     # Germany border
  geom_sf(data = bl_border, color = NA, alpha = 0.5) +                     # Belgium border
  geom_sf(data = oppervlaktewater, fill = "lightblue", color = NA, alpha = 0.5) +       # Surface water in Netherlands
  geom_sf(data = nl_border, fill = "black", color = NA, alpha = 0.3) +     # Netherlands national border (dark mode)
  # geom_sf(data = nl_border, fill = NA, color = "black") +     # Netherlands national border (light mode)
  geom_sf(data = nl_stats_border, fill = NA , color = NA) +            # Netherlands Province borders
  # Add mills data
  geom_sf(data = ex_mills, aes(color = "Disappeared Mills"), size = 0.5) +  # Disappeared mills
  geom_sf(data = mills, aes(color = "Existing Mills"), size = 0.5) +        # Existing mills
  # Add populated cities 
  geom_sf(data = nl_populated_palces, aes(shape = "circle"), size = 2, show.legend = FALSE) +
  # geom_text(data = nl_populated_palces,                     #white laabeling
  #           aes(x = st_coordinates(geometry)[, 1], 
  #               y = st_coordinates(geometry)[, 2], 
  #               label = name),
  #           size = 3.5,  # Adjust size of halo text
  #           color = "white",  # Halo color
  #           fontface = "bold", 
  #           nudge_y = 0.05,  # Adjust vertical position
  #           nudge_x = 0, 
  #           check_overlap = TRUE,
  #           family = "sans",
  #           alpha = 0.7) +  # Slight transparency for halo effect
  
  # Add main text labels (smaller, black text on top)       #dark labaling
  geom_text(data = nl_populated_palces, 
            aes(x = st_coordinates(geometry)[, 1], 
                y = st_coordinates(geometry)[, 2], 
                label = name),
            size = 3,  # Adjust size of label
            color = "black",  # Main label color
            fontface = "bold", 
            nudge_y = 0.05,  # Adjust vertical position
            nudge_x = 0, 
            check_overlap = FALSE) +  # Prevent overlap of labels
  # Customize color legend for mills
  scale_color_manual(
    name = "▪ Legend",  # Legend title
    values = c(
      "Existing Mills" = "#993404",  # Darker brown for existing mills
      "Disappeared Mills" = "#fec44f"  # Lighter yellow for disappeared mills
    ),
    labels = c(
      paste0("Disappeared Mills\n (", num_disappeared_mills, ")"),
      paste0("Existing Mills \n (", num_existing_mills, ")")
    )
  ) +
  guides(
    color = guide_legend(override.aes = list(size = 3))  # Customize legend symbols
  )+
  # Add title, subtitle, and captions
  labs(
    title = "▪ Mills' Memory",
    subtitle = "▪ Existing and Disappeared Mills in Netherlands",
    caption = "▪ Data Source: www.molendatabase.org | Map visualization by Massoud Ghaderian | 2024 | R Studio",
    x = NULL,  # Remove x-axis label
    y = NULL   # Remove y-axis label
  ) +
  
  # Set the map extent to the bounding box
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), 
           ylim = c(bbox["ymin"], bbox["ymax"])) +
  
  # Apply minimal theme with customizations
  theme_minimal() +
  theme(
    #Plot Elements
    plot.title = element_text(hjust = -0.01, size = 18, face = "bold", margin = margin(b = 0)),
    plot.subtitle = element_text(hjust = -0.01, size = 14, margin = margin(t = 0)),
    plot.caption = element_text(hjust = -0.01, size = 10, face = "italic", margin = margin(t = 15)),
    plot.margin = margin(t = 30, r = 20, b = 50, l = 20),
   
    #Legend settings
    # legend.position = c(0.95, 0.05),  # x and y position (percent of plot)
    legend.justification = c("right", "bottom"),  # Align legend's bottom-right corner
    # legend.box.margin = margin(5, 5, 5, 5),  # Add some space around the legend
    legend.background = element_rect(fill = "white", color = "white", size = 0.5),  # Optional: Add background and border to legend
    legend.text = element_text(size = 10),  # Increase size to 10 (adjust as needed)
    legend.title = element_text(size = 12),  # Increase legend title size
    legend.spacing.y = unit(1, "cm"),  # Adjust vertical spacing
    
    
    # Customizing  grid lines (for finer latitude and longitude)
    panel.grid.major = element_line(color = "lightgray", size = 0.5),  # Major grid lines: gray color, thickness 0.
    panel.grid.minor = element_line(color = "lightgray", size = 0.5),  # Minor grid lines: light gray, thinner
    
    # Ticks for axis (optional)
    axis.ticks.x = element_line(color = "darkgray", size = 1),  # Ticks for top
    axis.ticks.y = element_line(color = "darkgray", size = 1),  # Ticks for right
    
    # change axis labels style
    axis.text = element_text(
      size = 7,  # Change font size of numbers
      color = "darkgray",  # Change font color
      face = "italic",  # Make numbers bold (optional)
      family = "sans"  # Set font family (optional)
    ),
    # Moving axis labels inside the plot
    axis.text.x = element_text(
      size = 5, hjust = 0.5, vjust = 1 ,margin = margin(t = -10),  # Move x-axis labels to the right (hjust = 1)
    ),
    axis.text.y = element_text(
      size = 5, hjust = 0.5, vjust =0.5,margin = margin(r = -20),  # Move y-axis labels up (vjust = 1.5)
    ),
  ) +
  # Add a north arrow
  annotation_north_arrow(
    location = "bl", # Position: 'tl' = top-left, 'tr' = top-right, etc.
    which_north = "true", # "true" for true north, "grid" for grid north
    style = north_arrow_fancy_orienteering(fill = c("white", "white"), line_col = "black"),# Choose a style for the north arrow
    
    height = unit(1, "cm"),  # Adjust size
    width = unit(1, "cm"),
    pad_x = unit(2.5, "cm"),# Horizontal padding
    pad_y = unit(1, "cm")  # Vertical padding# Adjust size
  ) +
  # Add a scale bar
  annotation_scale(
    location = "bl", # Position: 'bl' = bottom-left
    width_hint = 0.2, # Adjust the width relative to the map
    line_width = 1,
    height = unit(0.1, "cm"), # Adjust the height of the scale bar
    pad_x = unit(1.7, "cm"),
    pad_y = unit(.75, "cm"),
    bar_cols = c("white", "white")
  )

# Display the main plot
main_plot


# Read and convert logo to raster
rbanism_logo <- image_read('https://rbanism.org/assets/imgs/about/vi_l.jpg')  # Download logo
rbanism_logo_raster <- grid::rasterGrob(rbanism_logo, interpolate = TRUE)

# Combine main plot with logo using cowplot
final_plot <- ggdraw(main_plot) +
  draw_grob(rbanism_logo_raster, x = 0.80, y = 0.75, width = 0.20, height = 0.20)

# Display the final plot
final_plot

# Save the final plot as a PDF
ggsave("Exiting and Disappeared Mills.jpg", plot = final_plot, 
       width = 8.27, height = 10, dpi = 300, 
       path = here("23-Memory/outputs"))



# SECTION 2 : Interactive Map of Disappeared Mills --------------------------

# Load the necessary libraries
library(leaflet)
library(sf)

# Load and preprocess shape files for disappeared mills
ex_mills <- st_read(here(disappeared_mills)) |> 
  st_transform(crs = st_crs(nl_border)) |> 
  st_crop(sf::st_bbox(nl_border))  # Crop to Netherlands' bounding box
head(ex_mills) 

#plotting map
main_plot <- ggplot() +
  # Add mills data
  geom_sf(data = ex_mills, aes(color = "Disappeared Mills"), size = 0.5)   # Disappeared mills

main_plot  
  
  # Create the map
leaflet_map <- leaflet(data = ex_mills) %>%
  addTiles() %>%
  addCircleMarkers(
    lat = ~st_coordinates(ex_mills)[, 2],   # Latitude from geometry
    lng = ~st_coordinates(ex_mills)[, 1],   # Longitude from geometry
    color = "#fec44f",                      # Circle color (yellow)
    radius = 8,                            # Size of the yellow circle
    popup = ~paste("<b>", molen_naam, "</b><br>",  # Show the name of the mill
                   "<a href='", infolink, "' target='_blank'>Click here for more info</a>")  # Link to website
  ) %>%
  addCircleMarkers(
    lat = ~st_coordinates(ex_mills)[, 2],   # Latitude for black point
    lng = ~st_coordinates(ex_mills)[, 1],   # Longitude for black point
    color = "black",                        # Color of the small black point
    radius = 1,                             # Smaller size of the black point
    opacity = 1
  )


# Display the map
leaflet_map


# Save the map as an HTML file
library(htmlwidgets)
saveWidget(leaflet_map, "23-Memory/outputs/Disappeared Mills.html")



# SECTION 3 : A Heat map of "Disappearanced milles"  --------------------------

# Make sure it is projected for spatial analysis (e.g., EPSG: 3857 for meters)
ex_mills <- st_transform(ex_mills, crs = 3857) 

# Load the Netherlands border shapefile
# Replace "path_to_netherlands_shapefile" with the actual path to your shapefile
netherlands_border <- st_read("23-Memory/data/shp/gadm41_NLD_0.shp")
netherlands_border <- st_transform(netherlands_border, crs = 3857)

# Extract coordinates for disappeared mills
mills_coords <- st_coordinates(ex_mills)
# Convert mills to a data frame for ggplot2
mills_df <- data.frame(x = mills_coords[, 1], y = mills_coords[, 2])

# Create the heat map with the Netherlands border
heatmap_plot <- ggplot() +
  # Add the heat map
  stat_density_2d(data = mills_df, aes(x = x, y = y, fill = ..density..), geom = "raster", contour = FALSE,alpha = .8) +
  scale_fill_viridis(option = "magma", name = "Density") +  # Use a magma color palette
  # Add the  border of Netherlands and Province
  geom_sf(data = netherlands_border, fill = NA, color = NA, size = 0.5) +
  geom_sf(data = nl_stats_border, fill = NA , color = "white") +    
  #Add populated cites and their labels 
  geom_sf(data = nl_populated_palces, aes(shape = "circle"), size = 2,color = "white" ,show.legend = FALSE) +
  geom_text(data = nl_populated_palces, 
            aes(x = st_coordinates(geometry)[, 1], 
                y = st_coordinates(geometry)[, 2], 
                label = name),
            size = 3,  # Adjust size of label
            color = "white",  # Main label color
            fontface = "bold", 
            nudge_y = 0.05,  # Adjust vertical position
            nudge_x = 0, 
            check_overlap = FALSE) +  # Prevent overlap of labels
  # Add title, subtitle, and captions
  labs(title = "▪ Mills' Memory",
       subtitle = "▪ Density Heatmap of Disappeared Mills in Netherlands ",
       caption = "▪ Data Source: www.molendatabase.org | Map visualization by Massoud Ghaderian | 2024 | R Studio",
       x = NULL,
       y = NULL) +
  theme_minimal() +
  theme(
    #black background
    plot.background = element_rect(fill = "black", color = NA),
    # panel.background = element_rect(fill = "black", color = NA),
    
    
    #Plot Elements
    plot.title = element_text(hjust = -0.01, size = 18, face = "bold",
                              margin = margin(b = 0), color = "white"),
    plot.subtitle = element_text(hjust = -0.01, size = 14,
                                 margin = margin(t = 0), color = "white"),
    plot.caption = element_text(hjust = -0.01, size = 10, face = "italic",
                                margin = margin(t = 15), color = "white"),
    plot.margin = margin(t = 30, r = 20, b = 50, l = 20),
    
    #Legend settings
    # legend.position = c(0.95, 0.05),  # x and y position (percent of plot)
    legend.justification = c("right", "bottom"),  # Align legend's bottom-right corner
    # legend.box.margin = margin(5, 5, 5, 5),  # Add some space around the legend
    legend.background = element_rect(fill = "black", color = NA, size = 0.5),  
    legend.text = element_text(size = 10, color = "white"),  
    legend.title = element_text(size = 12, color = "white"),  
    legend.spacing.y = unit(1, "cm"),  # Adjust vertical spacing
    
    
    # Customizing  grid lines (for finer latitude and longitude)
    panel.grid.major = element_line(color = "white", size = 0.5), 
    panel.grid.minor = element_line(color = "white", size = 0.5),

    # Ticks for axis (optional)
    axis.ticks.x = element_line(color = "white", size = 1),  # Ticks for top
    axis.ticks.y = element_line(color = "white", size = 1),  # Ticks for right
    
    # change axis labels style
    axis.text = element_text(
      size = 7,  # Change font size of numbers
      color = "white",  # Change font color
      face = "italic",  # Make numbers bold (optional)
      family = "sans"  # Set font family (optional)
    ),
    # Moving axis labels inside the plot
    axis.text.x = element_text(
      size = 5, hjust = 0.5, vjust = 1 ,margin = margin(t = 0),
      # Move x-axis labels to the right (hjust = 1)
    ),
    axis.text.y = element_text(
      size = 5, hjust = 0.5, vjust =0.5,margin = margin(r = 0),
      # Move y-axis labels up (vjust = 1.5)
    ),
  ) +
  # Add a north arrow
  annotation_north_arrow(
    location = "bl", # Position: 'tl' = top-left, 'tr' = top-right, etc.
    which_north = "true", # "true" for true north, "grid" for grid north
    style = north_arrow_fancy_orienteering(
      fill = c("white", "white"), # Arrow fill colors
      line_col = "black",         # Border color for arrows
      text_col = "white"            # Set the color of the "N"
    ),
    height = unit(1, "cm"),  # Adjust size
    width = unit(1, "cm"),
    pad_x = unit(2.5, "cm"),# Horizontal padding
    pad_y = unit(1, "cm")  # Vertical padding# Adjust size
  ) +
  # Add a scale bar
  annotation_scale(
    location = "bl", # Position: 'bl' = bottom-left
    width_hint = 0.2, # Adjust the width relative to the map
    line_width = 1,
    height = unit(0.1, "cm"), # Adjust the height of the scale bar
    pad_x = unit(1.7, "cm"),
    pad_y = unit(.75, "cm"),
    bar_cols = c("white", "white"),
    text_col = "white"  # Change the scale bar text color
    )

# Display the heatmap
print(heatmap_plot)

# Save the final plot as a PDF
ggsave("Heat Map of  Disappeared Mills.jpg", plot = heatmap_plot, 
       width = 8.27, height = 10, dpi = 300, 
       path = here("23-Memory/outputs"))


# SECTION 4 : " histogram of Disappearance Years"  --------------------------


##  Data Preparation  --------------------------------------------

head(ex_mills)
st_crs(ex_mills)
num_disappeared_mills <- nrow(ex_mills)
cat("Total disappeared mills:", num_disappeared_mills, "\n")

# Convert 'verdwenen1' to numeric for disappeared mills
ex_mills <- ex_mills[ex_mills$verdwenen1 != 0, ] #to reject 0 values 
ex_mills$year <- as.integer(ex_mills$verdwenen1)  #to reject NA and invalid values
ex_mills$year
summary(ex_mills$year)



##  outliers checking and histogram --------------------------------------------

# Boxplot to check for outliers in general
boxplot(ex_mills$year, main = "Boxplot of Year", ylab = "Year")


# Calculate Z-scores
z_scores <- scale(ex_mills$year)

# Identify outliers with Z-scores greater than 3 or less than -3
outliers_Z <- ex_mills$year[abs(z_scores) > 3]
outliers_Z


# Plot histogram of 'year' and highlight outliers
hist(ex_mills$year, main = "Histogram of Year and numbers of Disapperance", 
     xlab = "Year", col = "#fec44f", breaks = 30, 
     xlim = c(min(ex_mills$year) - 10, max(ex_mills$year) + 10),
     border = NA)  # Change the border color  

# Highlight the outliers
points(outliers_Z, rep(0, length(outliers_Z)), col = "#fec44f", pch = 19)


# Plot histogram zoomed in on a specific range
hist(ex_mills$year, main = "Zoomed-In Histogram", 
     xlab = "Year", col = "#fec44f", breaks = 30, 
     xlim = c(1800, 2000),  # Focus on years between 1800 and 2000
     border = NA)



# Calculate the frequency of each year
year_freq <- table(ex_mills$year)
year_freq
# Find the year(s) with the maximum frequency
max_freq <- max(year_freq)
max_freq
max_years <- names(year_freq[year_freq == max_freq])
max_years

sum_freq <- sum(year_freq)
print(sum_freq)

# Add text annotations to the histogram for the years with maximum frequency
text(x = as.numeric(max_years), y = max_freq, labels = max_years, 
     col = "blue", pos = 3, cex = 0.8)  # Add text above the bars

# SECTION 4 : Animation of  "Year OF Disappearance"  --------------------------

# Base plot setup (no animation yet)
base_map <- ggplot() +
  geom_sf(data = nl_border, fill = "black", color = NA, alpha = 0.8) +     # Netherlands national border (dark mode)
  geom_sf(data = ex_mills, size = 0.7 ,aes(color = "Disappeared Mills")) +
  geom_sf(data = nl_populated_palces, aes(shape = "Populated Places"), size = 2, color = "white",show.legend = FALSE) +
  geom_text(data = nl_populated_palces, 
            aes(x = st_coordinates(geometry)[, 1], 
                y = st_coordinates(geometry)[, 2], 
                label = name),
            size = 3,  # Adjust size of label
            color = "white",  # Main label color
            fontface = "bold", 
            nudge_y = 0.05,  # Adjust vertical position
            nudge_x = 0, 
            check_overlap = FALSE) +  # Prevent overlap of labels
  labs(
    title = "▪ Mills' Memory",
    subtitle = "▪ Timeline of Mills in the Netherlands: {frame_time}",
    caption = "▪ Data Source: www.molendatabase.org | Map visualization by Massoud Ghaderian | 2024 | R Studio",
    x = NULL,  # Remove x-axis label
    y = NULL   # Remove y-axis label
  )+
  scale_color_manual(
    name = "▪ Legend",  # Legend title
    values = c("Disappeared Mills" = "#fec44f"),
    labels = c("Disappeared Mills")
  ) +
  scale_shape_manual(
    values = c("Populated Places" = 16),  # Circle shape
    labels = c("Populated Places")
  )+
  theme_minimal() +
  theme(
    #Plot Elements
    plot.title = element_text(hjust = -0.01, size = 18, face = "bold", margin = margin(b = 0)),
    plot.subtitle = element_text(hjust = -0.01, size = 14, margin = margin(t = 0)),
    plot.caption = element_text(hjust = -0.01, size = 10, face = "italic", margin = margin(t = 15)),
    plot.margin = margin(t = 30, r = 20, b = 50, l = 20),
    
    #Legend settings
    # legend.position = c(0.95, 0.05),  # x and y position (percent of plot)
    legend.justification = c("right", "bottom"),  # Align legend's bottom-right corner
    # legend.box.margin = margin(5, 5, 5, 5),  # Add some space around the legend
    legend.background = element_rect(fill = "white", color = "white", size = 0.5),  # Optional: Add background and border to legend
    legend.text = element_text(size = 10),  # Increase size to 10 (adjust as needed)
    legend.title = element_text(size = 12),  # Increase legend title size
    legend.spacing.y = unit(1, "cm"),  # Adjust vertical spacing
    
    
    # Customizing  grid lines (for finer latitude and longitude)
    panel.grid.major = element_line(color = "lightgray", size = 0.5),  # Major grid lines: gray color, thickness 0.
    panel.grid.minor = element_line(color = "lightgray", size = 0.5),  # Minor grid lines: light gray, thinner
    
    # Ticks for axis (optional)
    axis.ticks.x = element_line(color = "darkgray", size = 1),  # Ticks for top
    axis.ticks.y = element_line(color = "darkgray", size = 1),  # Ticks for right
    
    # change axis labels style
    axis.text = element_text(
      size = 7,  # Change font size of numbers
      color = "darkgray",  # Change font color
      face = "italic",  # Make numbers bold (optional)
      family = "sans"  # Set font family (optional)
    ),
    # Moving axis labels inside the plot
    axis.text.x = element_text(
      size = 5, hjust = 0.5, vjust = 1 ,margin = margin(t = -10),  # Move x-axis labels to the right (hjust = 1)
    ),
    axis.text.y = element_text(
      size = 5, hjust = 0.5, vjust =0.5,margin = margin(r = -20),  # Move y-axis labels up (vjust = 1.5)
    ),
  ) +
  # Add a north arrow
  annotation_north_arrow(
    location = "bl", # Position: 'tl' = top-left, 'tr' = top-right, etc.
    which_north = "true", # "true" for true north, "grid" for grid north
    style = north_arrow_fancy_orienteering(fill = c("white", "white"), line_col = "black"),# Choose a style for the north arrow
    
    height = unit(1, "cm"),  # Adjust size
    width = unit(1, "cm"),
    pad_x = unit(2.5, "cm"),# Horizontal padding
    pad_y = unit(1, "cm")  # Vertical padding# Adjust size
  ) +
  # Add a scale bar
  annotation_scale(
    location = "bl", # Position: 'bl' = bottom-left
    width_hint = 0.2, # Adjust the width relative to the map
    line_width = 1,
    height = unit(0.1, "cm"), # Adjust the height of the scale bar
    pad_x = unit(1.7, "cm"),
    pad_y = unit(.75, "cm"),
    bar_cols = c("white", "white")
  )

#show static map
base_map


##  animation 1 --------------------------------------------


# Now we set up the animation using gganimate
library(gganimate)


# Add transition for animation using the 'year' field for time-based animation
animated_map <- base_map +
  transition_time(year) +          # Transition over years (animation)
  ease_aes('linear')               # Ensure smooth transition between years

animated_map


# Render the animation as a video (MP4 format)
animated_map_output <- animate(
  animated_map, 
  width = 800, 
  height = 600, 
  fps = 15, 
  duration = 40, 
  renderer = av_renderer()  # Use av_renderer to create a video
)

# Optionally, save the animation as an MP4 file
anim_save("/R-WorkSpaces/R-30dayMapChallange/23-Memory/outputs/mills_timeline.mp4", animated_map_output)


##  animation 2 --------------------------------------------
# Load Required Libraries
library(tidyverse)
library(sf)
library(gganimate)

# Prepare Cumulative Data
str(ex_mills)
head(ex_mills)
ex_mills
# Create a sequence of years
years <- seq(min(ex_mills$year, na.rm = TRUE), max(ex_mills$year, na.rm = TRUE))

years
# Expand data for each mill and year
expanded_data <- expand.grid(mill_id = 1:nrow(ex_mills), year = years) %>%
  left_join(ex_mills %>% mutate(mill_id = row_number()), by = "mill_id") %>%
  mutate(
    year = as.integer(year),  # Explicitly convert 'year' to integer
    disappeared_year = as.integer(verdwenen1),  # Ensure 'verdwenen1' is numeric
    visible = ifelse(year < verdwenen1, TRUE, FALSE)  # Visibility logic
  )

head(expanded_data)

# Base Map Setup
base_map <- ggplot() +
  geom_sf(data = nl_border, fill = "black", color = NA, alpha = 0.8) +  # Netherlands border
  geom_sf(data = expanded_data %>% filter(visible), 
          aes(geometry = geometry, group = mill_id), 
          size = 0.7, col = "#fec44f") +
  labs(
    title = "Timeline of Mills in the Netherlands: {closest_state}",
    subtitle = "Yellow: Mills Visible Before Disappearance",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()

# Add Animation Transition
animated_map <- base_map +
  transition_states(
    states = year,                  # Transition over years
    transition_length = 2,          # Adjust transition speed
    state_length = 1                # Pause per year
  ) +
  ease_aes('linear')                # Smooth transitions

animated_map

# Render Animation
animated_map_output <- animate(
  animated_map, 
  width = 800, 
  height = 600, 
  fps = 10,                         # Frames per second
  duration = length(years) * 2,     # Total duration
  renderer = av_renderer()
)

# Save the Animation
anim_save("/R-WorkSpaces/R-30dayMapChallange/23-Memory/outputs/mills_timeline.mp4", animated_map_output)




# SECTION 5 : Function of Existing Mills  --------------------------

# Load necessary libraries
library(sf)        # For spatial data handling
library(ggplot2)   # For visualization

# Ensure your dataset (`mills`) is loaded as an sf object and projected correctly
mills <- st_transform(mills, crs = 3857)  # Transform to a projected CRS for spatial visualization

head(mills)
# Check the unique mill types
unique(mills$FUNCTIE)

# Create a map to visualize the types of existing mills
mill_function_map <- ggplot(data = mills) +
  geom_sf(aes(color = FUNCTIE), size = 2) +  # Use 'FUNCTIE' column to color the mills
  scale_color_brewer(palette = "Set3", name = "Mill Type") +  # Use a color palette
  labs(title = "function of Existing Mills in the Netherlands",
       x = "Longitude (projected)",
       y = "Latitude (projected)") +
  theme_minimal()

# Display the map
print(mill_function_map)


# Save the final plot as a PDF
ggsave("Mills.png", plot = mill_function_map, 
       width = 8.27, height = 10, dpi = 600, 
       path = "/R-WorkSpaces/R-30dayMapChallange/23-Memory/outputs/")


# SECTION 6  : existing mill on DSM  --------------------------

library(terra)
library(sf)
library(rayshader)

# Load the mills data
mills <- st_read("23-Memory/data/shp/Molens.shp")

# Get the bounding box of the mills (expand it slightly for better context)
mills_bbox <- st_bbox(mills)
expanded_bbox <- mills_bbox + c(-0.01, -0.01, 0.01, 0.01)  # Expand by 0.01 degrees

# WCS API call to fetch DSM for the bounding box
wcs_url <- "https://service.pdok.nl/rws/ahn/wms/v1_0?SERVICE=WMS&request=GetCapabilities&version=1.3.0"
dsm <- rast(paste0(
  wcs_url,
  "?SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage",
  "&COVERAGEID=layer_name",  # Replace with specific layer name
  "&FORMAT=image/tiff",
  "&SUBSET=Lat(", expanded_bbox["ymin"], ",", expanded_bbox["ymax"], ")",
  "&SUBSET=Long(", expanded_bbox["xmin"], ",", expanded_bbox["xmax"], ")"
))

# Convert DSM raster to a matrix for rayshader
dsm_matrix <- as.matrix(dsm, wide = TRUE)

# Render the 3D map
dsm_matrix %>%
  rayshader::height_shade() %>%
  rayshader::plot_3d(heightmap = ., zscale = 10, windowsize = c(1000, 800))

# Overlay mills (optional: convert coordinates to match DSM matrix)
mills_coords <- st_coordinates(mills)
points3d(mills_coords[, 1], mills_coords[, 2], col = "blue", size = 5)

# Adjust the view
rayshader::render_camera(theta = 45, phi = 30, zoom = 0.8)


# SECTION 6  : existing mill on DSM  --------------------------

# Load required libraries
library(leaflet)
library(sf)
library(raster)

# Define the WMS URL for DSM
wms_url <- "https://service.pdok.nl/rws/ahn/wms/v1_0?SERVICE=WMS&request=GetCapabilities&version=1.3.0"

# Load mill locations (replace with your file path)
mills_shapefile <- st_read("23-Memory/data/shp/Molens.shp")

# Check the structure of the shapefile (optional)
head(mills_shapefile)

# Transform the CRS to match WGS 84 (EPSG:4326)
mills_shapefile <- st_transform(mills_shapefile, crs = 4326)

# Verify mill locations by plotting them (optional)
plot(st_geometry(mills_shapefile))

# Create a leaflet map
leaflet() %>%
  # Add WMS basemap for DSM using the defined parameters
  addWMSTiles(
    url = wms_url,
    layers = "ahn3_05m_dsm",   # Ensure this layer name is correct
    options = WMSTileOptions(
      format = "image/png", 
      transparent = TRUE
    ),
    # Set bounding box for Netherlands area
    bbox = c(3.3, 50.8, 7.3, 53.7), # Adjust the bounding box if necessary
    crs = "EPSG:4326",
    version = "1.3.0"
  ) %>%
  # Add existing mills as points (use the correct CRS)
  addCircleMarkers(
    data = mills_shapefile,
    color = "red",
    radius = 5,
    popup = ~NAAM,  # Pop-up with the name of the mill
    fillOpacity = 0.8
  ) %>%
  # Customize map view (adjust zoom level and center to fit your area)
  setView(lng = 5, lat = 52.3, zoom = 10)  # Centered on the Netherlands




# SECTION 6  : existing mill on DSM  --------------------------
# Load required libraries
library(leaflet)
library(sf)
library(raster)

# Define the WMS URL for DSM
wms_url <- "https://service.pdok.nl/rws/ahn/wms/v1_0?SERVICE=WMS&request=GetCapabilities&version=1.3.0"

# Load mill locations (replace with your file path)
mills_shapefile <- st_read("23-Memory/data/shp/Molens.shp")

# Check the structure of the shapefile (optional)
head(mills_shapefile)

# Transform the CRS to match WGS 84 (EPSG:4326)
mills_shapefile <- st_transform(mills_shapefile, crs = 4326)

# Verify mill locations by plotting them (optional)
plot(st_geometry(mills_shapefile))

# Create a leaflet map
leaflet() %>%
  # Add WMS basemap for DSM using the defined parameters
  addWMSTiles(
    wms_url,
    layers = "ahn3_05m_dsm",   # Ensure this layer name is correct
    options = WMSTileOptions(
      format = "image/png", 
      transparent = TRUE
    )
  ) %>%
  # Add existing mills as points (use the correct CRS)
  addCircleMarkers(
    data = mills_shapefile,
    color = "red",
    radius = 5,
    popup = ~NAAM,  # Pop-up with the name of the mill
    fillOpacity = 0.8
  ) %>%
  # Customize map view (adjust zoom level and center to fit your area)
  setView(lng = 5, lat = 52.3, zoom = 10)  # Centered on the Netherlands


# SECTION 6  : existing mill on DSM in amsterdam --------------------------
# Load required libraries
library(leaflet)
library(sf)
library(raster)

# Define the WMS URL for DSM
wms_url <- "https://service.pdok.nl/rws/ahn/wms/v1_0?SERVICE=WMS&request=GetCapabilities&version=1.3.0"

# Load mill locations (replace with your file path)
mills_shapefile <- st_read("23-Memory/data/shp/Molens.shp")

# Check the structure of the shapefile (optional)
head(mills_shapefile)

# Transform the CRS to match WGS 84 (EPSG:4326)
mills_shapefile <- st_transform(mills_shapefile, crs = 4326)

# Verify mill locations by plotting them (optional)
plot(st_geometry(mills_shapefile))

# Define the bounding box for Amsterdam area (this will not be passed directly to the WMS)
bbox_amsterdam <- c(4.75, 52.25, 5.05, 52.45)  # Approximate bbox for Amsterdam

# Create a leaflet map
leaflet() %>%
  # Add WMS basemap for DSM using the defined parameters
  addWMSTiles(
    wms_url,
    layers = "ahn3_05m_dsm",  # Ensure this layer name is correct
    options = WMSTileOptions(
      format = "image/png", 
      transparent = TRUE
    )
  ) %>%
  # Add existing mills as points (use the correct CRS)
  addCircleMarkers(
    data = mills_shapefile,
    color = "red",
    radius = 5,
    popup = ~NAAM,  # Pop-up with the name of the mill
    fillOpacity = 0.8
  ) %>%
  # Customize map view (adjust zoom level and center to Amsterdam)
  setView(lng = 4.9041, lat = 52.3676, zoom = 12)  # Centered on Amsterdam

