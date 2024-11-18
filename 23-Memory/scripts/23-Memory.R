# Load libraries
library(ggplot2)
library(sf)      # For working with spatial data
library(tmap)    # For thematic maps
library(here)    # For managing file paths

# Define paths to shapefiles
existing_molen <- ("F:/R-WorkSpaces/R-30dayMapChallange/23-Memory/data/shp/Molens.shp")
disappeared_molen <- ("F:/R-WorkSpaces/R-30dayMapChallange/23-Memory/data/shp/1/verdwenenmolens/verdwenenmolensPoint.shp")


# Read shapefiles
molen <- st_read(existing_molen)
ex_molen <- st_read(disappeared_molen)


if (st_crs(molen) == st_crs(ex_molen)) {
  print("Yes,same coordinate system")
} else {
  print("No")
}

# Check CRS
st_crs(molen )
st_crs(ex_molen)

# Check geometry
st_geometry_type(molen)
st_geometry_type(ex_molen)

# Inspect data
colnames(molen)
colnames(ex_molen)

head(molen)
head(ex_molen)

# Plot the ex_molens

ggplot(data = ex_molen) +
  geom_sf () +
  theme_minimal() + 
  labs(title = "Basic Map Of Molens")

# Plot the map using ggplot2
ggplot() +
  geom_sf(data = molen, fill = "blue", color = "red") +
  geom_sf(data = ex_molen, fill = "brown", color = "red") +
  theme_minimal() +
  labs(title = "Basic Map of Molens")




# Convert molen to sf object using x and y coordinates
molen_sf <- st_as_sf(molen, coords = c("x", "y"), crs = 4326)  # Replace 4326 with your CRS if different

# Convert ex_molen to sf object using x and y coordinates
ex_molen_sf <- st_as_sf(ex_molen, coords = c("x", "y"), crs = 4326)



st_geometry_type(molen_sf)
st_geometry_type(ex_molen_sf)

# Plot both datasets
ggplot() +
  geom_sf(data = molen_sf, aes(color = "Existing Molens"), size = 2) +
  geom_sf(data = ex_molen_sf, aes(color = "Disappeared Molens"), size = 2) +
  theme_minimal() +
  labs(title = "Basic Map of Molens", color = "Legend") +
  scale_color_manual(values = c("Existing Molens" = "blue", "Disappeared Molens" = "brown"))



