# Load libraries
library(ggplot2)
library(here)
library(sf)

# Load CSV
existing_molen_csv <- here("23-Memory/data/ExportedCSV/Molens.csv")
disappeared_molen_csv <- here("23-Memory/data/ExportedCSV/verdwenenmolens.csv")

molen <- read.csv(existing_molen_csv)
ex_molen <- read.csv(disappeared_molen_csv)

# Convert to sf objects
molen_sf <- st_as_sf(molen, coords = c("x", "y"), crs = 4326)
ex_molen_sf <- st_as_sf(ex_molen, coords = c("x", "y"), crs = 4326)

# Load Netherlands boundary (replace with your file path)
netherlands_boundary <- st_read(here("F:/R-WorkSpaces/R-30dayMapChallange/01-Points/data/shp/gadm41_NLD_shp/gadm41_NLD_0.shp"))

# Ensure CRS matches
netherlands_boundary <- st_transform(netherlands_boundary, crs = st_crs(molen_sf))

# Filter windmills to within the Netherlands
molen_sf_filtered <- st_intersection(molen_sf, netherlands_boundary)
ex_molen_sf_filtered <- st_intersection(ex_molen_sf, netherlands_boundary)

# Inspect filtered data
print(paste("Filtered existing mills:", nrow(molen_sf_filtered)))
print(paste("Filtered disappeared mills:", nrow(ex_molen_sf_filtered)))

# Plot filtered data
ggplot() +
  geom_sf(data = netherlands_boundary, fill = "lightblue", color = "blue", alpha = 0.3) +
  geom_sf(data = molen_sf_filtered) +
  geom_sf(data = ex_molen_sf_filtered) +
  theme_minimal() +
  labs(
    title = "Filtered Map of Molens in the Netherlands",
    subtitle = "Only windmills within the Netherlands boundary"
  )

