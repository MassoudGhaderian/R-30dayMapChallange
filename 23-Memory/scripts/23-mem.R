library(sf)      # For spatial operations
library(ggplot2) # For visualization


# Paths to your CSV files
existing_molen_csv <- "F:/R-WorkSpaces/R-30dayMapChallange/23-Memory/data/csv/Molens.csv"
disappeared_molen_csv <- "F:/R-WorkSpaces/R-30dayMapChallange/23-Memory/data/csv/VerdwenenMolens.csv"

# Read the CSV files
molen <- read.csv(existing_molen_csv)
ex_molen <- read.csv(disappeared_molen_csv)

# Check column names
colnames(molen)
colnames(ex_molen)

# Preview the data in the wkb_geometry column
head(molen$the_geom)
head(ex_molen$wkb_geometry)


# Convert existing molen to sf object
molen_sf <- st_as_sf(molen, wkt = "the_geom", crs = 4326)  # Replace 4326 with your CRS if different

# Convert disappeared molen to sf object
ex_molen_sf <- st_as_sf(ex_molen, wkt = "wkb_geometry", crs = 4326)



st_geometry_type(molen_sf)
st_geometry_type(ex_molen_sf)

# Preview the data
head(molen_sf)
head(ex_molen_sf)

st_geometry_type(molen_sf)
st_geometry_type(ex_molen_sf)

# Preview the data
head(molen_sf)
head(ex_molen_sf)



