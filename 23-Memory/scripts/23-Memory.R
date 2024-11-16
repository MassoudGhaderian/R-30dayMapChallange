# Load libraries
library(ggplot2)
library(sf)      # For working with spatial data
library(tmap)    # For thematic maps
library(here)    # For managing file paths

# Define paths to shapefiles
existing_molen <- here("F:/R-WorkSpaces/R-30dayMapChallange/23-Memory/data/shp/Molens.shp")
disappeared_molen <- here("F:/R-WorkSpaces/R-30dayMapChallange/23-Memory/data/shp/verdwenenmolens.shp")

# Read shapefiles
molen <- st_read(existing_molen)
ex_molen <- st_read(disappeared_molen)


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


# Plot the map using ggplot2
ggplot() +
  geom_sf(data = molen, fill = "blue", color = "red") +
  geom_sf(data = ex_molen, fill = "brown", color = "red") +
  theme_minimal() +
  labs(title = "Basic Map of Molens")

# add other bases  shape file ---------------------------------------------


#add surface water in Netherlands
oppervlaktewater <- st_read("F:/R-WorkSpaces/R-30dayMapChallange/23-Memory/data/shp/oppervlaktewater.shp")
head(oppervlaktewater)


#add populated places
populated_palces <- st_read("F:/R-WorkSpaces/R-30dayMapChallange/23-Memory/data/shp/populated_places.shp")
head(populated_palces)

#add provinces
provinces <- st_read("F:/R-WorkSpaces/R-30dayMapChallange/23-Memory/data/shp/provinces.shp")
head(populated_palces)

# add sea
NorthSea <- st_read("F:/R-WorkSpaces/R-30dayMapChallange/23-Memory/data/shp/NorthSea.shp")


#add border
nl_border <- st_read("F:/R-WorkSpaces/R-30dayMapChallange/23-Memory/data/shp/netherlands_border.shp")

#add Netherlands boundry
NL0 <- st_read("F:/R-WorkSpaces/R-30dayMapChallange/23-Memory/data/shp/gadm41_NLD_shp/gadm41_NLD_0.shp")

#add stats boundry
NL1 <- st_read("F:/R-WorkSpaces/R-30dayMapChallange/23-Memory/data/shp/gadm41_NLD_shp/gadm41_NLD_1.shp")

#add cities boundry
NL2 <- st_read("F:/R-WorkSpaces/R-30dayMapChallange/23-Memory/data/shp/gadm41_NLD_shp/gadm41_NLD_2.shp")

ggplot() +
  geom_sf(data=oppervlaktewater)+
  # geom_sf(data=provinces, fill="NA", color="lightgray")+
  # geom_sf(data=nl_border)
  # geom_sf(data=NorthSea)+
  geom_sf(data=NL0)+
  geom_sf(data=NL1)+
  geom_sf(data=NL2)+
  geom_sf(data = molens, fill="blue")+ 
  geom_sf(data=populated_palces, fill="black")+
  geom_sf_text(data=populated_palces,aes(label = name))+
  theme_minimal()+
  labs(title="Molens in Netherlands")



# symbology and labeling---------------------------------------------------------------


ggplot() +
  labs(title="Molens in Netherlands")+
  geom_sf(data=oppervlaktewater, fill="lightblue", color="NA")+
  # geom_sf(data=provinces, fill="NA", color="lightgray")+
  # geom_sf(data=nl_border)
  # geom_sf(data=NL2, fill="NA", color="black")+
  geom_sf(data=NL0, fill="white", color="gray")+
  geom_sf(data=NL1, fill="NA", color="gray")+
  geom_sf(data=oppervlaktewater, fill="lightblue", color="NA")+
  geom_sf(data = molens, shape=24, fill="blue", color="NA", size=1)+ # Sets fill and border color
  geom_sf(data=populated_palces, fill="black", size =2, lwd=3)+
  geom_sf_text(data=populated_palces,
               aes(label = name),
               size=3,
               color="black",
               fill="white",
               lwd=2,
               fontface="bold",
               nudge_y = 5000)+ # Adjust vertical position of labels
  theme_minimal() 


# theme map ---------------------------------------------------------------




ggplot() +
  # theme_dark()+
  # theme_minimal() 
  # theme_light()
  # theme_classic()
  
  # my theme 1 minimal
  theme(
    plot.background = element_rect(fill = "white", color = NA),  # Background for the entire plot
    panel.background = element_rect(fill = "white", color = NA),     # Background for the plotting area
    plot.title = element_text(vjust = -27, hjust = 0),             # Center and position title
    panel.grid.major = element_line(color = "lightgray", size = 0.5) # Customize grid lines
  )+
  labs(title="Molens in Netherlands",
       x="Longitude",
       y="Latitude")+
  geom_sf(data=oppervlaktewater, fill="lightblue", color="NA")+
  # geom_sf(data=provinces, fill="NA", color="lightgray")+
  # geom_sf(data=nl_border)
  geom_sf(data = molens, shape=24, fill="blue", color="NA", size=1)+ # Sets fill and border color
  geom_sf(data=populated_palces, fill="black", size =2, lwd=3)+
  geom_sf_text(data=populated_palces,
               aes(label = name),
               size=3,
               color="black",
               fill="white",
               lwd=2,
               fontface="bold",
               nudge_y = 5000)+ # Adjust vertical position of labels
  
  geom_sf(data=NL0, fill="NA", color="gray")

















# Check CRS for each dataset
st_crs(oppervlaktewater)
st_crs(molens)
st_crs(populated_palces)
