

# libraries and packages --------------------------------------------------


install.packages(c("ggplot2", "sf", "tmap"))

library(ggplot2)
library(sf)  # for working with spatial data
library(tmap)   # for thematic maps



# basic map ----------------------------------------------------------

# molens_path <-"D:/R-WorkSpace/30DayMapChallenge2024 by Rbanism/01-Points/shp/Molens.shp"
molens_path <-"D:/R-WorkSpace/R-30dayMapChallange/01-Points/data/shp/Molens.shp"

molens <- st_read(molens_path)

head(molens)
colnames(molens)
st_as_sf(molens)

#add data

ggplot(data = molens) +
  geom_sf()+
  theme_minimal()+
  labs(title="Basic Map Of Molens")


#basic map(color full)

ggplot(data = molens) +
  geom_sf(fill="blue", color="red")+ # Sets fill and border color
  theme_minimal()+
  labs(title="Basic Map Of Molens")



# add other bases  shape file ---------------------------------------------


#add surface water in Netherlands
oppervlaktewater <- st_read("D:/R-WorkSpace/R-30dayMapChallange/01-Points/data/shp/oppervlaktewater.shp")
head(oppervlaktewater)


#add populated places
populated_palces <- st_read("D:/R-WorkSpace/R-30dayMapChallange/01-Points/data/shp/populated_places.shp")
head(populated_palces)

#add provinces
provinces <- st_read("D:/R-WorkSpace/R-30dayMapChallange/01-Points/data/shp/provinces.shp")
head(populated_palces)

# add sea
NorthSea <- st_read("D:/R-WorkSpace/R-30dayMapChallange/01-Points/data/shp/NorthSea.shp")


#add border
nl_border <- st_read("D:/R-WorkSpace/R-30dayMapChallange/01-Points/data/shp/netherlands_border.shp")

#add Netherlands boundry
NL0 <- st_read("D:/R-WorkSpace/R-30dayMapChallange/01-Points/data/shp/gadm41_NLD_shp/gadm41_NLD_0.shp")

#add stats boundry
NL1 <- st_read("D:/R-WorkSpace/R-30dayMapChallange/01-Points/data/shp/gadm41_NLD_shp/gadm41_NLD_1.shp")

#add cities boundry
NL2 <- st_read("D:/R-WorkSpace/R-30dayMapChallange/01-Points/data/shp/gadm41_NLD_shp/gadm41_NLD_2.shp")

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
