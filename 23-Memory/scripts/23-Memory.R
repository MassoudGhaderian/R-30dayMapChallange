
#loading libraries
library(ggplot2)
library(here)
library(sf)

# Load CSV
existing_molen_csv <- here("23-Memory/data/ExportedCSV/Molens.csv")
disappeared_molen_csv <- here("23-Memory/data/ExportedCSV/verdwenenmolens.csv")

molen <- read.csv(existing_molen_csv)
ex_molen <- read.csv(disappeared_molen_csv)

#inspect data
head(molen)
head(ex_molen)

# Convert to sf object
molen_sf <- st_as_sf(ex_molen, coords = c("x", "y"), crs = 4326)
ex_molen_sf <- st_as_sf(ex_molen, coords = c("x", "y"), crs = 4326)


#exploring type of disappeared mils
ggplot(data = ex_molen_sf) +
  geom_sf(aes(color = type), size = 1) +
  labs(title = "Existing and Disappeared Mills in the Netherlands",
       subtitle = "With locations of existing and disappeared mills") +
  theme_minimal()


#exploring type of exsiting mils
ggplot(data = molen_sf) +
  geom_sf(aes(color = type), size = 1) +
  labs(title = "Existing and Disappeared Mills in the Netherlands",
       subtitle = "With locations of existing and disappeared mills") +
  theme_minimal()




# Plot
ggplot() +
  geom_sf(data = ex_molen_sf, color="red") +
  geom_sf(data = molen_sf, color="green") +
  theme_minimal() + 
  labs(title = "Basic Map Of Molens")


