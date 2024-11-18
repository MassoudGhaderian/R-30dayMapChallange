library(sf)      # For spatial operations
library(ggplot2) # For visualization


# Paths to your CSV files
disappeared_molen_csv <- "F:/R-WorkSpaces/R-30dayMapChallange/23-Memory/data/csv/VerdwenenMolens.csv"

# Read the CSV files
ex_molen <- read.csv(disappeared_molen_csv)

# Check column names
colnames(ex_molen)

# Plot the ex_molens
ggplot(data =ex_molen) +
  geom_sf () +
  theme_minimal() + 
  labs(title = "Basic Map Of Molens")

