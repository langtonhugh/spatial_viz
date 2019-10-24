
library(tidyverse)
library(sf)

# Load 2019 IMD data
imd.df <- read_csv("data/eng_imd19_lsoa.csv")

# Subset LSOA/LA name, LSOA/LA code and countrywide rank and rename
imd.sub.df <- imd.df %>% 
  select(`LSOA code (2011)`,
         `Local Authority District code (2019)`,
         `Local Authority District name (2019)`,
         `Index of Multiple Deprivation (IMD) Score`,
         `Index of Multiple Deprivation (IMD) Decile (where 1 is most deprived 10% of LSOAs)`) %>% 
  rename(LSOA11_code = `LSOA code (2011)`,
         LA11_code = `Local Authority District code (2019)`,
         LA11_name = `Local Authority District name (2019)`,
         IMD19score = `Index of Multiple Deprivation (IMD) Score`,
         IMD19rank = `Index of Multiple Deprivation (IMD) Decile (where 1 is most deprived 10% of LSOAs)`)

# Load shapefiles
lsoa.sf <- st_read("data/Lower_Layer_Super_Output_Areas_December_2011_Super_Generalised_Clipped__Boundaries_in_England_and_Wales.shp")

# Merge subset df with polygons
names(imd.sub.df)
names(lsoa.sf)

lsoa.sf <- lsoa.sf %>% 
  rename(LSOA11_code = lsoa11cd) %>% 
  select(LSOA11_code, st_areasha, st_lengths)


lsoa.imd.sub.sf <- left_join(lsoa.sf, imd.sub.df, by = "LSOA11_code")

# Note:
# Warning over character coercion not a problem.
# There are some losses, with less LSOA polygons than there are
# observations in the IMD data. This needs to be investigated.

# Brief reorder to columns and arrange alphabetically
lsoa.imd.sub.sf <- lsoa.imd.sub.sf %>% 
  select(LA11_code, LA11_name, LSOA11_code, st_areasha,
         st_lengths, IMD19score, IMD19rank) %>% 
  arrange(LA11_name)

