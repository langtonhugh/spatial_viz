# 2 - data handling

library(cowplot)
library(tidyverse)
library(sf)
library(readxl)
library(cartogram)
library(geogrid)

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

# for each Local Authority name, calculate the percentage of LSOAs which fall into
# the top (=1) IMD decile i.e. most deprived.

LA.imd <- imd.sub.df %>% 
  mutate(IMD19rank = as.factor(IMD19rank)) %>% 
  group_by(LA11_name, IMD19rank) %>% 
  summarise(n = n()) %>% 
  complete(IMD19rank, fill = list(n = 0)) %>% 
  ungroup() %>% 
  group_by(LA11_name) %>% 
  mutate(LSOAn = sum(n),
         prop.imd = round(100*(n/LSOAn), 2)) %>% 
  arrange(LA11_name, IMD19rank) %>% 
  slice(1) %>% 
  arrange(desc(prop.imd)) %>% 
  ungroup()

# Top 10 most deprived by % in most deprived decile (note: still arranged)
top10 <- LA.imd %>% 
  slice(1:10) %>% 
  select(LA11_name) %>% 
  pull()

# Load shapefiles of LSOA
lsoa.sf <- st_read("data/Lower_Layer_Super_Output_Areas_December_2011_Super_Generalised_Clipped__Boundaries_in_England_and_Wales.shp")

# Load population data at LSOA level
pop.df <- read_xlsx("data/SAPE21DT1a-mid-2018-on-2019-LA-lsoa-syoa-estimates-formatted.xlsx",
                    sheet = 4, skip = 4)

# Subset, overwriting to save space
pop.df <- pop.df %>% 
  rename(LSOA11_code = `Area Codes`, pop = `All Ages`) %>% 
  select(LSOA11_code, pop) 

# Join with sf object 
lsoa.sf <- lsoa.sf %>% 
  rename(LSOA11_code = lsoa11cd) %>% 
  select(LSOA11_code, st_areasha, st_lengths)

imd.sub.df <- left_join(imd.sub.df, pop.df, by = "LSOA11_code")

# Merge subset df with polygons
lsoa.imd.sub.sf <- left_join(lsoa.sf, imd.sub.df, by = "LSOA11_code")

# Note:
# Warning over character coercion not a problem.
# There are some losses, with less LSOA polygons than there are
# observations in the IMD data. This needs to be investigated.

# Brief select, reorder and change class of rank (from numeric to factor)
lsoa.imd.sub.sf <- lsoa.imd.sub.sf %>% 
  select(LA11_code, LA11_name, LSOA11_code, st_areasha,
         st_lengths, IMD19score, IMD19rank, pop) %>% 
  arrange(LA11_name) %>% 
  mutate(IMD19rank = as.factor(IMD19rank))

# Extract Top 10
top10.sf <- lsoa.imd.sub.sf %>%
  filter(LA11_name %in% top10) %>% 
  mutate(LA11_name = as.factor(LA11_name))

# -----------------------------------------------------------------------------------
# Create dorling polygons loop ------------------------------------------------------
# -----------------------------------------------------------------------------------

# Split sf object into list by levels of LA11_name for loop
top10.list.sf <- group_split(top10.sf, LA11_name)

# Create Dorling cartogram by population for each element
top10.dorl.list.sf <- lapply(top10.list.sf, function(x){
  cartogram_dorling(x, "pop", k = 1)
})

# Birmingham, Manchester and (maybe) Liverpool do not quite work with k=1
names(top10.list.sf) <- paste0(labs,"_sf")
list2env(top10.list.sf, envir = .GlobalEnv)

Birmingham_dorl  <- cartogram_dorling(Birmingham_sf, "pop", k = 0.3)
Liverpool_dorl   <- cartogram_dorling(Liverpool_sf , "pop", k = 0.6)
Manchester_dorl  <- cartogram_dorling(Manchester_sf, "pop", k = 0.6)

# Birmingham is 1 in the list, Liverpool and Manchester are 8 and 9

top10.dorl.list.sf[[1]] <- Birmingham_dorl
top10.dorl.list.sf[[8]] <- Liverpool_dorl
top10.dorl.list.sf[[9]] <- Manchester_dorl

# -----------------------------------------------------------------------------------
# Geogrids  -------------------------------------------------------------------------
# -----------------------------------------------------------------------------------

# Calcuating empty grid for original LSOA boundaries
top10.grid.empty.list.sf <- lapply(top10.list.sf, function(x){
  calculate_grid(x, grid_type = "hexagonal", seed = 1)
})

# Assign the dorling outputs to the empty grids (no loop yet)
# Bring the empty grids into environment individually
labs <- c("Birmingham", "Blackburn", "Blackpool", "Burnley","Hartlepool", "Kingston"  ,"Knowsley", "Liverpool", "Manchester","Middlesbrough")
names(top10.grid.empty.list.sf) <- paste0(labs,"_grid")
list2env(top10.grid.empty.list.sf, envir = .GlobalEnv)

# Bring the original sf objects into environment individually
names(top10.list.sf) <- paste0(labs,"_sf")
list2env(top10.list.sf, envir = .GlobalEnv)

# Assign individually (THIS CAN TAKE SEVERAL HOURS)
Birmingham_hex    <- assign_polygons(Birmingham_sf   , Birmingham_grid)
Blackburn_hex     <- assign_polygons(Blackburn_sf    , Blackburn_grid)
Blackpool_hex     <- assign_polygons(Blackpool_sf    , Blackpool_grid)
Burnley_hex       <- assign_polygons(Burnley_sf      , Burnley_grid)
Hartlepool_hex    <- assign_polygons(Hartlepool_sf   , Hartlepool_grid)
Kingston_hex      <- assign_polygons(Kingston_sf     , Kingston_grid)
Knowsley_hex      <- assign_polygons(Knowsley_sf     , Knowsley_grid)
Liverpool_hex     <- assign_polygons(Liverpool_sf    , Liverpool_grid)
Manchester_hex    <- assign_polygons(Manchester_sf   , Manchester_grid)
Middlesbrough_hex <- assign_polygons(Middlesbrough_sf, Middlesbrough_grid)

# Create list of these sf objects

hex_list <- list(Birmingham_hex, Blackburn_hex, Blackpool_hex, Burnley_hex, Hartlepool_hex,
                 Kingston_hex, Knowsley_hex, Liverpool_hex, Manchester_hex, Middlesbrough_hex)


# Save workspace to avoid re-generating the hex objects
save.image("scripts/data_handling_workspace_v2.RData")

