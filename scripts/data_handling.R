# 2 - data handling

# Edits made by SL 13 January 2022 for MoD presentation.

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

# Top 10 most deprived but as df for new stuff.
top10_df <- LA.imd %>% 
  slice(1:10) %>% 
  select(LA11_name) %>% 
  mutate(rank = 1:10,
         half = "top10")

# 'Bottom' 10 most deprived. NOTE: this is "some" of the top 10 least, because they literally have zero.
# So, we just select them and then take a random sample of ten.

set.seed(1612) # for reproduction.

bot10_df <- LA.imd %>% 
  filter(prop.imd == 0) %>% 
  sample_n(10) %>% 
  select(LA11_name) %>% 
  mutate(rank = 1:10,
         half = "bottom10") 

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

names(lsoa.imd.sub.sf)

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

# Extract top and bottom 10.
topbot10.sf <- lsoa.imd.sub.sf %>%
  filter(LA11_name %in% c(top10_df$LA11_name, bot10_df$LA11_name) ) %>% 
  mutate(LA11_name = as.factor(LA11_name))

# Join back the rank stuff with it.
topfull_df <- bind_rows(top10_df, bot10_df) # rbind the tens.

topbot10_new_sf <- left_join(topbot10.sf, topfull_df) 

# Check.
names(topbot10_new_sf)

# Check that we have 20.
unique(topbot10_new_sf$LA11_name)
length(unique(topbot10_new_sf$LA11_name))

# Plot.
topbot10_new_list_sf <- group_split(topbot10_new_sf, LA11_name)

topbot10_new_list_gg <- lapply(topbot10_new_list_sf, function(x){
  ggplot(data = x) +
    geom_sf(mapping = aes(fill = IMD19rank)) +
    scale_fill_viridis_d() +
    theme_void() +
    labs(title = paste(unique(x$LA11_name)))
})

map_check_gg <- cowplot::plot_grid(plotlist = topbot10_new_list_gg, nrow = 5)

# Save.
ggsave(map_check_gg, filename = "visuals/topbot10.png", height = 32, width = 32, unit = "cm")

# Just take the top 10 again.
top10_list_sf <- topbot10_new_sf %>% 
  filter(half == "top10") %>% 
  group_split(LA11_name)

# Plot function from visualisation.r,

viri <- viridis::viridis(10) # colour blind friendly
myplot1 <- function(data){
  ggplot(data) + theme_void() +
    geom_sf(aes(fill = IMD19rank), colour = "white", size = 0.0015)  +
    scale_fill_manual(values = viri) +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "none", # turn on to check the legend is correct (some LA don't have any wealthy deciles)
          panel.grid.minor = element_line(colour = "transparent"),
          panel.grid.major = element_line(colour = "transparent"),
          panel.background = element_rect(fill = "grey12", colour = "grey12"),
          plot.background = element_rect(fill = "grey12"),
          panel.border = element_blank())
}

# Plot top 10.
top10_list_gg <- lapply(top10_list_sf, myplot1)

top10_gg <- cowplot::plot_grid(plotlist = top10_list_gg, nrow = 5)

# Save.
ggsave(top10_gg, filename = "visuals/top10.png", height = 32, width = 32, unit = "cm")

# Give names for save.
temp <- topbot10_new_sf %>% 
  filter(half == "top10") 

names(top10_list_gg) <-  unique(temp$LA11_name)

# Save invididually for MoD survey.
for (i in top10_list_gg) {
  ggsave(filename = paste("visuals/", "hello", ".png", sep = ""))
}

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
labs <- c("Birmingham", "Blackburn", "Blackpool", "Burnley","Hartlepool", "Kingston"  ,"Knowsley", "Liverpool", "Manchester","Middlesbrough")
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

# *** For all LA. Skip to next section for just the top 10 ***

# Identify those extremes (< 20, > 1000)
n.vec <- lsoa.imd.sub.sf %>% 
  as_tibble() %>% 
  group_by(LA11_name) %>% 
  tally() %>% 
  filter(n > 20 & n < 1000) %>% 
  pull(LA11_name)

no.ex.lsoa.imd.sub.sf <- lsoa.imd.sub.sf %>% 
  filter(LA11_name %in% n.vec)

# Create list of all LA sf objects.
list.sf <- group_split(no.ex.lsoa.imd.sub.sf, LA11_name)

# Create geogrids for all of these non-extreme LAs
grid.empty.list.sf <- lapply(list.sf, function(x){
  calculate_grid(x, grid_type = "hexagonal", seed = 1)
})

# Assign to the empty grids (TAKES TIME!!!)
list.hex.sf <- map2(list.sf, grid.empty.list.sf, assign_polygons)

# Save workspace
save.image("scripts/data_handling_vis_workspace.RData")

# *** For top 10 only *** 
# Calcuating empty grid for original LSOA boundaries
top10.grid.empty.list.sf <- lapply(top10.list.sf, function(x){
  calculate_grid(x, grid_type = "hexagonal", seed = 1)
})

# Assign to the empty grids (no loop yet)
# Bring the empty grids into environment individually
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

