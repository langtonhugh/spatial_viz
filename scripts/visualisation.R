# 3 - Visualisation

load("scripts/data_handling_vis_workspace.RData")

library(cowplot)
library(tidyverse)
library(sf)
library(scales)
library(viridis)

# ggplot function and defining colours
viri <- viridis::viridis(10) # colour blind friendly
ryb <- brewer_pal(palette = "RdYlBu")(10) # main

myplot1 <- function(data){
  ggplot(data) +
    geom_sf(aes(fill = IMD19rank), colour = "white", size = 0.15)  +
    scale_fill_manual(values = viri) + 
    theme_void() +
    theme(panel.grid.major = element_line(colour = "transparent"),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          legend.position = "none",
          panel.background = element_rect(fill = "grey12", colour = "grey12"))
}

myplot2 <- function(data){
  ggplot(data) +
    geom_sf(aes(fill = IMD19rank), colour = "transparent")  +
    scale_fill_manual(values = ryb) + 
    theme_void() +
    theme(panel.grid.major = element_line(colour = "transparent"),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          legend.position = "none")
}


# -----------------------------------------------------------------------
# Big Poster style   ----------------------------------------------------
# -----------------------------------------------------------------------

# make the plots 
list.hex.gg <- lapply(list.hex.sf, myplot2)

## name elements according to their LA names

# create vector containing LAnames ordered by IMD rank, without the two outliers (N = 315)
LA.imd <- LA.imd %>% 
  arrange(n)
vec_temp <- as.data.frame(unique(LA2.imd$LA11_name)) # the ordered list)
names(vec_temp) <- "LAname"
vec_temp_sub <- vec_temp %>%
  filter(LAname %in% n.vec) %>%
  pull(LAname)

## order the list.hex.sf list according to this vector
# name the list alphabetically
vec_temp_sub_sort <- sort(vec_temp_sub)
names(list.hex.gg) <- vec_temp_sub_sort

# order
vec_temp_sub <- as.character(vec_temp_sub)
list.hex.gg <- list.hex.gg[vec_temp_sub]

plot_grid(plotlist = list.hex.gg, ncol = 16, scale = 0.9)

# Loop through each element (Local Authority) with viz function
orig_plot <- lapply(top10.list.sf, myplot1)
dorl_plot <- lapply(top10.dorl.list.sf, myplot1)
hex_plot  <- lapply(hex_list, myplot1)

# -----------------------------------------------------------------------
# Small Poster style   --------------------------------------------------
# -----------------------------------------------------------------------

# Extract elements from lists for arranging.

labs <- c("Birmingham", "Blackburn", "Blackpool", "Burnley","Hartlepool", "Kingston"  ,"Knowsley", "Liverpool", "Manchester","Middlesbrough")

names(orig_plot) <- paste0(labs,"_orig_gg")
names(dorl_plot) <- paste0(labs,"_dorl_gg")
names(hex_plot)  <- paste0(labs,"_hex_gg")

list2env(orig_plot, envir = .GlobalEnv)
list2env(dorl_plot, envir = .GlobalEnv)
list2env(hex_plot , envir = .GlobalEnv)

# Generate labels
titles <- c("Middlesbrough","Liverpool","Knowsley","Kingston","Manchester","Blackpool",
            "Birmingham","Burnley","Blackburn","Hartlepool")

mytitleplot <- function(title){
  ggdraw() + draw_label(title, colour = "white", size = 22, angle = 90, fontfamily = "mono",fontface = "bold")
}

labs2 <- lapply(titles, mytitleplot)
names(labs2) <- paste0(titles,"_labs")
list2env(labs2, envir = .GlobalEnv)

origtitle <- ggdraw() + draw_label("Original", colour = "white", size = 20, fontfamily = "mono", fontface = "bold") 
dorltitle <- ggdraw() + draw_label("Dorling" , colour = "white", size = 20, fontfamily = "mono", fontface = "bold") 
geogtitle <- ggdraw() + draw_label("Geogrid" , colour = "white", size = 20, fontfamily = "mono", fontface = "bold") 


# Hacky way of creating a custom legend for the main plot
p1 <- ggplot() + geom_tile(aes(x = viri, fill = viri, y = 1)) + 
  scale_fill_manual(values = viri) + 
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(colour = "white",family = "mono", size = 20, face = "bold"),
        axis.title = element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill = "grey12", colour = "grey12"),
        panel.grid.major = element_line(colour = "transparent"),
        panel.grid.minor = element_line(colour = "transparent"),
        plot.background  = element_rect(fill = "grey12", colour = "grey12")) +
  scale_x_discrete(labels = c(" 1 \n \n Most deprived","2","3","4","5",
                              "6","7","8","9"," 10 \n \n Least deprived")) 

origtitle <- ggdraw() + draw_label("Original", colour = "white", size = 20, fontfamily = "mono", fontface = "bold")
dorltitle <- ggdraw() + draw_label("Dorling" , colour = "white", size = 20, fontfamily = "mono", fontface = "bold")
geogtitle <- ggdraw() + draw_label("Geogrid" , colour = "white", size = 20, fontfamily = "mono", fontface = "bold")

maintitle1 <- ggdraw() + draw_label("Visualising neighbourhood",
                                   colour = "white", size = 62, hjust = 0.5, fontfamily = "mono", fontface = "bold")
maintitle2 <- ggdraw() + draw_label("deprivation in England",
                                   colour = "white", size = 62, hjust = 0.5, fontfamily = "mono", fontface = "bold")

subtitle1 <- ggdraw() + draw_label("Index of Multiple Deprivation deciles (2019) for",
                                   colour = "white", size = 32, hjust = 0.5, fontfamily = "mono")
subtitle2 <- ggdraw() + draw_label("the top ten most deprived Local Authorities",
                                  colour = "white", size = 32, hjust = 0.5, fontfamily = "mono")

caption1 <- ggdraw() + draw_label("Top 10 most deprived Local Authorities determined by the percentage of Lower Super Output Areas classified into the first decile.",
                                   colour = "white", size = 14, hjust = 0.5, fontfamily = "mono")
caption2 <- ggdraw() + draw_label("Deprivation data from the Office for National Statistics (2019). Contains OS Data © Crown copyright 2019.",
                                  colour = "white", size = 14, hjust = 0.5, fontfamily = "mono")
caption3 <- ggdraw() + draw_label("Each Local Authority mapped on different scale.",
                                  colour = "white", size = 14, hjust = 0.5, fontfamily = "mono")
caption4 <- ggdraw() + draw_label("@sh_langton",
                                  colour = "white", size = 26, hjust = 0.5, fontfamily = "mono")



# # Arrange maps
threes_cow <-   plot_grid(NULL               , origtitle            , dorltitle            , geogtitle           ,
                          Middlesbrough_labs , Middlesbrough_orig_gg, Middlesbrough_dorl_gg, Middlesbrough_hex_gg,
                          Liverpool_labs     , Liverpool_orig_gg    , Liverpool_dorl_gg    , Liverpool_hex_gg    ,
                          Knowsley_labs      , Knowsley_orig_gg     , Knowsley_dorl_gg     , Knowsley_hex_gg     ,
                          Kingston_labs      , Kingston_orig_gg     , Kingston_dorl_gg     , Kingston_hex_gg     ,
                          Manchester_labs    , Manchester_orig_gg   , Manchester_dorl_gg   , Manchester_hex_gg   ,
                          Blackpool_labs     , Blackpool_orig_gg    , Blackpool_dorl_gg    , Blackpool_hex_gg    ,
                          Birmingham_labs    , Birmingham_orig_gg   , Birmingham_dorl_gg   , Birmingham_hex_gg   ,
                          Burnley_labs       , Burnley_orig_gg      , Burnley_dorl_gg      , Burnley_hex_gg      ,
                          Blackburn_labs     , Blackburn_orig_gg    , Blackburn_dorl_gg    , Blackburn_hex_gg    ,
                          Hartlepool_labs    , Hartlepool_orig_gg   , Hartlepool_dorl_gg   , Hartlepool_hex_gg   ,
                          ncol = 4,
                          scale = c(1,1,1,1,
                                    1,1,1,1,
                                    1,1,1,1,
                                    1,1,1,1,
                                    1,0.8,0.8,0.8,
                                    1,1,1,1,
                                    1,1,1,1,
                                    1,1,1,1,
                                    1,0.7,0.7,0.7,
                                    1,1,1,1,
                                    1,0.8,0.8,0.8),
                          rel_widths = c(0.1,1,1,1),
                          rel_heights = c(0.4,1,1,1,
                                          1,1,1,1,
                                          1,1,1,1,
                                          1,1,1,1,
                                          1,1,1,1,
                                          1,1,1,1,
                                          1,1,1,1,
                                          1,1,1,1,
                                          1,1,1,1,
                                          1,1,1,1,
                                          1,1,1,1)) +
                          theme(panel.background = element_rect(fill = "grey12", colour = "grey12"))

# Generate full plot with labels
full_plot <- plot_grid(maintitle1,
                       maintitle2,
                       subtitle1,
                       subtitle2,
                       threes_cow,
                       NULL,
                       p1,
                       caption1,
                       caption2,
                       caption3,
                       caption4,
                       nrow = 11,
                       rel_heights = c(0.04,0.04,0.02,0.02,1,0.03,0.05,0.01,0.01,0.01,0.07),
                       scale = c(1,1,1,1,1,1,0.8)) +
  theme(panel.background = element_rect(fill = "grey12", colour = "grey12"))

# Save as PNG
# ggsave(full_plot, filename = "visuals/triplets.jpeg",
#        height = 42, width = 24, device = "jpeg", dpi = 600)

ggsave(full_plot, filename = "visuals/triplets_cb.jpeg",
       height = 42, width = 24, device = "jpeg", dpi = 600)

# Save workspace to avoid re-generating the hex objects
save.image("scripts/data_handling_vis_workspace.RData")

# -----------------------------------------------------------------------
# One-off triplets   ------------------------------------------------------
# -----------------------------------------------------------------------
# 
# # Manchester and Birmingham
# manc_bir <-   plot_grid(NULL               , origtitle            , dorltitle            , geogtitle           ,
#                         Birmingham_labs    , Birmingham_orig_gg   , Birmingham_dorl_gg   , Birmingham_hex_gg   ,
#                         Manchester_labs    , Manchester_orig_gg   , Manchester_dorl_gg   , Manchester_hex_gg   , 
#                           ncol = 4,
#                           rel_widths  = c(0.1,1,1,1),
#                           rel_heights = c(0.3,1,1,1,
#                                           1,1,1,1,
#                                           1.1,1,1,1)) +
#   theme(panel.background = element_rect(fill = "grey12", colour = "grey12"))
# 
# manc_bir_leg <- plot_grid(manc_bir, p1, nrow = 2, rel_heights = c(1,0.17), scale = c(1,0.5))  +
#   theme(panel.background = element_rect(fill = "grey12", colour = "grey12"))
# 
# ggsave(manc_bir_leg, filename = "visuals/manc_birm_triplets.tiff",
#        height = 16, width = 16, device = "tiff", dpi = 500)
# 
# # Stand-alone
# 
# caption4 <- ggdraw() + draw_label("Lower Super Output Area deprivation deciles for England (2019). Dorling scale by resident population. Contains OS Data © Crown copyright 2019.",
#                                   colour = "white", size = 12, hjust = 0.5, fontfamily = "mono")
# caption5 <- ggdraw() + draw_label("@sh_langton",
#                                   colour = "white", size = 16, hjust = 0.5, fontfamily = "mono", fontface = "bold")
# 
# manc_bir_leg <- plot_grid(manc_bir, p1, caption4, caption5, nrow = 4, rel_heights = c(1,0.17,0.02,0.03), scale = c(1,0.5,1,1))  +
#   theme(panel.background = element_rect(fill = "grey12", colour = "grey12"))
# 
# ggsave(manc_bir_leg, filename = "visuals/manc_birm_triplets_png.png",
#        height = 16, width = 16, device = "png", dpi = 300)
# 
# # Burnley and Hartelpool
# burn_har <- plot_grid(NULL               , origtitle            , dorltitle            , geogtitle           ,
#                       Burnley_labs       , Burnley_orig_gg      , Burnley_dorl_gg      , Burnley_hex_gg      ,
#                       Hartlepool_labs    , Hartlepool_orig_gg   , Hartlepool_dorl_gg   , Hartlepool_hex_gg   ,   
#                       ncol = 4,
#                       scale = c(1,1,1,1,
#                                 1,1,1,0.9,
#                                 1,1,0.9,0.9),
#                       rel_widths  = c(0.1,1,1,1),
#                       rel_heights = c(0.3,1,1,1,
#                                       1,1,1,1,
#                                       1,1,1,1)) +
#   theme(panel.background = element_rect(fill = "grey12", colour = "grey12"))
# 
# burn_har_leg <- plot_grid(burn_har, p1, nrow = 2, rel_heights = c(1,0.17), scale = c(1,0.5))  +
#   theme(panel.background = element_rect(fill = "grey12", colour = "grey12"))
# 
# ggsave(burn_har_leg, filename = "visuals/burn_har_triplets.tiff",
#        height = 16, width = 16, device = "tiff", dpi = 500)
# 
# 
# 
# 
# # Blackpool
# bla <- plot_grid(NULL           , origtitle              , dorltitle             , geogtitle           ,
#                  Blackpool_labs , Blackpool_orig_gg      , Blackpool_dorl_gg     , Blackpool_hex_gg    ,
#                  ncol = 4,
#                  rel_widths  = c(0.1,1,1,1,
#                                  0.1,1,1,1),
#                  rel_heights = c(0.3,1,1,1,
#                                  1,1,1,1)) +
#   theme(panel.background = element_rect(fill = "grey12", colour = "grey12"))
# 
# bla_leg <- plot_grid(bla, p1, nrow = 2, rel_heights = c(1,0.23), scale = c(1,0.5))  +
#   theme(panel.background = element_rect(fill = "grey12", colour = "grey12"))
# 
# ggsave(bla_leg, filename = "visuals/bla_triplets.tiff",
#        height = 12, width = 16, device = "tiff", dpi = 500)
# 
# # Calcuating areas
# 
# # actual proportional split
# prop.table(table(Blackpool_sf$IMD19rank))
# 
# # total area of Blackpool
# total.vec <- sum(Blackpool_sf$st_areasha)
# #test <- sum(as.numeric(st_area(Blackpool_sf)))
# 
# # areal proportion of Blackpool consisting of nhoods in each decile
# Blackpool_sf %>%
#   as_tibble() %>% 
#   group_by(IMD19rank) %>% 
#   summarise(sum_area = sum(st_areasha)) %>% 
#   mutate(prop_area = 100*(sum_area/total.vec))
# 
# # note the following is hacky: this outputs Manchester_sf etc objects into the global
# # environment, but they actually the dorling sf objects. This overwrites the raw boundary sf objects.
# list2env(top10.dorl.list.sf, envir = .GlobalEnv)
# 
# # Calculate the areal sum of the whole map
# total.vec.dorl <- as.numeric(sum(st_area(Blackpool_sf)))
# 
# Blackpool_sf %>%
#   mutate(st_areasha_dorl = as.numeric(st_area(Blackpool_sf))) %>% 
#   as_tibble() %>% 
#   group_by(IMD19rank) %>% 
#   summarise(sum_area = sum(st_areasha_dorl)) %>% 
#   mutate(prop_dorl_area = 100*(sum_area/total.vec.dorl)) 
# 
# # note the following is hacky: this outputs Manchester_sf etc objects into the global
# # environment, but they actually the geogrid sf objects. This overwrites the raw boundary sf objects.
# list2env(hex_list, envir = .GlobalEnv)
# 
# total.hex.vec <- sum(Blackpool_hex$st_areasha)
# Blackpool_hex %>%
#   mutate(st_areasha_hex = st_area(Blackpool_hex)) %>% 
#   as_tibble() %>% 
#   group_by(IMD19rank) %>% 
#   summarise(sum_area = sum(st_areasha_hex)) %>% 
#   mutate(prop_area = 100*(sum_area/total.hex.vec))





