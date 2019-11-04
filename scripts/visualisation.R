# 3 - Visualisation

load("C:/Users/PhD/OneDrive-MMU/Projects/spatial_viz/scripts/data_handling_workspace.RData")

library(cowplot)
library(tidyverse)
library(sf)
library(scales)
library(viridis)

# ggplot function and defining colours
spec <- brewer_pal(palette = "Spectral")(10)
viri <- viridis::inferno(10)
ryb <- brewer_pal(palette = "RdYlBu")(10)

myplot <- function(data){
  ggplot(data) +
    geom_sf(aes(fill = IMD19rank), colour = "white", size = 1)  +
    scale_fill_manual(values = ryb) + 
    theme_void() +
    theme(panel.grid.major = element_line(colour = "transparent"),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          legend.position = "none",
          panel.background = element_rect(fill = "grey12", colour = "grey12"))
}

# Loop through each element (Local Authority) with viz function
orig_plot <- lapply(top10.list.sf, myplot)
dorl_plot <- lapply(top10.dorl.list.sf, myplot)
hex_plot  <- lapply(hex_list, myplot)

# # Facet plot function
# facet_function <- function(xlist){
#           plot_grid(plotlist = xlist,
#                     nrow = 2,
#                     labels = c("Birmingham", "Blackburn", "Blackpool ", "Burnley","Hartlepool",
#                                "Kingston"  ,"Knowsley", "Liverpool", "Manchester","Middlesbrough"),
#                     hjust = 0, vjust = 5, label_colour = "white") +
#     theme(panel.background = element_rect(fill = "grey12", colour = "grey12"))
# }
# 
# # Run function on lists
# orig_cowplot <- facet_function(orig_plot)
# dorl_cowplot <- facet_function(dorl_plot)
# hex_cowplot  <- facet_function(hex_plot)
# 
# # Save each as png
# ggsave(orig_cowplot, filename = "visuals/top10_originals.png",
#        height = 16, width = 22, device = "png")
# 
# ggsave(dorl_cowplot, filename = "visuals/top10_dorling_pop.png",
#        height = 16, width = 22, device = "png")
# 
# ggsave(hex_cowplot, filename = "visuals/top10_geogrid.png",
#        height = 16, width = 22, device = "png")

# -----------------------------------------------------------------------
# Poster style   --------------------------------------------------------
# -----------------------------------------------------------------------

# Extract elements from lists for arranging.
names(orig_plot) <- paste0(labs,"_orig_gg")
list2env(orig_plot, envir = .GlobalEnv)

names(dorl_plot) <- paste0(labs,"_dorl_gg")
list2env(dorl_plot, envir = .GlobalEnv)

names(hex_plot) <- paste0(labs,"_hex_gg")
list2env(hex_plot, envir = .GlobalEnv)

# Generate labels
titles <- c("Middlesbrough","Liverpool","Knowsley","Kingston","Manchester","Blackpool",
            "Birmingham","Burnley","Blackburn","Hartlepool")

mytitleplot <- function(title){
  ggdraw() + draw_label(title, colour = "white", size = 22, angle = 90, fontfamily = "mono",fontface = "bold")
}

labs <- lapply(titles, mytitleplot)
names(labs) <- paste0(titles,"_labs")
list2env(labs, envir = .GlobalEnv)

origtitle <- ggdraw() + draw_label("Original", colour = "white", size = 20, fontfamily = "mono", fontface = "bold") 
dorltitle <- ggdraw() + draw_label("Dorling" , colour = "white", size = 20, fontfamily = "mono", fontface = "bold") 
geogtitle <- ggdraw() + draw_label("Geogrid" , colour = "white", size = 20, fontfamily = "mono", fontface = "bold") 


# Hacky way of creating a custom legend for the main plot
ryb <- brewer_pal(palette = "RdYlBu")(10)
p1 <- ggplot() + geom_tile(aes(x = ryb, fill = ryb, y = 1)) + 
  scale_fill_manual(values = ryb) + 
  theme(panel.grid.major = element_line(colour = "transparent"),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(colour = "white",family = "mono", size = 18,face = "bold"),
        axis.title = element_blank(),
        legend.position = "none",
        panel.background = element_rect(fill = "grey12", colour = "grey12")) +
  scale_x_discrete(labels = c(" 1 \n \n Most deprived","2","3","4","5",
                              "6","7","8","9"," 10 \n \n Least deprived"))

# Generate labels
titles <- c("Middlesbrough","Liverpool","Knowsley","Kingston","Manchester","Blackpool",
            "Birmingham","Burnley","Blackburn","Hartlepool")

mytitleplot <- function(title){
  ggdraw() + draw_label(title, colour = "white", size = 20, angle = 90, fontfamily = "mono", fontface = "bold")
}

labs <- lapply(titles, mytitleplot)
names(labs) <- paste0(titles,"_labs")
list2env(labs, envir = .GlobalEnv)

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
caption2 <- ggdraw() + draw_label("Deprivation data and raw spatial boundaries obtained from the Office for National Statistics (2019) under Open Government Licence.",
                                  colour = "white", size = 14, hjust = 0.5, fontfamily = "mono")
caption3 <- ggdraw() + draw_label("@sh_langton",
                                  colour = "white", size = 26, hjust = 0.5, fontfamily = "mono")





# Arrange maps
threes_cow <-   plot_grid(NULL               , origtitle            , dorltitle            , geogtitle            ,
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
                                    1,0.8,0.8,0.7),
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
                       nrow = 10,
                       rel_heights = c(0.04,0.04,0.02,0.02,1,0.03,0.06,0.01,0.01,0.07),
                       scale = c(1,1,1,1,1,1,0.8)) +
  theme(panel.background = element_rect(fill = "grey12", colour = "grey12"))

# Save as PNG
ggsave(full_plot, filename = "visuals/triplets.png",
       height = 32, width = 16, device = "png", dpi = 300)

# -----------------------------------------------------------------------
# Split triplets   ------------------------------------------------------
# -----------------------------------------------------------------------

# Extract elements from lists for arranging.
names(orig_plot) <- paste0(labs,"_orig_gg")
list2env(orig_plot, envir = .GlobalEnv)

names(dorl_plot) <- paste0(labs,"_dorl_gg")
list2env(dorl_plot, envir = .GlobalEnv)

names(hex_plot) <- paste0(labs,"_hex_gg")
list2env(hex_plot, envir = .GlobalEnv)

# Arrange maps
top_cow <-   plot_grid(NULL               , origtitle            , dorltitle            , geogtitle            ,
                       Middlesbrough_labs , Middlesbrough_orig_gg, Middlesbrough_dorl_gg, Middlesbrough_hex_gg,
                       Liverpool_labs     , Liverpool_orig_gg    , Liverpool_dorl_gg    , Liverpool_hex_gg    ,
                       Knowsley_labs      , Knowsley_orig_gg     , Knowsley_dorl_gg     , Knowsley_hex_gg     ,
                       Kingston_labs      , Kingston_orig_gg     , Kingston_dorl_gg     , Kingston_hex_gg     ,
                       Manchester_labs    , Manchester_orig_gg   , Manchester_dorl_gg   , Manchester_hex_gg   ,
                       ncol = 4,
                       scale = c(1,1,1,1,
                                 1,1,1,1,
                                 1,1,1,1,
                                 1,1,1,1,
                                 1,0.8,0.8,0.8,
                                 1.1,1.1,1.1,1.1),
                       rel_widths  = c(0.1,1,1,1),
                       rel_heights = c(0.3,1,1,1,
                                       1,1,1,1,
                                       1,1,1,1,
                                       1,1,1,1,
                                       1,1,1,1,
                                       1,1,1,1)) +
  theme(panel.background = element_rect(fill = "grey12", colour = "grey12"))

bottom_cow <-   plot_grid(NULL               , origtitle            , dorltitle            , geogtitle           ,
                          Blackpool_labs     , Blackpool_orig_gg    , Blackpool_dorl_gg    , Blackpool_hex_gg    ,
                          Birmingham_labs    , Birmingham_orig_gg   , Birmingham_dorl_gg   , Birmingham_hex_gg   ,
                          Burnley_labs       , Burnley_orig_gg      , Burnley_dorl_gg      , Burnley_hex_gg      ,
                          Blackburn_labs     , Blackburn_orig_gg    , Blackburn_dorl_gg    , Blackburn_hex_gg    ,
                          Hartlepool_labs    , Hartlepool_orig_gg   , Hartlepool_dorl_gg   , Hartlepool_hex_gg   ,   
                          ncol = 4,
                          scale = c(1,1,1,1,
                                    1,1,1,1,
                                    1,1,1,1,
                                    1,0.7,0.7,0.6,
                                    1,1,1,1,
                                    1,0.8,0.8,0.7),
                          rel_widths  = c(0.1,1,1,1),
                          rel_heights = c(0.3,1,1,1,
                                          1,1,1,1,
                                          1,1,1,1,
                                          1,1,1,1,
                                          1,1,1,1,
                                          1,1,1,1)) +
  theme(panel.background = element_rect(fill = "grey12", colour = "grey12"))

# Generate full plot with labels
top_plot <- plot_grid(top_cow, p1, nrow = 2, rel_heights = c(1,0.17), scale = c(1,0.5))  +
  theme(panel.background = element_rect(fill = "grey12", colour = "grey12"))

bot_plot <- plot_grid(bottom_cow, p1, nrow = 2, rel_heights = c(1,0.17), scale = c(1,0.5))  +
  theme(panel.background = element_rect(fill = "grey12", colour = "grey12"))

# # Save as PNG
ggsave(top_plot, filename = "visuals/top_triplets.png",
       height = 16, width = 16, device = "png", dpi = 300)

ggsave(bot_plot, filename = "visuals/bottom_triplets.png",
       height = 16, width = 16, device = "png", dpi = 300)

# -----------------------------------------------------------------------
# One-off triplets   ------------------------------------------------------
# -----------------------------------------------------------------------

# Manchester and Birmingham
manc_bir <-   plot_grid(NULL               , origtitle            , dorltitle            , geogtitle           ,
                        Birmingham_labs    , Birmingham_orig_gg   , Birmingham_dorl_gg   , Birmingham_hex_gg   ,
                        Manchester_labs    , Manchester_orig_gg   , Manchester_dorl_gg   , Manchester_hex_gg   , 
                          ncol = 4,
                          rel_widths  = c(0.1,1,1,1),
                          rel_heights = c(0.3,1,1,1,
                                          1,1,1,1,
                                          1.1,1,1,1)) +
  theme(panel.background = element_rect(fill = "grey12", colour = "grey12"))

manc_bir_leg <- plot_grid(manc_bir, p1, nrow = 2, rel_heights = c(1,0.17), scale = c(1,0.5))  +
  theme(panel.background = element_rect(fill = "grey12", colour = "grey12"))

ggsave(manc_bir_leg, filename = "visuals/manc_birm_triplets.png",
       height = 16, width = 16, device = "png", dpi = 400)

# Burnley and Hartelpool
burn_har <- plot_grid(NULL               , origtitle            , dorltitle            , geogtitle           ,
                      Burnley_labs       , Burnley_orig_gg      , Burnley_dorl_gg      , Burnley_hex_gg      ,
                      Hartlepool_labs    , Hartlepool_orig_gg   , Hartlepool_dorl_gg   , Hartlepool_hex_gg   ,   
                      ncol = 4,
                      scale = c(1,1,1,1,
                                1,1,1,0.9,
                                1,1,0.9,0.9),
                      rel_widths  = c(0.1,1,1,1),
                      rel_heights = c(0.3,1,1,1,
                                      1,1,1,1,
                                      1,1,1,1)) +
  theme(panel.background = element_rect(fill = "grey12", colour = "grey12"))

burn_har_leg <- plot_grid(burn_har, p1, nrow = 2, rel_heights = c(1,0.17), scale = c(1,0.5))  +
  theme(panel.background = element_rect(fill = "grey12", colour = "grey12"))

ggsave(burn_har_leg, filename = "visuals/burn_har_triplets.png",
       height = 16, width = 16, device = "png", dpi = 400)

# Blackpool
bla <- plot_grid(NULL           , origtitle              , dorltitle             , geogtitle           ,
                 Blackpool_labs , Blackpool_orig_gg      , Blackpool_dorl_gg     , Blackpool_hex_gg    ,
                 ncol = 4,
                 rel_widths  = c(0.1,1,1,1,
                                 0.1,1,1,1),
                 rel_heights = c(0.3,1,1,1,
                                 1,1,1,1)) +
  theme(panel.background = element_rect(fill = "grey12", colour = "grey12"))

bla_leg <- plot_grid(bla, p1, nrow = 2, rel_heights = c(1,0.23), scale = c(1,0.5))  +
  theme(panel.background = element_rect(fill = "grey12", colour = "grey12"))

ggsave(bla_leg, filename = "visuals/bla_triplets.png",
       height = 12, width = 16, device = "png", dpi = 400)

# Calcuating areas

prop.table(table(Middlesbrough_sf$IMD19rank))

# total area of Middlesbrough
total.vec <- sum(Middlesbrough_sf$st_areasha)

# areal proportion of Middlesbrough consisting of nhoods in each decile
total.vec <- sum(Middlesbrough_sf$st_areasha)
Middlesbrough_sf %>%
  as_tibble() %>% 
  group_by(IMD19rank) %>% 
  summarise(sum_area = sum(st_areasha)) %>% 
  mutate(prop_area = 100*(sum_area/total.vec))

names(top10.dorl.list.sf) <- paste0(labs,"_dorl")
list2env(top10.dorl.list.sf, envir = .GlobalEnv)

total.dorl.vec <- sum(Middlesbrough_dorl$st_areasha)
Middlesbrough_dorl %>%
  mutate(st_areasha_dorl = st_area(Middlesbrough_dorl)) %>% 
  as_tibble() %>% 
  group_by(IMD19rank) %>% 
  summarise(sum_area = sum(st_areasha_dorl)) %>% 
  mutate(prop_area = 100*(sum_area/total.dorl.vec))


names(hex_list) <- paste0(labs,"_hex")
list2env(hex_list, envir = .GlobalEnv)

total.hex.vec <- sum(Middlesbrough_hex$st_areasha)
Middlesbrough_hex %>%
  mutate(st_areasha_hex = st_area(Middlesbrough_hex)) %>% 
  as_tibble() %>% 
  group_by(IMD19rank) %>% 
  summarise(sum_area = sum(st_areasha_hex)) %>% 
  mutate(prop_area = 100*(sum_area/total.hex.vec))





