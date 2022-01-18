# 3 - Visualisation
# Edits made by SL 13 January 2022 for MoD presentation.

load("scripts/data_handling_vis_workspace.RData")

library(cowplot)
library(tidyverse)
library(sf)
library(scales)
library(viridis)

# ggplot function and defining colours
# viri <- viridis::viridis(10) # colour blind friendly
viri <- viridis::viridis(5) # colour blind friendly
magm <- viridis::magma(10) # colour blind friendly
plas <- viridis::plasma(10) # colour blind friendly
ryb <- brewer_pal(palette = "RdYlBu")(10) # main

# Recode test.
recode_test <- list.hex.sf[[300]] %>%
  mutate(IMD19rank_new5 = recode(IMD19rank,
                                 "1"  = 1,
                                 "2"  = 1,
                                 "3"  = 2,
                                 "4"  = 2,
                                 "5"  = 3,
                                 "6"  = 3,
                                 "7"  = 4,
                                 "8"  = 4,
                                 "9"  = 5,
                                 "10" = 5))

# Check.
ggplot(data = recode_test) +
  geom_point(mapping = aes(x = IMD19rank, y = IMD19rank_new5))

# Recode and plot.
myplot1 <- function(data){
  data %>%
    mutate(IMD19rank_new5 = recode(IMD19rank,
                                   "1"  = 1,
                                   "2"  = 1,
                                   "3"  = 2,
                                   "4"  = 2,
                                   "5"  = 3,
                                   "6"  = 3,
                                   "7"  = 4,
                                   "8"  = 4,
                                   "9"  = 5,
                                   "10" = 5),
           IMD19rank_new5 = as.character(IMD19rank_new5)) %>% 
  ggplot(data = .) +
    theme_void() +
    geom_sf(aes(fill = IMD19rank_new5), colour = "white", size = 0.0015)  +
    scale_fill_manual(values = viri, labels = 1:5, drop = F) + 
    guides(fill = guide_legend(nrow = 1)) +
    labs(fill = NULL) +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "bottom",
          legend.text = element_text(colour = "white"),
          legend.box.background = element_rect(colour = "transparent"),
          legend.background = element_rect(fill = "transparent", 
                                           colour = "transparent"),
          panel.grid.minor = element_line(colour = "transparent"),
          panel.grid.major = element_line(colour = "transparent"),
          panel.background = element_rect(fill = "grey12", colour = "grey12"),
          plot.background = element_rect(fill = "grey12"),
          panel.border = element_blank(),
          plot.margin = unit(c(t = 0, r = 0, b = 1, l = 0), "cm"))
}

# -----------------------------------------------------------------------
# Big Poster style   ----------------------------------------------------
# -----------------------------------------------------------------------

# make the plots 
par(bg="gray12")
list.hex.gg <- lapply(list.hex.sf, myplot1)

## name elements according to their LA names

# create vector containing LAnames ordered by IMD rank, without the two outliers (N = 315)
LA.imd <- LA.imd %>% 
  # arrange(n)
  arrange(prop.imd) # edit 14 jan 2022

vec_temp <- as.data.frame(unique(LA.imd$LA11_name)) # the ordered list)
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

length(list.hex.gg)

top10.hex.list.sf <- list.hex.gg[306:315]

length(top10.hex.list.sf)

names(top10.hex.list.sf) <- names(list.hex.gg[306:315])

# plot blank visual for demo only
#plot_grid(plotlist = list.hex1.gg, ncol = 16, scale = 0.9)
# Update comment: why two identical? Seems pointless.

# p1 <- plot_grid(plotlist = list.hex.gg, nrow = 16, scale = 0.9) +
#   theme(panel.background = element_rect(fill = "grey12", colour = "grey12"))
# 
# p2 <- plot_grid(plotlist = list.hex.gg, ncol = 16, scale = 0.9) +
#   theme(panel.background = element_rect(fill = "grey12", colour = "grey12"))

# ggsave(p1, filename = "visuals/poster_wip_viri_low_qual.jpeg",
#        width = 42, height = 24, device = "png", dpi = 100)

# ggsave(p2, filename = "visuals/poster_wip_viri.jpeg",
#        height = 42, width = 24, device = "jpeg", dpi = 600)

# -----------------------------------------------------------------------
# Small Poster style   --------------------------------------------------
# -----------------------------------------------------------------------

# Loop through each element (Local Authority) with viz function
orig_plot <- lapply(top10.list.sf, myplot1)
dorl_plot <- lapply(top10.dorl.list.sf, myplot1)
hex_plot  <- top10.hex.list.sf # Exists so just rename object.

# Pull out the real resident population living in the most deprived fifth in the MoD case studies.
residents_list <- top10.list.sf[c("Birmingham_sf", "Burnley_sf", "Hartlepool_sf")]

# Function.
pop_fun <- function(x) {
  x %>%
    mutate(IMD19rank_new5 = recode(IMD19rank, "1"  = 1, "2"  = 1, "3"  = 2, "4"  = 2, "5"  = 3,
                                              "6"  = 3, "7"  = 4, "8"  = 4, "9"  = 5, "10" = 5), # repeat necessary
         IMD19rank_new5 = as.character(IMD19rank_new5)) %>% 
    group_by(LA11_name, IMD19rank_new5) %>% 
    summarise(pop_imd = sum(pop)) %>% 
    ungroup() %>% 
    mutate(total_pop = sum(pop_imd),
           prop_pop   = 100*pop_imd/total_pop) %>% 
    as_tibble() %>% 
    select(-geometry)
}

# Run through the three study sites.
residents_imd_list <- lapply(residents_list, pop_fun)

# Bind rows.
residents_imd_df <- bind_rows(residents_imd_list)

# Save for usage!
write_csv(x = residents_imd_df, file = "results/residents_imd_df.csv")

# hex_plot  <- lapply(top10.hex.list.sf, myplot1) # edit 14 jan 2022
# hex_plot  <- lapply(hex_list, myplot1)

# Check.
length(orig_plot)
length(dorl_plot)
length(hex_plot)

names(orig_plot)
names(dorl_plot)
names(hex_plot)

# Original is incorrect ordering but it is labelled correctly.
names(orig_plot) <- str_remove_all(names(orig_plot), "_sf")
names(dorl_plot) <- names(orig_plot)

# Now save.
lapply(names(orig_plot), function(i)
  ggsave(plot = orig_plot[[i]], filename = paste0("visuals/mod/", i, "_original", ".png"),
         height = 7, width = 7, unit = "in")
)

lapply(names(dorl_plot), function(i)
  ggsave(plot = dorl_plot[[i]], filename = paste0("visuals/mod/", i, "_dorling", ".png"),
         height = 7, width = 7, unit = "in")
)

lapply(names(hex_plot), function(i)
  ggsave(plot = hex_plot[[i]], filename = paste0("visuals/mod/", i, "_hex", ".png"),
         height = 7, width = 7, unit = "in")
)

# Save them to check everything matches up.
original_gg <- plot_grid(plotlist = orig_plot, nrow = 10)
dorling_gg  <- plot_grid(plotlist = dorl_plot, nrow = 10)
hex_gg      <- plot_grid(plotlist = hex_plot , nrow = 10)
fu
labs <- c("Birmingham", "Blackburn", "Blackpool", "Burnley","Hartlepool", "Kingston"  ,"Knowsley", "Liverpool", "Manchester","Middlesbrough")

names(orig_plot) <- paste0(labs,"_orig_gg")
names(dorl_plot) <- paste0(labs,"_dorl_gg")
names(hex_plot)  <- paste0(labs,"_hex_gg")

ll_cols_gg <- plot_grid(original_gg, dorling_gg, hex_gg, ncol = 3)

ggsave(full_cols_gg, height = 30, width = 12, filename = "visuals/col_test.png", unit = "cm")

# Extract elements from lists for arranging.


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
# save.image("scripts/data_handling_vis_workspace.RData")

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





