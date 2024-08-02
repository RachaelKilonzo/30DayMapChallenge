setwd("C:/Users/PC/Desktop/Portfolio/MapChallenge/Day1")
getwd()

#-------------------------------------------------------------------------------
# 1. Libraries
#-------------------------------------------------------------------------------

library(httr)
library(tidyverse)
library(sf)
library(showtext)
library(MoMAColors)
library(cowplot)


showtext_auto()
font_add_google("Acme")
font1 = "Acme"
font_add_google("Pacifico")
font2 = "Pacifico"


#-------------------------------------------------------------------------------
# 2. Download the data for Africa
#-------------------------------------------------------------------------------

get_data = function(){
  url = "https://data.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10_af_shp.zip"
  res = httr::GET(
    url,
    write_disk("SubSaharaAfrica_rivers.zip"),
    progress()
  )
  unzip("SubSaharaAfrica_rivers.zip")
  filenames = list.files("HydroRIVERS_v10_af_shp",
                         pattern = "*.shp", full.names = TRUE)
  return(filenames)
}

filenames = get_data()

filenames

#-------------------------------------------------------------------------------
# 3. Load the Rivers
#-------------------------------------------------------------------------------

load_rivers = function(){
  list_riv = lapply(filenames, sf::st_read)
  africa_riv = list_riv[[1]] |>
    sf::st_cast("LINESTRING")
  
  return(africa_riv)
}

africa_riv = load_rivers()

#-------------------------------------------------------------------------------
# 4. Set River width
#-------------------------------------------------------------------------------

unique(africa_riv$ORD_FLOW)

get_river_width = function() {
  africa_riv_width = africa_riv |>
    dplyr::mutate(width = as.numeric(ORD_FLOW),
                  width = dplyr::case_when(width == 2 ~ 1,
                                           width == 3 ~ 0.8,
                                           width == 4 ~ 0.7,
                                           width == 5 ~ 0.6,
                                           width == 6 ~ 0.5,
                                           width == 7 ~ 0.4,
                                           width == 8 ~ 0.3,
                                           width == 9 ~ 0.2,
                                           width == 10 ~ 0.1,
                                           TRUE ~ 0)
    ) |>
    sf::st_as_sf()
  
  return(africa_riv_width)
}

africa_riv_width = get_river_width()

#-------------------------------------------------------------------------------
# 5. Bounding Box
#-------------------------------------------------------------------------------

crsLONGLAT = "+proj=longlat +datum=WGS84 +no_defs"

get_bounding_box = function(bbox, new_prj, bb) {
  bbox = st_sfc(
    st_polygon(list(cbind(c(-18.0, 52.0, 52.0, -18.0, -18.0),
                          c(-35.0, -35.0, 18.0, 18.0, -35.0)))),
    crs = crsLONGLAT)
  
  new_prj = sf::st_transform(bbox, crs = 4087)
  bb = sf::st_bbox(new_prj)
  
  return(bb)
}

bbox = get_bounding_box()

#-------------------------------------------------------------------------------
# 5. Plot the map
#-------------------------------------------------------------------------------

# Define custom color palette from MoMAColors


plt = ggplot() +
  geom_sf(data = africa_riv_width,
          aes(color = factor(ORD_FLOW),
              size = width,
              alpha = factor(ORD_FLOW))) +
  coord_sf(crs = 4087,
           xlim = c(bbox["xmin"], bbox["xmax"]),
           ylim = c(bbox["ymin"], bbox["ymax"])) +
  scale_color_manual(
    name = "",
    values = c("#073763", "#1f4b72", "#385e82",
               "#517391", "#6a87a1", "#839bb1",
               "#9bafc0", "#b4c3d0", "#cdd7df"
    )
  ) +
  scale_size(range = c(0, 0.3)) +
  scale_alpha_manual(values = c("2" = 1, "3" = 0.8, "4" = 0.7,
                                "5" = 0.6, "6" = 0.5, "7" = 0.4,
                                "8" = 0.3, "9" = 0.2, "10" = 0.1)) +
  theme_void() +
  theme(plot.background = element_rect(fill = "white", color = "white"),
        legend.position = "none",
        plot.margin = margin(3, 1, 0.25, 0, "cm")) 


ggdraw(plt) +
  draw_label(label = "Sub-Sahara Africa Rivers", x = 0.5, y = 0.95, color = "#295f48", fontfamily = font1, size = 300,  fontface = "bold") +
  draw_label(label = "Github : Rachael Kilonzo | Source: HydroSHEDS database https://www.hydrosheds.org/ | Tutorial: Milos Agathon", x = 0.5, y = 0.015,
             color = "#295f48", fontface = "bold", size = 130, fontfamily = font2) +
  theme(plot.background = element_rect(fill = "white", color = "white"))


ggsave("C:/Users/PC/Desktop/Portfolio/MapChallenge/Day1/SSAfricaRivers.png", width = 20, height = 20, dpi = 600) 




