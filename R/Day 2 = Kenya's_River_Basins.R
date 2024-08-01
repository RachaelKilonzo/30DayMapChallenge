setwd("C:/Users/PC/Desktop/Portfolio/MapChallenge/Day2")
getwd()

#-------------------------------------------------------------------------------
# 1. Deal with the libraries & Fonts
#-------------------------------------------------------------------------------

library(tidyverse)
library(sf)
library(giscoR)
library(showtext)
library(cowplot)

font_add_google("Acme")
font1 = "Acme"
font_add_google("Pacifico")
font2 = "Pacifico"
showtext_auto()

#-------------------------------------------------------------------------------
      # 2. Get Kenya's Boarders
#-------------------------------------------------------------------------------

get_country_borders = function(){
  country_borders = giscoR::gisco_get_countries(
    resolution = "3",
    country = "KE"
  )
  return(country_borders)
}

country_borders = get_country_borders()


#-------------------------------------------------------------------------------
# 3. GET THE BASINS
#-------------------------------------------------------------------------------

# https://data.hydrosheds.org/file/HydroBASINS/standard/hybas_af_lev03_v1c.zip


get_basins = function(){
  url = "https://data.hydrosheds.org/file/HydroBASINS/standard/hybas_af_lev03_v1c.zip"
  file_name = "Africa_HydroBasins"
  
  download.file(
    url = url,
    destfile = file_name,
    mode = "wb"
  )
  
  unzip(file_name)
}

get_basins()

list.files()

load_basins = function(){
  filenames =  list.files(
    pattern = ".shp$",
    full.names = T
  )
  africa_basin = sf::st_read(
    filenames
  )
  
  return(africa_basin)
}

africa_basin = load_basins()


sf::sf_use_s2(F)


kenya_basin = africa_basin |>
  sf::st_intersection(
    country_borders
  ) |>
  dplyr::select(
    HYBAS_ID
  )

#-------------------------------------------------------------------------------
                  # 4. Download African Rivers
#-------------------------------------------------------------------------------


# https://data.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10_af_shp.zip

get_rivers = function(){
  url = "https://data.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10_af_shp.zip"
  file_name = "africa-rivers.zip"
  
  download.file(
    url = url,
    destfile = file_name,
    mode = "wb"
  )
  
  unzip(file_name)
}
  
get_rivers() 
  
list.files() 

load_rivers = function(){
  filenames = list.files(
    path = "HydroRIVERS_v10_af_shp",
    pattern = ".shp$",
    full.names = T
  )
  
  africa_rivers = sf::st_read(
    filenames
  )
  
  return(africa_rivers)
}  
  

africa_rivers = load_rivers()

#-------------------------------------------------------------------------------
        # 5. Clip Kenyan Rivers
#------------------------------------------------------------------------------- 

kenya_rivers = africa_rivers |>
  dplyr::select(
    ORD_FLOW
  ) |>
  sf::st_intersection(
    country_borders
  )
  

#-------------------------------------------------------------------------------
              # 6. Determine basins for every river
#-------------------------------------------------------------------------------

kenya_river_basin = sf::st_intersection(
  kenya_rivers,
  kenya_basin
)


#-------------------------------------------------------------------------------
                  # 7. Adjust the river width
#-------------------------------------------------------------------------------

unique(kenya_river_basin$ORD_FLOW)


kenya_river_basin_width = kenya_river_basin |>
  dplyr::mutate(
    width = as.numeric(ORD_FLOW),
    width = dplyr::case_when(
      width == 4 ~ 0.8,
      width == 5 ~ 0.7,
      width == 6 ~ 0.6,
      width == 7 ~ 0.5,
      width == 8 ~ 0.4,
      width == 9 ~ 0.3,
      width == 10 ~ 0.2,
      TRUE ~ 0
    )
  )

sf::st_as_sf()
#-------------------------------------------------------------------------------
           # 8. Plotting
#-------------------------------------------------------------------------------

unique(kenya_river_basin_width$HYBAS_ID)

hcl.pals("qualitative")

plt = ggplot() +
  geom_sf(data = kenya_river_basin_width,
    aes(color = factor(HYBAS_ID), size = width, alpha = width)
  ) +
  scale_color_manual(name = "", values = hcl.colors(4, "Dark 3", alpha = 1)) +
  scale_size(range = c(0.1, 0.7)) +
  scale_alpha(range = c(0.01, 0.7)) +
  theme_void()+
  theme(plot.background = element_rect(fill = "black", color = "black"),
        legend.position = "none",
        plot.margin = margin(3, 1, 0.25, 0, "cm")
    ) 

ggdraw(plt) +
  draw_label(label = "Kenya's River Basins", x = 0.5, y = 0.95, color = "#afeeee", fontfamily = font1, size = 260,  fontface="bold") +
  draw_label(label = "Github : Rachael Kilonzo | Source: HydroSHEDS database https://www.hydrosheds.org/ | Inspired by: Milos Popovic", x = 0.5, y = 0.015,
             color = "#afeeee", fontface = "bold", size = 120, fontfamily = font2) 

  
ggsave("C:/Users/PC/Desktop/Portfolio/MapChallenge/Day2/KenyaRiverBasins.png", width = 20, height = 20, dpi = 600) 





















  
  
  
  
  
  
  
  
  
  

