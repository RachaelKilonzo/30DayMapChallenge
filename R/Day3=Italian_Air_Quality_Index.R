#-------------------------------------------------------------------------------
          # AIR QUALITY
#-------------------------------------------------------------------------------

setwd("C:/Users/PC/Desktop/Portfolio/MapChallenge/Day3")
getwd()

#-------------------------------------------------------------------------------
          # 1. Libraries
#-------------------------------------------------------------------------------

#install.packages("gstat")
#install.packages("rayshader")

library(tidyverse) 
library(httr)
library(giscoR)
library(sf)
library(gstat)
library(classInt)
library(cowplot)
library(showtext)
library(MoMAColors)
library(rayshader)

font_add_google("Acme")
font1 = "Acme"

showtext_auto()
#-------------------------------------------------------------------------------
            # 2. EXTRACT THE AQI (AIR QUALITY INDEX) DATA
#-------------------------------------------------------------------------------

url = "https://api.waqi.info/v2/search/?token=fa012501dd2961d2f89a34ada922a6419e5ed943&keyword=Italy"

request = httr::GET(url = url)

request

#-------------------------------------------------------------------------------
              # 3. CLEAN THE DATA
#-------------------------------------------------------------------------------

# parse the json data -> run the response -> map into a list if data frame -> 
# flatten it -> create a single data frame

make_aqi_dataframe = function(request) {
  waqi_data = httr::content(request, as = "parsed", type = "application/json")$data                                    
  waqi_flat = waqi_data |>
    purrr::map(as.data.frame)|>
    purrr::map(purrr::flatten)
  
  str(waqi_flat)
  
  waqi_df = data.frame(t(do.call(cbind, waqi_flat))) |>
    dplyr::select(2, 7, 8)
  
  head(waqi_df)
  
  names(waqi_df) = c("aqi", "lat", "long")
  
  waqi_df_clean = waqi_df |>
    dplyr::mutate_at(c("aqi", "lat", "long"),as.numeric)
  
  return(waqi_df_clean)
  
}


waqi_df_clean = make_aqi_dataframe(request = request) |>
  na.omit()



#-------------------------------------------------------------------------------
            # 4. GRID ORGANISATION TO PREDICT AVERAGE AQI FOR THE COUNTRY
#-------------------------------------------------------------------------------


country_sf = giscoR::gisco_get_countries(country = "IT",  resolution = "1")

country_transformed = country_sf |>
  sf::st_transform(3857)

country_grid = sf::st_make_grid(country_transformed,cellsize = units::as_units(
  100, "km^2"), what = "polygons", square = T) |>
  sf::st_intersection(sf::st_buffer(country_transformed, 0)) |>
  sf::st_as_sf() |>
  sf::st_make_valid() |>
  sf::st_transform(4326)

sf::st_geometry(country_grid) = "geometry"

#Save plot to a file

png("C:/Users/PC/Desktop/Portfolio/MapChallenge/Day3/country_grid_plot.png", width = 800, height = 800)
plot(sf::st_geometry(country_grid))
dev.off()


#-------------------------------------------------------------------------------
          # 5. DATA FRAME TO SF POINTS
#-------------------------------------------------------------------------------

waqi_sf = waqi_df_clean |>
  sf::st_as_sf(coords = c("long", "lat")) |>
  sf::st_set_crs(4326)

waqi_sf   # sf object

#-------------------------------------------------------------------------------
        # 6. INTERPOLATION
#-------------------------------------------------------------------------------

waqi_interp = gstat::idw(aqi ~ 1, waqi_sf, country_grid) |> #inverse distance weighting
  dplyr::select(1:3)|>
  dplyr::rename(aqi=var1.pred)

head(waqi_interp)


#-------------------------------------------------------------------------------
            # THEME AND COLORS
#-------------------------------------------------------------------------------

theme_for_the_win = function() {
  theme_minimal() +
    theme(
      axis.line = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      legend.position = c(2, 0.8),  # Bottom right position
      legend.justification = c(2, 0.8),  # Align 
      legend.title = element_text(size = 120, color = "#3e1c00"),
      legend.text = element_text(size = 120, color = "#3e1c00"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = unit(c(t = 0, r = 0, b = 0, l = 0), "lines"),
      plot.background = element_rect(fill = "white", color = NA)
    )
    
}

# Define custom color palette from MoMAColors
althoff_palette = moma.colors("Althoff")[1:5]

cols = althoff_palette
pal = colorRampPalette(cols)(512)

breaks = classInt::classIntervals(waqi_interp$aqi, n = 6, style = "equal")$brks

#-------------------------------------------------------------------------------
              # LAMBERT PROJECTION
#-------------------------------------------------------------------------------

crs_lambert_italy = "+proj=laea +lat_0=41.5 +lon_0=12.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

#-------------------------------------------------------------------------------
              # 2D PLOT OF AIR POLLUTION
#-------------------------------------------------------------------------------

plt = ggplot() +
   geom_sf(data = waqi_interp, aes(fill = aqi, color = aqi),size = 0) +
  scale_fill_gradientn(name = "Air Quality Index", colors = pal, breaks = round(breaks, 0), labels = round(breaks, 0), na.value = "grey80") +
  scale_color_gradientn(name = "Air Quality Index", colors = pal, breaks = round(breaks, 0), labels = round(breaks, 0), na.value = "grey80")+
  guides(color = "none",
         fill = guide_colorbar(
           direction = "horizontal",
           barwidth = 20,
           barheight = 2.5,
           title.position = "top",
           title.hjust = 0.8,
           title.vjust = 0.05,
           label.hjust = 1.3,
           title.theme = element_text(size = 120, color = "#3e1c00"),
           label.theme = element_text(size = 120, color = "#3e1c00")
           )
         ) +
  coord_sf(crs = crs_lambert_italy) +
  theme_for_the_win()

ggdraw(plt) +
  draw_label(label = "Inquinamento Atmosferico in Italia", x = 0.5, y = 0.98, color = "#3e1c00", fontfamily = font1, size = 350,  fontface = "bold") +
  draw_label(label = "Github : Rachael Kilonzo | Source: https://aqicn.org/api | Tutorial: Milos Agathon", x = 0.5, y = 0.015,
             color = "#3e1c00", fontface = "bold", size = 180, fontfamily = font1) +
  theme(plot.background = element_rect(fill = "white", color = "white"))

ggsave("C:/Users/PC/Desktop/Portfolio/MapChallenge/Day3/AirPollutionItaly.png", width = 20, height = 20, dpi = 600) 










