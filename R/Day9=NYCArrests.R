library(tidyverse)
library(sf)
library(ggtext)
library(ggpointdensity)
library(patchwork)
library(showtext)
library(rcartocolor)
library(sysfonts)
library(cowplot)
library(MetBrewer)
library(colorspace)

font_add_google("Caveat")
font = "Caveat"
showtext_auto()

df_arrests = readr::read_csv("NYPD_Arrest_Data__Year_to_Date__20240815.csv")

#Select sex crimes and rape arrests
df_arrests = df_arrests %>%
  filter(OFNS_DESC %in% c("FELONY SEX CRIMES", "RAPE", "SEX CRIMES"))

# Borough shape files
boroughs = sf::read_sf(dsn = here::here("Borough Boundaries", "geo_export_1b0f6e14-6c7a-47a0-925f-aa4dd9a9f96d.shp"))

pal = met.brewer("Gauguin")
swatchplot(pal)

  
# Load MetBrewer palette for gradient scale

gradient_pal = MetBrewer::met.brewer("Greek")
gradient_pal = rev(gradient_pal)

# Custom scale with gradientn using gradient_pal
map = ggplot(df_arrests) +
  geom_sf(data = boroughs, color = "black") +
  geom_point(aes(Longitude, Latitude), size = 10.8, color = "#fadadd") +
  ggpointdensity::geom_pointdensity(aes(Longitude, Latitude), adjust = 0.001, size = 10.2) +
  scale_color_gradientn(colors = gradient_pal,
                        breaks = c(5, 75, 190),
                        labels = c("low", " ", "high"),
                        name = "Arrest Rates",guide = guide_colorbar(direction = "horizontal",
                                                                     barheight = unit(3, "mm"),
                                                                     barwidth = unit(100, "mm"),
                                                                     draw.ulim = FALSE,
                                                                     ticks.colour = "transparent",
                                                                     title.position = 'top',
                                                                     title.hjust = 0.5,
                                                                     label.hjust = 0.5)) +
  ggtitle("2024 NYC Clustered Arrests on Rape & Sex Crimes") +
  labs(x = " ", y = "", caption = "Visualization : Rachael Kilonzo | Data Source : NYPD Arrest Data | Map : NYC OpenData") +
  theme(plot.title = element_text(size = 280, face = "bold", hjust = 0.5, family = font, colour = pal[6], vjust = 0.94),
    plot.margin = margin(3, 3, 3, 3, "cm"),
    plot.background = element_rect(fill = "#fbe1e3", color = NA),
    panel.grid = element_blank(),
    plot.caption = element_text(family = font, face = "bold", size = 160, color = pal[6], hjust = 0.5, vjust = -1),
    panel.background = element_blank(),
    axis.text.x = element_text(size = 150, family = font, face = "bold", colour = pal[6]),
    axis.text.y = element_text(size = 150, family = font, face = "bold", colour = pal[6]),
    legend.position = c(0.15, 0.78),
    legend.direction = "horizontal",
    legend.background = element_blank(),
    legend.key = element_rect(fill = "#fbe1e3"),
    legend.title = element_text(face = "bold", family = font, size = 150, colour = pal[6], lineheight = 1.025,
                                margin = margin(b = 4)),
    legend.text = element_text(size = 150, family = font, colour = pal[6]),
    legend.margin = margin(7, 9, 7, 9, "cm"),
    legend.key.width = unit(3, "cm")
  )

# Save the map
ggsave("C:/Users/PC/Desktop/Portfolio/MapChallenge/Day9/NYC ARRESTS/nyc_sexcrimes_arrests.png", height = 35, width = 33)

df_arrests%>% count(PERP_RACE)

df_arrests = df_arrests %>%
  mutate(PERP_RACE = case_when(grepl("Hispanic", PERP_RACE, ignore.case = TRUE) ~ "HISPANIC",TRUE ~ PERP_RACE))

df_arrests%>% count(PERP_RACE)

map_race = df_arrests %>%
  filter(PERP_RACE %in%  c("ASIAN / PACIFIC ISLANDER", "BLACK", "HISPANIC", "WHITE")) %>%
  mutate(PERP_RACE = glue::glue("{PERP_RACE}")) %>%
  ggplot() +
  geom_sf(data = boroughs, color = "black", size = 0.4) +
  geom_point(aes(Longitude, Latitude, fill = PERP_RACE, alpha = PERP_RACE), shape = 21, color = "#fadadd",
             size = 2.2, stroke = 1.2) +
  scale_x_continuous(limits = c(-74.3, -73.7)) +
  scale_y_continuous(limits = c(40.45, 40.9)) +
  scale_fill_manual(values = c(pal[1], pal[2], pal[4], pal[5]), guide = F) +
  scale_alpha_manual(values = c(0.6, 0.6, 0.6, 0.6),guide = F) +
  facet_grid(PERP_RACE~.) +
  labs(x = " ", y = "")+
  theme(strip.background = element_rect(fill = "#fbe1e3", color = "#fbe1e3"),
        strip.text.y = element_text(family = font, color = pal[6], size = 120, face = "bold",margin = margin(l = 15)),
        plot.margin = margin(3, 0, 3, 3),
        panel.background = element_rect(fill = "#fbe1e3", colour = NA),
        panel.spacing = unit(20, "pt"),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "#fbe1e3", color = NA),
        axis.text.x = element_text(size = 100, family = font, face = "bold", colour = pal[6], angle = -90),
        axis.text.y = element_text(size = 100, family = font, face = "bold", colour = pal[6]))


map + map_race + plot_layout(widths = c(1, 0.4))

ggsave("ArrestsNYC.png", width = 35, height = 33)
  
  
  
  
  
  
