library(magick)
library(cowplot)
library(tidyverse)
library(showtext)
library(sysfonts)
library(MetBrewer)
library(colorspace)

showtext_auto()
font_add_google("Berkshire Swash")
font = "Berkshire Swash"

colors = met.brewer("Hokusai1")
swatchplot(colors)
textcolor = colors[7]
background_color = colors[5]

image = image_read("cropped_KiberaSlum.png")
info =  image_info(image)
info


# Convert the image to ggplot object
image_gg = ggdraw() + 
  draw_image(image, scale = 1)

# Create a rectangular background
rect_background = ggplot() +
  geom_rect(aes(xmin = 0, xmax = 1, ymin = 0, ymax = 1), fill = background_color) +
  theme_void()

# Combine the background and the image with a title
banner = ggdraw() +
  draw_plot(rect_background, 0, 0, 1, 1) +
  draw_plot(image_gg, 0.1, 0.1, 0.8, 0.8) +  
  draw_label("Exploring the Heart of Kibera: A 2.5 km Radius Perspective", x = 0.5, y = 0.93, hjust = 0.5, color = textcolor, fontfamily = font, size = 110)

ggsave("C:/Users/PC/Desktop/Portfolio/MapChallenge/Day6/Kibera/RadiusKibera.png", height = 15, width = 15)

