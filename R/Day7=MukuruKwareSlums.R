library(magick)
library(cowplot)
library(tidyverse)
library(showtext)
library(sysfonts)
library(MetBrewer)
library(colorspace)

showtext_auto()
font_add_google("Indie Flower")
font_add_google("Lobster")
font1 = "Indie Flower"
font2 = "Lobster"

colors = met.brewer("Cassatt2")
swatchplot(colors)
textcolor = colors[9]
background_color = colors[4]

image = image_read("MukuruKware-removebg.png")
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
  draw_label("An Open Map Layout of Mukuru kwa Njenga, Ruben & Kware Slums", x = 0.5, y = 0.92, hjust = 0.5, color = textcolor, fontfamily = font2, fontface = "bold", size = 150) +
  draw_label("Mukuru kwa Ruben", x = 0.2, y = 0.65, hjust = 0.5, color = textcolor, fontfamily = font2, size = 100, angle = 60) +
  draw_label("Mukuru kwa Njenga", x = 0.4, y = 0.3, hjust = 0.5, color = textcolor, fontfamily = font2, size = 100, angle = 315) +
  draw_label("Kware", x = 0.65, y = 0.7, hjust = 0.5, color = textcolor, fontfamily = font2, size = 100, angle = 310) +
  draw_label("Github : Rachael Kilonzo | Source : OpenStreetMap | Inspiration : Marcelo de Oliveira Rosa Prates", x = 0.5, y = 0.1, color = textcolor, fontface = "bold", size = 75, fontfamily = font1) 

ggsave("C:/Users/PC/Desktop/Portfolio/MapChallenge/Day7/MukuruKware/MukuruNeighbourhood.png", height = 20, width = 20)

