library(magick)
library(cowplot)
library(tidyverse)
library(showtext)
library(sysfonts)
library(MetBrewer)
library(colorspace)

showtext_auto()
font_add_google("Montserrat")
font = "Montserrat"

colors = met.brewer("Archambault")
swatchplot(colors)
textcolor = colors[5]
background_color = colors[4]


# Read images
nairobi = image_read("Nairobi.png")
dar = image_read("Dar Es Salaam.png")
kampala = image_read("Kampala.png")
kigali = image_read("Kigali.png")

# Combine images horizontally
#combined_image = image_append(c(nairobi, dar, kampala, kigali), stack = FALSE)

# Save the result
#image_write(combined_image, path = "combined_image.png")

combined_image =  image_read("combined_image.png")

info =  image_info(combined_image)
info

# Convert the image to ggplot object
image_gg = ggdraw() + 
  draw_image(combined_image, scale = 1)

# Create a rectangular background
rect_background = ggplot() +
  geom_rect(aes(xmin = 0, xmax = 2, ymin = 0, ymax = 1), fill = background_color) +
  theme_void()

# Combine the background and the image with a title
banner = ggdraw() +
  draw_plot(rect_background, 0, 0, 1.2, 1.2) +
  draw_plot(image_gg, 0.05, 0.05, 0.95, 0.95) +  
  draw_label("Central Business Districts of EAC Countries", x = 0.5, y = 0.92, hjust = 0.5, color = textcolor, fontfamily = font, fontface = "bold", size = 150) +
  draw_label("Nairobi", x = 0.135, y = 0.82, hjust = 0.5, color = textcolor, fontfamily = font, fontface = "bold", size = 100) +
  draw_label("Dar es Salaam", x = 0.385, y = 0.82, hjust = 0.5, color = textcolor, fontfamily = font, fontface = "bold", size = 100) +
  draw_label("Kampala", x = 0.635, y = 0.82, hjust = 0.5, color = textcolor, fontfamily = font, fontface = "bold", size = 100) +
  draw_label("Kigali", x = 0.875, y = 0.82, hjust = 0.5, color = textcolor, fontfamily = font, fontface = "bold", size = 100) +
  draw_label("Github : Rachael Kilonzo | Source : OpenStreetMap", x = 0.5, y = 0.1, color = textcolor, fontface = "bold", size = 75, fontfamily = font) 

ggsave("C:/Users/PC/Desktop/Portfolio/MapChallenge/Day8/EAC_Capitals/EAC_CBDS.png", height = 10, width = 30)

