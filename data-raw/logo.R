library(tidyverse)
library(hexSticker)
library(magick)
library(sysfonts)
library(fs)

font_add("Century Gothic", "GOTHIC.TTF")

# logo --------------------------------------------------------------------

image_wombat <- image_read("https://upload.wikimedia.org/wikipedia/commons/thumb/3/3d/Wombat_(PSF).png/313px-Wombat_(PSF).png")

sticker(image_wombat,
        package = "wombats",
        filename = path("man/figures/logo.png"),
        s_width = 1.3,
        s_height = 1.3,
        s_x = 1,
        s_y = 1.15,
        p_size = 25,
        h_fill = "snow",
        h_color = "dimgray",
        p_color = "dimgray",
        p_y = 0.6,
        p_family = "Century Gothic")
