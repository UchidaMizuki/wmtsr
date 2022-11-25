library(tidyverse)
library(hexSticker)
library(magick)
library(sysfonts)
library(fs)

# logo --------------------------------------------------------------------

path_image <- "https://upload.wikimedia.org/wikipedia/commons/thumb/3/3d/Wombat_(PSF).png/313px-Wombat_(PSF).png"
image <- image_read(path_image)

# font <- font_files()
font_add("Segoe UI", "segoeui.ttf")

sticker(image,
        package = "wombats",
        filename = path("man/figures/logo.png"),
        s_width = 1.25,
        s_height = 1.25,
        s_x = 1,
        s_y = 0.75,
        p_size = 25,
        h_fill = "snow",
        h_color = "dimgray",
        p_color = "dimgray",
        p_family = "Segoe UI")
