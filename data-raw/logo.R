library(tidyverse)
library(hexSticker)
library(magick)
library(fs)

font_logo <- "Poppins"
sysfonts::font_add_google(font_logo)

# logo --------------------------------------------------------------------

file_logo <- "man/figures/logo.png"
image_wombat <- image_read("https://upload.wikimedia.org/wikipedia/commons/thumb/3/3d/Wombat_(PSF).png/313px-Wombat_(PSF).png")

sticker(image_wombat,
        package = "wombats",
        filename = file_logo,

        s_width = 1.3,
        s_height = 1.3,
        s_x = 1,
        s_y = 1.15,

        p_size = 23,
        p_color = "dimgray",
        p_y = 0.6,
        p_family = font_logo,
        p_fontface = "bold.italic",

        h_fill = "snow",
        h_color = "dimgray")

usethis::use_logo(file_logo)
