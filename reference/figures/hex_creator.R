## code to create hex sticker

library(ggplot2)
library(here)
library(magick)
library(hexSticker)
library(showtext)

font_add_google("Caveat")

#thesis_svg <- image_read_svg('https://brand.mcmaster.ca/app/uploads/2019/04/icon-bookmark_svg.svg', width = 900)
azucena <- image_read(paste0(here(), "/man/figures/azucena.png"))

hp <- azucena %>%
  #image_scale("150") %>%
  image_ggplot()


hs <- sticker(hp,
              s_x=1,
              s_y=1.15,
              s_width=1.1,
              s_height=1.1,
              package="MexBrewer",
              h_fill="#eb4901",
              h_color="#48a8a7",
              p_size=7,
              p_x = 1.03,
              p_y = 0.5,
              p_family = "Caveat",
              p_fontface = "bold",
              p_color = "#720f02",
              h_size = 2.3,
              url = "paezha.github.io/MexBrewer",
              u_color = "white",
              u_family = "Roboto Condensed",
              u_x = 0.27,
              u_y = 1.58,
              u_size = 1.5,
              filename = paste0(here(), "/man/figures/MexBrewer.png"))
