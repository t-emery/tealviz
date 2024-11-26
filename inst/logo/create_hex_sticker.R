library(hexSticker)
library(ggplot2)
library(tealviz)
library(showtext)
library(palmerpenguins)



font_add_google("Roboto Condensed")
## Automatically use showtext to render text for future devices
showtext_auto()

p <- palmerpenguins::penguins |>
  ggplot() +
  geom_point(aes(x = bill_length_mm,
                 y = flipper_length_mm,
                 size = body_mass_g,
                 fill = species),
             shape = 21,
             color = ti_colors$background) +
  scale_fill_ti(palette = "cool_colors",
                continuous = FALSE) +
  geom_smooth(aes(x = bill_length_mm,
                  y = flipper_length_mm),
              color = ti_colors$gold,
              fill = ti_colors$gold) +
  theme_void() +
  # get rid of legend
  theme(legend.position = "none") +
  # for hexSticker
  theme_transparent()


sticker(subplot = p,
        # p - name position, font, and color
        package = "tealviz",
        p_color = ti_colors$teal,
        p_family = "Roboto Condensed",
        p_fontface = "bold",
        p_size = 30,
        # s - subplot details
        s_x = 1, # centered
        s_y = .77, # gives a little bit of margin with title
        s_width = 1.2, # make it a little wider
        s_height = 1,
        # h - hex sticker details
        h_fill = ti_colors$background,
        h_color = "#002f49",
        # - url details
        url = "https://t-emery.github.io/tealviz/",
        u_color = ti_colors$dark_text,
        u_size = 4,
        # filepath to save sticker
        filename = "inst/logo/tealviz_hex_logo.png",
        dpi = 500)

use_logo("inst/logo/tealviz_hex_logo.png")
