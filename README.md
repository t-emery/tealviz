
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tealviz <a href="https://t-emery.github.io/tealviz/"><img src="man/figures/logo.png" align="right" height="139" alt="tealviz website" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/Promptly-Technologies-LLC/tealviz/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Promptly-Technologies-LLC/tealviz/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`tealviz` is a collection of data visualization tools for [Teal
Emery](https://www.tealemery.com/) + [Teal
Insights](https://www.linkedin.com/company/teal-insights/?viewAsMember=true).
The package is publicly available on GitHub with a permissive (MIT)
license to facilitate the reproducibility of Teal Emery + Teal Insights
analysis. I kindly ask that you don’t use it for your own work, however
you are welcome to use the code as a building block to create your own
data viz package.

This package builds upon a package that [Cara
Thompson](https://www.cararthompson.com/) helped me build. I’m a
researcher, not an artist, so having such a skilled designer help me
build this package was a huge help. I highly recommend her work. The
inspired coding & visual design work are hers. The mistakes are mine.

## Prerequisites

To install on Ubuntu, you may first need to install `libmagick++-dev` on
your system by running the following commands in your terminal:

``` bash
sudo apt-get update
sudo apt-get install libmagick++-dev
```

## Installation

You can install the development version of tealviz from
[GitHub](https://github.com/) with:

``` r
devtools::install_github("t-emery/tealviz")
```

### Fonts

`tealviz` assumes that the user has fonts installed on their computer.
The package will attempt to install fonts for you, but may not succeed
depending on your operating system and permissions. If you do not have
the fonts installed, the package will not work. The fonts used in this
package are: `Roboto`, `Roboto Condensed`, `Lora`, and `Lora Bold`. You
can install them manually from [Google
Fonts](https://fonts.google.com/). They are also available as assets in
the `tealviz` package. You can find them in the
[`inst/assets/fonts`](https://github.com/t-emery/tealviz/tree/main/inst/assets/fonts)
folder of the GitHub repo. You can install them with the `extrafont`
package.

Fonts are tricky in R. If you’re having trouble, read this [excellent
explainer by June
Choe](https://yjunechoe.github.io/posts/2021-06-24-setting-up-and-debugging-custom-fonts/).

## Example

The core of the `tealviz` package is a series of `ggplot2` custom
themes, and custom colors/palettes.

``` r
library(tealviz)
# On loading the package, the `font_hoist()` function automatically makes sure the fonts are available for use. You will see messages in the console to inform you whether the fonts loaded correctly or not. 
library(ggplot2)

palmerpenguins::penguins |>
  dplyr::filter(!is.na(bill_length_mm), 
                !is.na(flipper_length_mm), 
                !is.na(body_mass_g)) |>
  ggplot() +
  geom_point(aes(x = bill_length_mm,
                 y = flipper_length_mm,
                 size = body_mass_g,
                 fill = body_mass_g),
             shape = 21,
             color = ti_colors$background) +
  labs(title = "Perfectly proportional penguins",
       subtitle = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.",
       x = "Bill length (mm)",
       y = "Flipper length (mm)",
       fill = "Body mass (g)",
       caption = "Demo plot, built with {palmerpenguins}") +
  guides(size = "none") +
  scale_fill_ti(continuous = TRUE) +
  theme_ti()
```

<img src="man/figures/README-example-1.png" width="100%" />

The Title and Subtitle use
[`ggtext`](https://wilkelab.org/ggtext/articles/introduction.html) under
the hood. This adds powerful functionality, to be explored in its own
vignette. For now, know that it will auto-wrap the subtitle so you can
provide commentary about the visualizations key takeaways without
worrying about line breaks.
