
#' Set of colors used in the Teal Insights color palettes and theme
#'
#' Anchor colors used to create the TI color palettes.
#' These can also be used as stand-alone colors (e.g. ti_colors$gold returns the
#' hex code \"#f8c03e\")
#'
#' @export
ti_colors <- list(
  dark_text = "#00101A",
  light_text = "#323F47",
  background = "#FEFCF7",
  dark_teal =  "#002f49",
  mid_teal = "#004C6F",
  teal =  "#0079a8",
  mint = "#86AFA1",
  muted_gold = "#d9b74b",
  orange = "#ad712f",
  dark_orange = "#804126",
  crimson = "#53121d",
  positive = "#397c54",
  pos_neg_neutral = "#99abb6",
  negative = "#742428",
  republican = "#A90E0E",
  rep_dem_neutral = "#FEFCF7",
  democrat = "#042A9C",
  gold = "#f8c03e",
  na_value = "#848B90",
  grid_lines = "#F3F3F4"
)

#' Set of color palettes used in the `scale_color_ti()` and `scale_fill_ti()`
#' functions
#'
#' Palettes created using the TI anchor colors. To view the list of palettes and
#' their associated hex codes, use `ti_palettes`).
#' Note that the palettes used in plots interpolate between the anchor colors,
#' allowing each palette to be used with any number of colors (within reason!)
#'
#' @export
ti_palettes <- list(
  default = c(ti_colors$dark_teal,
              ti_colors$teal,
              ti_colors$muted_gold,
              ti_colors$dark_orange),
  teal_to_gold = c(ti_colors$dark_teal,
                   ti_colors$teal,
                   ti_colors$gold),
  teal_to_crimson = c(ti_colors$mid_teal,
                      ti_colors$muted_gold,
                      ti_colors$orange,
                      ti_colors$crimson),
  cool_colors = c(ti_colors$dark_teal,
                  ti_colors$teal,
                  ti_colors$mint),
  warm_colors = c(ti_colors$muted_gold,
                  ti_colors$orange,
                  ti_colors$crimson),
  skip_gold = c(ti_colors$dark_teal,
                ti_colors$teal,
                ti_colors$mint,
                ti_colors$orange,
                ti_colors$dark_orange),
  neg_to_pos = c(ti_colors$negative,
                 ti_colors$pos_neg_neutral,
                 ti_colors$positive),
  us_rep_to_dem = c(ti_colors$republican,
                    ti_colors$rep_dem_neutral,
                    ti_colors$democrat),
  divergent = c(ti_colors$dark_teal,
                ti_colors$teal,
                ti_colors$mint,
                ti_colors$muted_gold,
                ti_colors$dark_orange,
                ti_colors$crimson)
)

#' Applying the color palettes to the `color` aesthetic within the plots
#'
#' A function to apply the TI color palettes to ggplots, using interpolation to
#' ensure the right number of colors
#'
#' @param palette The TI palette you want to use. Choose from "default",
#' "teal_to_gold", "teal_to_crimson", "cool_colors", "warm_colors", "skip_gold",
#' "neg_to_pos", "us_rep_to_dem" or "divergent"
#' @param reverse Logical. Default is `FALSE`. `TRUE` reverses the palette (e.g.
#' makes "neg_to_pos" start with the positive end rather than the negative end
#' of the colour palette).
#' @param continuous Logical. Default is `FALSE`. Change to `TRUE` when applying
#' the color scale to a continuous variable
#' @param .aesthetic Default is "color". Can be changed to "fill", but it's best
#' to use `scale_fill_ti()` to apply the colour palettes to the fill aesthetic.
#' @param .colors Used to access `ti_colors`. Please leave as is.
#' @param .palettes Used to access `ti_palettes`. Please leave as is.
#' @param ... Additional arguments to pass to `ggplot2::continuous_scale` /
#' `ggplot2::discrete_scale` (e.g. `guide`, `limits`, etc.)
#'
#' @export
#'
scale_color_ti <- function(
  palette = "default",
  reverse = FALSE,
  continuous = FALSE,
  .aesthetic = "color",
  .colors = ti_colors,
  .palettes = ti_palettes,
  ...
) {


  pal_to_use <- .palettes[[palette]]

  if (reverse) {

    pal_to_use <- rev(pal_to_use)
  }

  if (continuous == FALSE) {

    ggplot2::discrete_scale(
      aesthetics = .aesthetic,
      palette = grDevices::colorRampPalette(pal_to_use),
      na.value = .colors$na_value,
      ...
    )

  } else {

    if (.aesthetic == "fill") {

      ggplot2::scale_fill_gradientn(
        colors = pal_to_use,
        na.value = .colors$na_value,
        ...
      )

    } else {

      ggplot2::scale_color_gradientn(
        colors = pal_to_use,
        na.value = .colors$na_value,
        ...
      )
    }
  }

}

#' Applying the color palettes to the `fill` aesthetic within the plots
#'
#' A function to apply the TI color palettes to ggplots, using interpolation to
#' ensure the right number of colors.
#' It builds upon `scale_color_ti()`, to ensure the two functions remain aligned
#' in future developments.
#'
#' @param palette The TI palette you want to use. Choose from "default",
#' "teal_to_gold", "teal_to_crimson", "cool_colors", "warm_colors", "skip_gold",
#' "neg_to_pos", "us_rep_to_dem" or "divergent"
#' @param reverse Logical. Default is `FALSE`. Change to `TRUE` to reverse the
#' palette (e.g. to make "neg_to_pos" start with the positive end rather than
#' the negative end of the colour palette).
#' @param continuous Logical. Default is `FALSE`. Change to `TRUE` when applying
#' the color scale to a continuous variable
#' @param .aesthetic Default is "fill". Can be changed to "color", but it's best
#' to use `scale_color_ti()` to apply the color palettes to the color aesthetic.
#' @param .colors ti_colors
#' @param .palettes ti_palettes
#' @param ... Additional arguments to pass to `ggplot2::continuous_scale` /
#' `ggplot2::discrete_scale` (e.g. `guide`, `limits`, etc.)
#'
#' @export
#'
scale_fill_ti <- function(
  palette = "default",
  reverse = FALSE,
  continuous = FALSE,
  .aesthetic = "fill",
  .colors = ti_colors,
  .palettes = ti_palettes,
  ...
) {

  scale_color_ti(
    .aesthetic = .aesthetic,
    palette = palette,
    reverse = reverse,
    continuous = continuous,
    .colors = ti_colors,
    .palettes = ti_palettes,
    ...
  )

}
