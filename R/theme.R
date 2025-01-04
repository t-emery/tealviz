# Helper function to create base theme elements
create_base_theme <- function(base_font, base_text_size, colors) {
  ggplot2::theme_minimal(base_family = base_font, base_size = base_text_size) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(color = colors$grid_lines),
      panel.grid.minor = ggplot2::element_line(color = colors$grid_lines),
      axis.ticks = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(
        color = colors$light_text,
        family = base_font
      ),
      axis.title.x = ggplot2::element_text(
        margin = ggplot2::margin(base_text_size, 0, 0, 0),
        color = colors$light_text,
        family = base_font
      ),
      axis.title.y = ggplot2::element_text(
        margin = ggplot2::margin(0, base_text_size, 0, 0)
      )
    )
}

# Helper function to create text elements
create_text_elements <- function(
  title_font,
  base_font,
  colors,
  background_col,
  base_text_size
) {
  ggplot2::theme(
    text = ggplot2::element_text(color = colors$light_text),
    plot.title = ggtext::element_markdown(
      family = title_font,
      color = colors$dark_text,
      size = ggplot2::rel(2),
      margin = ggplot2::margin(
        base_text_size * 1.5, 0,
        base_text_size / 2,
        base_text_size,
        unit = "points"
      )
    ),
    strip.text = ggplot2::element_text(
      family = base_font,
      color = colors$dark_text,
      size = ggplot2::rel(1.3)
    ),
    strip.background = ggplot2::element_rect(
      color = background_col,
      fill = background_col
    ),
    legend.title = ggtext::element_markdown(
      color = colors$dark_text,
      family = base_font,
      size = ggplot2::rel(1),
      lineheight = 1.3
    ),
    legend.text = ggplot2::element_text(
      color = colors$light_text,
      family = base_font,
      size = ggplot2::rel(0.9)
    ),
    plot.caption = ggplot2::element_text(
      color = colors$light_text,
      family = base_font,
      size = ggplot2::rel(0.75)
    )
  )
}

#' theme_ti
#'
#' @param title_font The default title is "Roboto". To swich to the matching
#' Serif font picked out in the design phase (Lora), use `title_font = "fancy"`.
#' To change to a different font (e.g. to match up with an external publication
#' requirement), simply change as desired (e.g. \"Times New Roman\").
#' Make sure you've installed the fonts you want to use on your device first!
#' @param base_font The default font is "Roboto"). If you want to use a
#' different font (e.g. to match up with an external publication requirement),
#' simply change as desired (e.g. "Arial").
#' Make sure you've installed the fonts you want to use on your device first!
#' @param void Logical (`TRUE/FALSE`). If `TRUE`, all grid lines and axes are
#' removed. This is useful when creating pie/donut charts.
#' @param show_grid_lines Logical (`TRUE/FALSE`). If `FALSE`, all grid lines are
#' removed but the axis text is retained.
#' @param base_text_size Base text size in pt. The relative size of the title /
#' subtitle text and of the margins is derived from this.
#' @param background_color Logical. The default (`TRUE`) adds a light color to
#' the background in line with the Teal Insights color palette. Change to
#' `FALSE` for a white background.
#' @param .colors Used to read in `ti_colors`. Please leave as is!
#'
#' @return No object is returned. Instead, the theme is applied to a ggplot
#' plot.
#' @export
#'
# Main theme function
theme_ti <- function(
  title_font = "Roboto Condensed Bold",
  base_font = "Roboto Condensed",
  void = FALSE,
  show_grid_lines = TRUE,
  base_text_size = 11,
  background_color = TRUE,
  .colors = ti_colors
) {
  # Process font and background settings
  title_font <- if (title_font == "fancy") "Lora" else title_font
  background_col <- if (background_color) .colors$background else "#FFFFFF"

  # Create base theme
  bespoke_theme <- create_base_theme(base_font, base_text_size, .colors)

  # Apply void theme if requested
  if (void) {
    bespoke_theme <- bespoke_theme +
      ggplot2::theme_void(base_family = base_font)
  }

  # Add text elements
  bespoke_theme <- bespoke_theme +
    create_text_elements(
      title_font,
      base_font,
      .colors,
      background_col,
      base_text_size
    ) +
    ggplot2::theme(
      plot.caption.position = "plot",
      plot.title.position = "plot",
      plot.subtitle = ggtext::element_textbox_simple(
        family = base_font,
        valign = 0,
        vjust = 1,
        size = ggplot2::rel(1.2),
        lineheight = 1.3,
        margin = ggplot2::margin(
          base_text_size / 2, 0,
          base_text_size * 1.5,
          base_text_size,
          unit = "points"
        )
      ),
      plot.background = ggplot2::element_rect(
        color = background_col,
        fill = background_col
      ),
      panel.background = ggplot2::element_rect(
        color = background_col,
        fill = background_col
      ),
      plot.margin = ggplot2::margin(
        t = base_text_size,
        r = base_text_size * 1.5,
        b = base_text_size,
        l = base_text_size * 1.5,
        unit = "points"
      )
    )

  # Remove grid lines if requested
  if (!show_grid_lines) {
    bespoke_theme <- bespoke_theme +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank()
      )
  }

  bespoke_theme
}


#' ggplot Theme version 1
#'
#' @param base_size Text Size
#' @param base_family Text Font
#' @param title_size Size of Title Text
#' @param subtitle_size Size of Subtitle Text
#' @param axis_size Size of Axis Text
#' @param caption_size Size of Caption Text
#'
theme_ti_1 <- function(
  base_size = 12,
  base_family = "Roboto Condensed",
  title_size = 18,
  subtitle_size = 12,
  axis_size = 10,
  caption_size = 8
) {
  # Start with ggplot2's theme_minimal and then modify further
  bespoke_theme <- ggplot2::theme_minimal(
    base_size = base_size,
    base_family = base_family
  ) +
    ggplot2::theme(
      # Set up the text elements with the specified fonts and sizes
      text = ggplot2::element_text(family = base_family, size = base_size),
      plot.title = ggplot2::element_text(
        family = paste(base_family, "Bold"),
        size = title_size,
        hjust = 0
      ),
      plot.subtitle = ggplot2::element_text(
        family = paste(base_family, "Italic"),
        size = subtitle_size,
        hjust = 0,
        margin = ggplot2::margin(0.2, 0, 0.5, 0, "cm")
      ),
      plot.caption = ggplot2::element_text(
        family = paste(base_family, "Light Italic"),
        size = caption_size,
        margin = ggplot2::margin(0.5, 0, 0, 0, "cm"),
        hjust = 1
      ),
      axis.title = ggplot2::element_text(
        family = paste(base_family, "Bold"),
        size = axis_size
      ),
      axis.text = ggplot2::element_text(
        family = paste(base_family, "Light"),
        size = axis_size - 2,
        colour = "#626262"
      ),
      legend.position = "bottom"
    )
  bespoke_theme
}


#' ggplot Theme version 2
#'
#' @param base_size Text Size
#' @param base_family Text Font
#' @param title_size Size of Title Text
#' @param subtitle_size Size of Subtitle Text
#' @param axis_size Size of Axis Text
#' @param caption_size Size of Caption Text
#'
theme_ti_2 <- function(
  base_size = 12,
  base_family = "Roboto Condensed",
  title_size = 18,
  subtitle_size = 12,
  axis_size = 10,
  caption_size = 8
) {

  # Start with the minimal theme as a base and apply customizations
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      # Set global text appearance using the specified font family and base size
      text = ggplot2::element_text(family = base_family, size = base_size),
      # Customize the plot title
      plot.title = ggplot2::element_text(
        family = paste(base_family, "Bold"),
        size = title_size,
        hjust = 0
      ),
      # Customize the plot subtitle
      plot.subtitle = ggplot2::element_text(
        family = paste(base_family, "Light Italic"),
        size = subtitle_size,
        hjust = 0,
        margin = ggplot2::margin(0.2, 0, 0.5, 0, "cm")
      ),
      # Customize the plot caption
      plot.caption = ggplot2::element_text(
        family = paste(base_family, "Light Italic"),
        size = caption_size,
        margin = ggplot2::margin(0.5, 0, 0, 0, "cm"),
        hjust = 1
      ),
      # Customize the axis titles
      axis.title = ggplot2::element_text(
        family = paste(base_family, "Bold"),
        size = axis_size
      ),
      # Customize the axis text
      axis.text = ggplot2::element_text(
        family = paste(base_family, "Light"),
        size = axis_size - 2,
        colour = "#626262"
      ),
      # Set the plot background
      plot.background = ggplot2::element_rect(fill = "#F5F5F5", color = NA),
      # Set the panel background
      panel.background = ggplot2::element_rect(fill = "#F5F5F5", color = NA),
      # Position the legend at the bottom of the plot
      legend.position = "bottom",
      # Remove panel grid lines
      panel.grid = ggplot2::element_blank(),
      # Customize axis lines
      axis.line = ggplot2::element_line(colour = "#626262")
    )
}
