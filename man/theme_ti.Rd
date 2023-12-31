% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/theme.R
\name{theme_ti}
\alias{theme_ti}
\title{theme_ti}
\usage{
theme_ti(
  title_font = "Roboto Condensed Bold",
  base_font = "Roboto Condensed",
  void = FALSE,
  show_grid_lines = TRUE,
  base_text_size = 11,
  background_color = TRUE,
  .colors = ti_colors
)
}
\arguments{
\item{title_font}{The default title is "Roboto". To swich to the matching Serif font picked out in the design phase (Lora), use \code{title_font = "fancy"}.
To change to a different font (e.g. to match up with an external publication requirement), simply change as desired (e.g. \"Times New Roman\").
Make sure you've installed the fonts you want to use on your device first!}

\item{base_font}{The default font is "Roboto"). If you want to use a different font (e.g. to match up with an external publication requirement),
simply change as desired (e.g. "Arial").
Make sure you've installed the fonts you want to use on your device first!}

\item{void}{Logical (\code{TRUE/FALSE}). If \code{TRUE}, all grid lines and axes are removed. This is useful when creating pie/donut charts.}

\item{show_grid_lines}{Logical (\code{TRUE/FALSE}). If \code{FALSE}, all grid lines are removed but the axis text is retained.}

\item{base_text_size}{Base text size in pt. The relative size of the title / subtitle text and of the margins is derived from this.}

\item{background_color}{Logical. The default (\code{TRUE}) adds a light color to the background in line with the Teal Insights color palette. Change to \code{FALSE} for a white background.}

\item{.colors}{Used to read in \code{ti_colors}. Please leave as is!}
}
\value{
No object is returned. Instead, the theme is applied to a ggplot plot.
}
\description{
theme_ti
}
