# Copy the code you want to run silently when you load the library into the inside of this function
# Remember to use name spacing (e.g. systemfonts::relevant_function("blah") in doing so!

.onLoad <- function(...) {

  # these are the fonts to load
  fonts_to_hoist <- c("Roboto", "Roboto Condensed", "Lora", "Inconsolata")

  # use font_hoist() to load them all
  fonts_to_hoist |> purrr::map(.f = font_hoist)

}
