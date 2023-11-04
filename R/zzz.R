# Copy the code you want to run silently when you load the library into the inside of this function
# Remember to use name spacing (e.g. systemfonts::relevant_function("blah") in doing so!

.onLoad <- function(...) {

  font_hoist <- function(family_name, silent = FALSE) {
    font_specs <- systemfonts::system_fonts() |>
      dplyr::filter(family == {{family_name}}) |>
      dplyr::mutate(family = paste(family, style)) |>
      dplyr::select(plain = path, name = family)

    purrr::pwalk(as.list(font_specs), systemfonts::register_font)

    if (!silent)  message(paste0("Hoisted ", nrow(font_specs), " variants:\n",
                                 paste(font_specs$name, collapse = "\n")))
  }

  font_hoist("Roboto Condensed")

}
