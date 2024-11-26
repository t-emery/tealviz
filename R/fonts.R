#' @title
#' Make font families available in ggplot2
#'
#' @description
#' Sometimes, even when fonts are loaded on a computer, they are not
#' immediately available for use in R. This function fixes that.
#'
#' @details
# nolint start
#' See [June Choe's blog post explaining this](
#' https://yjunechoe.github.io/posts/2021-06-24-setting-up-and-debugging-custom-fonts/
#' ).
# nolint end
#' This function requires the user to have already downloaded the
#' font on their computer, and to have set up `ragg` as described
#' in the blog post.
#'
#' @param family_name name of the font family.
#' @param silent do you want to suppress the message?
#' @param check_only logical. If TRUE, only checks if fonts are available
#'   without attempting to register them.
#'
#' @return The function returns a message indicating whether the font
#'   was successfully loaded or not.
#' @export
#'
#' @examples
#' # load Roboto Condensed (must be loaded from Google Fonts on system already)
#' font_hoist("Roboto Condensed")
#'
#' # Try loading a font not on your computer, or that doesn't exist.
#' # It will give an informative error message.
#' font_hoist("Lobster Monster")


# Function to make system fonts available in R
font_hoist <- function(family_name, silent = FALSE, check_only = FALSE) {

  # ANSI escape codes for text colors
  green <- "\033[32m"
  red <- "\033[31m"
  reset <- "\033[0m"

  # Initialize vectors to track fonts that were and were not
  # successfully registered
  successful_fonts <- c()
  failed_fonts <- c()
  already_loaded_fonts <- c()

  # Step 1: Fetch all system fonts using the systemfonts package

  font_specs <- systemfonts::system_fonts() |>
    dplyr::filter(family == family_name) |>
    dplyr::mutate(family = paste(family, style)) |>
    dplyr::select(plain = path, name = family)


  # Step 2: Check if any fonts were found for the given family name
  if (nrow(font_specs) == 0) {
    if (!silent) {
      message(paste0(
        red,
        "No fonts found for the family '", family_name, "'. \n",
        "Make sure that the font is downloaded to your computer, ",
        "and that you can find it using systemfonts::system_fonts(). ",
        "If you are having trouble, check out June Choe's helpful blog post ",
        "on setting up and debugging custom fonts: ",
        "https://yjunechoe.github.io/posts/2021-06-24-setting-up-and-debugging-custom-fonts/", # nolint
        reset
      ))
    }
    return(list(specs = NULL, available = FALSE))
  }

  if (check_only) {
    return(list(specs = font_specs, available = TRUE))
  }

  # Step 3: Define a function to safely register a font
  safe_register_font <- function(plain, name) {
    tryCatch({
      systemfonts::register_font(plain = plain, name = name)
      # Update successful fonts vector
      successful_fonts <<- c(successful_fonts, name)
    }, error = function(e) {
      # Differentiate between already loaded fonts and other errors
      if (
        grepl("A system font with that family name already exists", e$message)
      ) {
        already_loaded_fonts <<- c(already_loaded_fonts, name)
      } else {
        # Update failed fonts vector
        failed_fonts <<- c(failed_fonts, name)
      }
    })
  }

  # Step 4: Register each font
  purrr::pwalk(as.list(font_specs), safe_register_font)

  # Step 5: Display a summary message
  if (length(successful_fonts) > 0) {
    message(paste0(
      green,
      "Successfully hoisted ", length(successful_fonts),
      " font(s) for the family '", family_name, "': ",
      paste(successful_fonts, collapse = ", "),
      reset
    ))
  }

  if (length(already_loaded_fonts) > 0) {
    message(paste0(
      green,
      "The following font(s) for the family '",
      family_name,
      "' are already loaded: ",
      paste(already_loaded_fonts, collapse = ", "),
      reset
    ))
  }

  if (length(failed_fonts) > 0) {
    message(paste0(
      red,
      "Failed to hoist ", length(failed_fonts), " font(s) for the family '",
      family_name, "': ",
      paste(failed_fonts, collapse = ", "),
      reset
    ))
  }

  # Step 6: Return the list of font specifications invisibly
  return(invisible(font_specs))
}
