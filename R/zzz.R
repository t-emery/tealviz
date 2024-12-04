# Package startup message and font loading
.onLoad <- function(...) {
  # Define required fonts
  fonts_to_check <- c(
    "Roboto",
    "Roboto Condensed",
    "Lora",
    "Inconsolata"
  )

  # Check font availability using systemfonts with error handling
  tryCatch({
    available_fonts <- systemfonts::system_fonts()
    missing_fonts <- fonts_to_check[!fonts_to_check %in% available_fonts$family]

    # Store missing fonts in package environment for .onAttach
    pkgenv <- new.env()
    pkgenv$missing_fonts <- missing_fonts
    assign("tealviz_env", pkgenv, envir = asNamespace("tealviz"))

    # Register available fonts with any special features needed
    available_fonts <- fonts_to_check[
      fonts_to_check %in% available_fonts$family
    ]
    if (length(available_fonts) > 0) {
      # Register lining numbers variant for each available font
      for (font in available_fonts) {
        tryCatch({
          if ("lnum" %in% unlist(textshaping::get_font_features(font))) {
            systemfonts::register_variant(
              name = paste0(font, "-lining"),
              family = font,
              features = systemfonts::font_feature(numbers = "lining")
            )
          }
        }, error = function(e) {
          assign("font_error", list(font = font, error = e), envir = pkgenv)
        })
      }
    }
  }, error = function(e) {
    assign("check_error", e, envir = pkgenv)
  })
}

.onAttach <- function(libname, pkgname) {
  pkgenv <- get("tealviz_env", envir = asNamespace("tealviz"))

  # Handle missing fonts message and installation prompt
  if (length(pkgenv$missing_fonts) > 0) {
    missing_fonts_msg <- paste(pkgenv$missing_fonts, collapse = ", ")

    msg <- paste0(
      sprintf("Note: Missing required fonts: %s\n", missing_fonts_msg),
      "Would you like to install them from Google Fonts? (y/n): "
    )

    # Only prompt if in interactive mode
    if (interactive()) {
      response <- readline(msg)
      if (tolower(substr(response, 1, 1)) == "y") {
        packageStartupMessage("\nAttempting to install fonts...")

        results <- lapply(pkgenv$missing_fonts, function(font) {
          if (install_google_font(font)) {
            packageStartupMessage(sprintf("Successfully installed '%s'", font))
            TRUE
          } else {
            packageStartupMessage(sprintf("Failed to install '%s'", font))
            FALSE
          }
        })

        if (any(unlist(results))) {
          packageStartupMessage(
            "\nPlease restart R for the new fonts to be available."
          )
        }
      } else {
        # Show the original URLs message if user declines
        font_urls <- sapply(pkgenv$missing_fonts, function(font) {
          url_font_name <- gsub(" ", "+", font)
          sprintf("https://fonts.google.com/specimen/%s", url_font_name)
        })

        msg <- paste0(
          "\nYou can manually install the fonts from:\n",
          paste0(
            " - ", pkgenv$missing_fonts, "\n    ", font_urls,
            collapse = "\n"
          ),
          "\n",
          "After installation, restart R and reload the package."
        )
        packageStartupMessage(msg)
      }
    } else {
      # In non-interactive mode, just show the URLs
      font_urls <- sapply(pkgenv$missing_fonts, function(font) {
        url_font_name <- gsub(" ", "+", font)
        sprintf("https://fonts.google.com/specimen/%s", url_font_name)
      })

      msg <- paste0(
        "Note: The following required fonts are not installed:\n",
        paste0(
          " - ", pkgenv$missing_fonts, "\n    ", font_urls,
          collapse = "\n"
        ),
        "\n",
        "Please install them from the URLs above.\n",
        "After installation, restart R and reload the package."
      )
      packageStartupMessage(msg)
    }
  }

  # Handle font registration errors
  if (exists("font_error", envir = pkgenv)) {
    packageStartupMessage(sprintf(
      "Note: Could not register lining numbers for font '%s': %s",
      pkgenv$font_error$font,
      conditionMessage(pkgenv$font_error$error)
    ))
  }

  # Handle font availability check errors
  if (exists("check_error", envir = pkgenv)) {
    packageStartupMessage(sprintf(
      "Warning: Error checking font availability: %s",
      conditionMessage(pkgenv$check_error)
    ))
  }
}
