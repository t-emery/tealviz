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
    dplyr::mutate(family = paste(family, style)) |> # nolint
    dplyr::select(plain = path, name = family) # nolint


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
      successful_fonts <<- c(successful_fonts, name)
    }, error = function(e) {
      failed_fonts <<- c(failed_fonts, name)
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

# Create a mockable version of jsonlite::fromJSON
get_from_json <- function(...) {
  jsonlite::fromJSON(...)
}

#' Download and install a Google Font
#' @description Downloads and installs a font family from Google Fonts using the
#'   official API
#' @param font_name Character string of the font name (e.g. "Dancing Script")
#' @param api_key Google Fonts API key. Defaults to a hard-coded key that
#'   has very generous usage limits. If you need your own key, you can get one
#'   from the Google Cloud Console.
#' @return Logical indicating whether the font was successfully installed
install_google_font <- function(
  font_name,
  api_key = "AIzaSyDOr3jWLtl4IP08yNaddV61_40f0YByPHo"
) {
  # Fetch font information from API for specific font
  api_url <- sprintf(
    "https://www.googleapis.com/webfonts/v1/webfonts?key=%s&family=%s",
    api_key,
    # Replace spaces with + for Google Fonts API format
    gsub(" ", "+", font_name)
  )

  response <- try({
    get_from_json(api_url)
  }, silent = TRUE)

  if (inherits(response, "try-error")) {
    message("Failed to fetch font information from Google Fonts API")
    return(FALSE)
  }

  # Check if any fonts were returned
  if (length(response$items) == 0) {
    message("Font '", font_name, "' not found in Google Fonts")
    return(FALSE)
  }

  # Use the first (and should be only) item
  font_info <- response$items[1, ]

  # Create temporary directory for downloads
  temp_dir <- tempdir()
  font_files <- c()

  # Download each variant
  for (variant in names(font_info$files)) {
    url <- font_info$files[[variant]]
    file_ext <- if(grepl("\\.ttf$", url)) ".ttf" else ".otf"
    dest_path <- file.path(temp_dir, paste0(
      gsub(" ", "_", font_name),
      "_",
      variant,
      file_ext
    ))

    message("Downloading variant: ", variant)
    if (download_font(url, dest_path)) {
      font_files <- c(font_files, dest_path)
    }
  }

  if (length(font_files) == 0) {
    message("No font files were successfully downloaded")
    return(FALSE)
  }

  # Install the downloaded files
  success <- install_font_files(font_files)

  # Cleanup
  unlink(temp_dir, recursive = TRUE)

  return(success)
}

#' Download font file
#' @description Downloads a font file from a direct URL
#' @param url The direct download URL for the font file
#' @param dest_path File path where the font file should be saved
#' @return Logical indicating whether the download was successful
#' @keywords internal
download_font <- function(url, dest_path) {
  message("Downloading from: ", url)

  result <- try({
    utils::download.file(url, dest_path, mode = "wb", quiet = TRUE)
  }, silent = TRUE)

  if (inherits(result, "try-error")) {
    message("Download failed: ", attr(result, "condition")$message)
    return(FALSE)
  }

  return(file.exists(dest_path) && file.size(dest_path) > 0)
}

#' Extract font files from zip archive
#' @description Extracts .ttf and .otf files from a downloaded font zip archive
#' @details Searches recursively through the extracted zip contents to find all
#'   font files, supporting both TrueType (.ttf) and OpenType (.otf) formats.
#' @param zip_path Path to the downloaded zip file
#' @param extract_dir Directory where the zip contents should be extracted
#' @return Character vector of full paths to the extracted font files
#' @keywords internal
extract_font_files <- function(zip_path, extract_dir) {
  # Add debug messages
  message("Checking zip file at: ", zip_path)

  if (!file.exists(zip_path)) {
    warning("Zip file does not exist")
    return(character(0))
  }

  if (file.size(zip_path) == 0) {
    warning("Zip file is empty")
    return(character(0))
  }

  # Try to list contents before extracting
  zip_contents <- try(utils::unzip(zip_path, list = TRUE), silent = TRUE)
  if (inherits(zip_contents, "try-error")) {
    warning("Invalid zip file format")
    return(character(0))
  }

  message(
    "Zip contents: ", paste(utils::head(zip_contents$Name), collapse = ", ")
  )

  # Attempt to unzip with error handling
  result <- tryCatch({
    utils::unzip(zip_path, exdir = extract_dir)
    TRUE
  }, error = function(e) {
    warning("Failed to extract zip file: ", e$message)
    FALSE
  })

  if (!result) {
    return(character(0))
  }

  # Find all font files recursively
  font_files <- list.files(
    extract_dir,
    pattern = "\\.(?:ttf|otf)$",
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE
  )
  return(font_files)
}

#' Install font files to appropriate system directory
#' @description Copies font files to the system's font directory and refreshes
#'   the font cache
#' @details Determines the correct font directory based on the operating system,
#'   creates it if necessary, copies the font files, and triggers a
#'   system-specific font cache refresh when required.
#' @param font_files Character vector of paths to the font files to install
#' @return Logical indicating whether all files were successfully installed
#' @keywords internal
install_font_files <- function(font_files) {
  os <- get_os()
  font_dir <- get_font_directory(os)

  if (!dir.exists(font_dir)) {
    dir.create(font_dir, recursive = TRUE)
  }

  # Copy files to font directory
  success <- copy_font_files(font_files, font_dir)

  # Refresh font cache if needed
  if (success) {
    refresh_font_cache(os)
  }

  return(success)
}

# Create mockable versions of .Platform and Sys.info
get_platform <- function() .Platform
get_sysinfo <- function() Sys.info()

# Start of Selection
#' Determine the current operating system
#'
#' This function identifies the operating system on which R is running.
#' It returns a character string indicating the OS type, which can be
#' one of the following: "windows", "macos", or "linux".
#'
#' @return A character string representing the operating system:
#'   "windows" for Windows OS,
#'   "macos" for macOS,
#'   "linux" for Linux.
#' @export
#' 
#' @examples
#' # Get the current operating system
#' os_type <- get_os()
#' print(os_type)
# End of Selection
get_os <- function() {
  if (get_platform()$OS.type == "windows") {
    return("windows")
  } else if (get_sysinfo()["sysname"] == "Darwin") {
    return("macos")
  } else {
    return("linux")
  }
}

#' Get appropriate font directory for current OS
#' @description Determines the correct font installation directory for the
#'   current operating system
#' @details Returns the standard font directory path for Windows
#'   (LocalAppData/Microsoft/Windows/Fonts), macOS (~/Library/Fonts), or Linux
#'   (~/.local/share/fonts).
#' @param os Operating system identifier ("windows", "macos", or "linux")
#' @return Character string containing the path to the system's font directory
#' @keywords internal
get_font_directory <- function(os) {
  switch(os,
    "windows" = file.path(
      Sys.getenv("LOCALAPPDATA"),
      "Microsoft",
      "Windows",
      "Fonts"
    ),
    "macos" = file.path(path.expand("~"), "Library", "Fonts"),
    "linux" = file.path(path.expand("~"), ".local", "share", "fonts"),
    stop("Unsupported operating system")
  )
}

#' Copy font files to destination directory
#' @description Copies font files to the system font directory
#' @details Attempts to copy each font file to the destination directory,
#'   overwriting existing files if necessary. Returns FALSE if any file copy
#'   operation fails.
#' @param font_files Character vector of paths to the source font files
#' @param dest_dir Path to the destination directory where fonts should be
#'   installed
#' @return Logical indicating whether all files were successfully copied
#' @keywords internal
copy_font_files <- function(font_files, dest_dir) {
  success <- TRUE
  for (file in font_files) {
    if (!file.copy(
      file,
      file.path(dest_dir, basename(file)),
      overwrite = TRUE
    )) {
      success <- FALSE
    }
  }
  return(success)
}

# Create mockable versions of system calls
run_system_command <- function(...) system(...)
run_system2_command <- function(...) system2(...)

#' Refresh system font cache if needed
#' @description Updates the system's font cache after installing new fonts
#' @details On Linux, runs `fc-cache`. On Windows, uses PowerShell to register
#'   fonts. No action needed for macOS.
#' @param os Operating system identifier ("windows", "macos", or "linux")
#' @return Logical indicating whether the cache refresh was successful
#' @keywords internal
refresh_font_cache <- function(os) {
  if (os == "linux") {
    # Run fc-cache on Linux
    tryCatch({
      run_system_command(
        "fc-cache -f -v",
        ignore.stdout = TRUE,
        ignore.stderr = TRUE
      )
    }, error = function(e) {
      # Silently continue if command fails
      TRUE
    })
  } else if (os == "windows") {
    # PowerShell command to register fonts
    ps_cmd <- paste(
      "Get-ChildItem -Path",
      shQuote(get_font_directory("windows")),
      "-Include ('*.ttf','*.otf') -Recurse |",
      "ForEach-Object {Add-Type -AssemblyName PresentationCore;",
      "[System.Windows.Media.Fonts]::GetFontFamilies($_.FullName)}"
    )
    tryCatch({
      run_system2_command(
        "powershell",
        args = c("-Command", ps_cmd),
        stdout = TRUE,
        stderr = TRUE
      )
    }, error = function(e) {
      # Silently continue if command fails
      TRUE
    })
  }
  # macOS doesn't need cache refresh
  return(TRUE)
}
