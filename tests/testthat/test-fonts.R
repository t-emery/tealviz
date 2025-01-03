# Test 1: Font Exists and Not Yet Loaded
test_that("Font is successfully hoisted when it exists and is not loaded", {
  expect_message(font_hoist("Roboto Condensed"), "Successfully hoisted")
})

# Test 2: Font Registration is Idempotent
test_that("Font registration can be safely repeated", {
  # First load the fonts
  first_result <- capture_messages(font_hoist("Arial"))
  # Then try to load them again - should work the same way
  second_result <- capture_messages(font_hoist("Arial"))
  # Verify both operations succeed
  expect_equal(first_result, second_result)
})

# Test 3: Font Does Not Exist
test_that("Error message is given when the font does not exist", {
  expect_message(font_hoist("Lobster Monster"), "No fonts found")
})

# Test download_font function
test_that("download_font handles successful and failed downloads", {
  # Create a temporary directory for test downloads
  temp_dir <- tempdir()
  test_path <- file.path(temp_dir, "test_font.zip")

  # Test successful download with Google Fonts URL
  real_url <- "http://fonts.gstatic.com/s/googlesans/v58/4Ua_rENHsxJlGDuGo1OIlJfC6l_24rlCK1Yo_Iqcsih3SAyH6cAwhX9RFD48TE63OOYKtrwEIJllpyw.ttf" # nolint
  expect_true(download_font(real_url, test_path))
  expect_true(file.exists(test_path))

  # Test failed download with invalid URL
  invalid_url <- "https://invalid.url/nonexistent.zip"
  expect_warning(
    result <- download_font(invalid_url, test_path),
    "Could not resolve hostname|Couldn't resolve host name"
  )
  expect_false(result)

  # Cleanup
  unlink(test_path)
})

test_that("extract_font_files finds font files recursively", {
  # Create temporary test directory structure
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "font_test")
  zip_path <- file.path(temp_dir, "test_fonts.zip")

  # Clean up any existing files first
  unlink(test_dir, recursive = TRUE)
  unlink(zip_path)

  # Verify cleanup was successful
  expect_false(dir.exists(test_dir))
  expect_false(file.exists(zip_path))

  # Create fresh directories
  dir.create(test_dir, recursive = TRUE)
  dir.create(file.path(test_dir, "subfolder"))

  # Create mock font files
  test_fonts <- c(
    file.path(test_dir, "font1.ttf"),
    file.path(test_dir, "font2.otf"),
    file.path(test_dir, "subfolder", "font3.TTF")
  )

  # Create non-font file
  not_font <- file.path(test_dir, "subfolder", "notafont.txt")

  # Create all test files
  sapply(c(test_fonts, not_font), writeLines, text = "test content")

  # Verify test files were created
  expect_true(all(file.exists(c(test_fonts, not_font))))

  # Create test zip file
  withr::with_dir(temp_dir, {
    utils::zip(zip_path, "font_test", flags = "-r")
  })

  # Verify zip file was created
  expect_true(file.exists(zip_path))

  # Test the function
  extracted_files <- extract_font_files(zip_path, temp_dir)

  # Sort both expected and actual files for comparison
  extracted_files <- sort(basename(extracted_files))
  expected_files <- sort(basename(test_fonts))

  # Assertions
  expect_equal(extracted_files, expected_files)
  expect_false(any(grepl("\\.txt$", extracted_files)))

  # Cleanup
  unlink(test_dir, recursive = TRUE)
  unlink(zip_path)
})

test_that("get_os correctly identifies operating system", {
  # Test Windows detection
  local_mocked_bindings(
    get_platform = function() list(OS.type = "windows")
  )
  expect_equal(get_os(), "windows")

  # Test macOS detection
  local_mocked_bindings(
    get_platform = function() list(OS.type = "unix"),
    get_sysinfo = function() c(sysname = "Darwin")
  )
  expect_equal(get_os(), "macos")

  # Test Linux detection
  local_mocked_bindings(
    get_platform = function() list(OS.type = "unix"),
    get_sysinfo = function() c(sysname = "Linux")
  )
  expect_equal(get_os(), "linux")
})

test_that("copy_font_files handles file copying correctly", {
  # Create temporary test directories and files
  temp_src <- tempdir()
  temp_dest <- file.path(temp_src, "dest")
  dir.create(temp_dest)

  # Create mock font files
  font1_path <- file.path(temp_src, "test1.ttf")
  font2_path <- file.path(temp_src, "test2.otf")
  writeLines("mock font data 1", font1_path)
  writeLines("mock font data 2", font2_path)

  # Test successful copy of multiple files
  result <- copy_font_files(
    c(font1_path, font2_path),
    temp_dest
  )
  expect_true(result)
  expect_true(file.exists(file.path(temp_dest, "test1.ttf")))
  expect_true(file.exists(file.path(temp_dest, "test2.otf")))

  # Test copying to non-existent directory
  bad_dest <- file.path(temp_src, "nonexistent")
  expect_warning(
    result <- copy_font_files(font1_path, bad_dest),
    "cannot create file.*No such file or directory"
  )
  expect_false(result)

  # Test copying non-existent source file
  result <- copy_font_files(
    c(font1_path, "nonexistent.ttf"),
    temp_dest
  )
  expect_false(result)

  # Cleanup
  unlink(temp_src, recursive = TRUE)
})

test_that("get_font_directory returns correct paths for each OS", {
  # Test Windows path
  withr::local_envvar(LOCALAPPDATA = "C:/Users/test/AppData/Local")
  expect_equal(
    get_font_directory("windows"),
    "C:/Users/test/AppData/Local/Microsoft/Windows/Fonts"
  )

  # Test macOS path
  withr::local_dir(tempdir())  # Temporarily change working directory
  expect_equal(
    get_font_directory("macos"),
    file.path(path.expand("~"), "Library", "Fonts")
  )

  # Test Linux path
  expect_equal(
    get_font_directory("linux"),
    file.path(path.expand("~"), ".local", "share", "fonts")
  )

  # Test invalid OS
  expect_error(
    get_font_directory("invalid"),
    "Unsupported operating system"
  )
})

# Test install_font_files function for Windows
test_that("install_font_files handles Windows font registration correctly", {
  # Create temporary test environment
  temp_src <- tempdir()
  test_fonts <- c("test1.ttf", "test2.otf")
  font_paths <- file.path(temp_src, test_fonts)

  # Create mock font files
  writeLines("mock font 1", font_paths[1])
  writeLines("mock font 2", font_paths[2])

  # Mock the dependent functions
  local_mocked_bindings(
    get_os = function() "windows",
    get_font_directory = function(os) file.path(temp_src, "fonts"),
    get_system_file = function(...) {
      # Return a mock path for the PowerShell script
      "path/to/install_fonts.ps1"
    },
    run_system2_command = function(command, args, stdout, stderr) {
      expect_equal(command, "powershell")
      expect_equal(args[1:3], c("-NoProfile", "-ExecutionPolicy", "Bypass"))
      expect_equal(args[4], "-File")
      expect_true(grepl("install_fonts.ps1$", args[5]))
      # The remaining args should be the font paths
      expect_equal(args[-(1:5)], font_paths)
      # Return mock successful output
      paste(
        "Successfully installed: test1.ttf",
        "Successfully installed: test2.otf",
        "Installation complete: 2 installed, 0 skipped",
        sep = "\n"
      )
    }
  )

  # Test successful installation
  expect_true(install_font_files(font_paths))

  # Cleanup
  unlink(temp_src, recursive = TRUE)
})

# Test refresh_font_cache for Linux
test_that("refresh_font_cache handles Linux font cache refresh correctly", {
  # Mock our wrapper functions instead of system/system2
  local_mocked_bindings(
    run_system_command = function(command, ...) {
      expect_equal(command, "fc-cache -f -v")
      TRUE
    }
  )

  # Test Linux font cache refresh
  expect_true(refresh_font_cache("linux"))
})

test_that("install_google_font handles full font installation process", {
  # Create mock API response matching actual Google Fonts API structure
  mock_api_response <- readRDS(
    test_path("fixtures", "google_fonts_response.rds")
  )

  # Mock all dependent functions
  local_mocked_bindings(
    # Mock API call
    get_from_json = function(url) {
      expect_match(
        url,
        "^https://www.googleapis.com/webfonts/v1/webfonts\\?key=.*&family=Roboto\\+Condensed$" # nolint
      )
      mock_api_response
    },
    # Mock download function
    download_font = function(url, dest_path) {
      # Verify URL format
      expect_match(
        url,
        "^https://fonts.gstatic.com/s/robotocondensed/.*\\.ttf$"
      )
      # Verify destination path matches expected pattern for any variant
      expect_match(
        dest_path,
        "Roboto_Condensed_(100|200|300|regular|500|600|700|800|900|.*italic)\\.(ttf|otf)$" # nolint
      )
      # Simulate successful download
      writeLines("mock font data", dest_path)
      TRUE
    },
    # Mock font installation - expect all 18 variants
    install_font_files = function(font_files) {
      # Expect all 18 variants (9 weights × 2 styles)
      expect_length(font_files, 18)
      expect_true(all(file.exists(font_files)))
      # Verify we have both regular and italic variants
      expect_true(any(grepl("italic", basename(font_files))))
      # Fix the regular expression for matching non-italic variants
      expect_true(any(grepl("regular|[1-9]00($|[^i])", basename(font_files))))
      TRUE
    }
  )

  # Test successful installation
  expect_true(install_google_font("Roboto Condensed"))

  # Test with failed API response
  local_mocked_bindings(
    get_from_json = function(url) stop("API error")
  )
  expect_false(install_google_font("Roboto Condensed"))

  # Test with font not found in API response
  local_mocked_bindings(
    get_from_json = function(url) {
      list(kind = "webfonts#webfontList", items = data.frame())
    }
  )
  expect_false(install_google_font("Roboto Condensed"))

  # Test with failed downloads
  local_mocked_bindings(
    get_from_json = function(url) mock_api_response,
    download_font = function(url, dest_path) FALSE
  )
  expect_false(install_google_font("Roboto Condensed"))

  # Test with failed installation
  local_mocked_bindings(
    get_from_json = function(url) mock_api_response,
    download_font = function(url, dest_path) TRUE,
    install_font_files = function(font_files) FALSE
  )
  expect_false(install_google_font("Roboto Condensed"))
})
