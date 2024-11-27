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
    "Couldn't resolve host name"
  )
  expect_false(result)

  # Cleanup
  unlink(test_path)
})

test_that("extract_font_files finds font files recursively", {
  # Create temporary test directory structure
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "font_test")
  dir.create(test_dir, recursive = TRUE)
  dir.create(file.path(test_dir, "subfolder"))

  # Create mock font files
  writeLines("", file.path(test_dir, "font1.ttf"))
  writeLines("", file.path(test_dir, "font2.otf"))
  writeLines("", file.path(test_dir, "subfolder", "font3.TTF"))
  writeLines("", file.path(test_dir, "subfolder", "notafont.txt"))

  # Create test zip file
  zip_path <- file.path(temp_dir, "test_fonts.zip")
  withr::with_dir(temp_dir, {
    utils::zip(zip_path, "font_test", flags = "-r")
  })

  # Test the function
  extracted_files <- extract_font_files(zip_path, temp_dir)

  # Assertions
  expect_length(extracted_files, 3)
  expect_true(
    all(grepl("\\.(?:ttf|otf)$", extracted_files, ignore.case = TRUE))
  )
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

# TODO: Enhance this test to check for the actual cache refresh behavior
test_that("refresh_font_cache handles different OS behaviors correctly", {
  # Mock our wrapper functions instead of system/system2
  local_mocked_bindings(
    run_system_command = function(...) TRUE,
    run_system2_command = function(...) TRUE
  )

  # Test Linux font cache refresh
  expect_true(refresh_font_cache("linux"))

  # Test Windows font cache refresh
  expect_true(refresh_font_cache("windows"))

  # Test macOS (should return TRUE without doing anything)
  expect_true(refresh_font_cache("macos"))

  # Test with failed system calls
  local_mocked_bindings(
    run_system_command = function(...) stop("Command failed"),
    run_system2_command = function(...) stop("Command failed")
  )

  # Should still return TRUE even if commands fail
  # (we don't want font installation to fail just because cache refresh failed)
  expect_true(refresh_font_cache("linux"))
  expect_true(refresh_font_cache("windows"))
})

test_that("install_font_files handles font installation correctly", {
  # Create temporary test environment
  temp_src <- tempdir()
  test_fonts <- c("test1.ttf", "test2.otf")
  font_paths <- file.path(temp_src, test_fonts)

  # Create mock font files
  writeLines("mock font 1", font_paths[1])
  writeLines("mock font 2", font_paths[2])

  # Mock the dependent functions
  local_mocked_bindings(
    get_os = function() "linux",
    get_font_directory = function(os) file.path(temp_src, "fonts"),
    refresh_font_cache = function(os) TRUE
  )

  # Test when font directory doesn't exist
  expect_true(install_font_files(font_paths))
  expect_true(dir.exists(file.path(temp_src, "fonts")))

  # Verify files were copied
  expect_true(file.exists(file.path(temp_src, "fonts", "test1.ttf")))
  expect_true(file.exists(file.path(temp_src, "fonts", "test2.otf")))

  # Test with non-existent source files
  nonexistent_paths <- c(
    file.path(temp_src, "nonexistent1.ttf"),
    file.path(temp_src, "nonexistent2.otf")
  )
  expect_false(install_font_files(nonexistent_paths))

  # Cleanup
  unlink(temp_src, recursive = TRUE)
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
      # Verify URL and destination path format
      expect_match(
        url,
        "^https://fonts.gstatic.com/s/robotocondensed/v25/.*\\.ttf$"
      )
      expect_match(
        dest_path,
        "Roboto_Condensed_.*\\.(ttf|otf)$"
      )
      # Simulate successful download
      writeLines("mock font data", dest_path)
      TRUE
    },
    # Mock font installation
    install_font_files = function(font_files) {
      expect_length(font_files, 2)
      expect_true(all(file.exists(font_files)))
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
