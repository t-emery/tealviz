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
  real_url <- "https://fonts.google.com/download?family=Roboto"
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
