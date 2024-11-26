

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
