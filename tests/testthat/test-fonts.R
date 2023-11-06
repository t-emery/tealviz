

# Test 1: Font Exists and Not Yet Loaded
test_that("Font is successfully hoisted when it exists and is not loaded", {
  expect_message(font_hoist("Roboto Condensed"), "Successfully hoisted")
})
#
# # Test 2: Font Already Loaded
# test_that("Already loaded fonts are recognized", {
#   expect_message(font_hoist("Arial"), "are already loaded")
# })
#
# Test 3: Font Does Not Exist
test_that("Error message is given when the font does not exist", {
  expect_message(font_hoist("Lobster Monster"), "No fonts found")
})



