
test_that("theme_ti works", {
  expect_s3_class(theme_ti(), "theme")
})

test_that("theme_ti works with void", {
  expect_s3_class(theme_ti(void = TRUE), "theme")
})

