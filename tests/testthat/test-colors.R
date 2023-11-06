

#library(testthat)
#library(ggplot2)

# Test ti_colors hex codes
test_that("ti_colors hex codes are correct", {
  expect_equal(ti_colors$dark_text, "#00101A")
  expect_equal(ti_colors$light_text, "#323F47")
  expect_equal(ti_colors$background, "#FEFCF7")
  expect_equal(ti_colors$dark_teal, "#002f49")
  expect_equal(ti_colors$mid_teal, "#004C6F")
  expect_equal(ti_colors$teal, "#0079a8")
  expect_equal(ti_colors$mint, "#86AFA1")
  expect_equal(ti_colors$muted_gold, "#d9b74b")
  expect_equal(ti_colors$orange, "#ad712f")
  expect_equal(ti_colors$dark_orange, "#804126")
  expect_equal(ti_colors$crimson, "#53121d")
  expect_equal(ti_colors$positive, "#397c54")
  expect_equal(ti_colors$pos_neg_neutral, "#99abb6")
  expect_equal(ti_colors$negative, "#742428")
  expect_equal(ti_colors$republican, "#A90E0E")
  expect_equal(ti_colors$rep_dem_neutral, "#FEFCF7")
  expect_equal(ti_colors$democrat, "#042A9C")
  expect_equal(ti_colors$gold, "#f8c03e")
  expect_equal(ti_colors$na_value, "#848B90")
  expect_equal(ti_colors$grid_lines, "#F3F3F4")
})

# Test ti_palettes color sequences
test_that("ti_palettes returns correct color hex codes", {
  expect_equal(ti_palettes$default, c(ti_colors$dark_teal,
                                      ti_colors$teal,
                                      ti_colors$muted_gold,
                                      ti_colors$dark_orange))
  expect_equal(ti_palettes$teal_to_gold, c(ti_colors$dark_teal,
                                           ti_colors$teal,
                                           ti_colors$gold))
  expect_equal(ti_palettes$teal_to_crimson, c(ti_colors$mid_teal,
                                              ti_colors$muted_gold,
                                              ti_colors$orange,
                                              ti_colors$crimson))
  expect_equal(ti_palettes$cool_colors, c(ti_colors$dark_teal,
                                          ti_colors$teal,
                                          ti_colors$mint))
  expect_equal(ti_palettes$warm_colors, c(ti_colors$muted_gold,
                                          ti_colors$orange,
                                          ti_colors$crimson))
  expect_equal(ti_palettes$skip_gold, c(ti_colors$dark_teal,
                                        ti_colors$teal,
                                        ti_colors$mint,
                                        ti_colors$orange,
                                        ti_colors$dark_orange))
  expect_equal(ti_palettes$neg_to_pos, c(ti_colors$negative,
                                         ti_colors$pos_neg_neutral,
                                         ti_colors$positive))
  expect_equal(ti_palettes$us_rep_to_dem, c(ti_colors$republican,
                                            ti_colors$rep_dem_neutral,
                                            ti_colors$democrat))
  expect_equal(ti_palettes$divergent, c(ti_colors$dark_teal,
                                        ti_colors$teal,
                                        ti_colors$mint,
                                        ti_colors$muted_gold,
                                        ti_colors$dark_orange,
                                        ti_colors$crimson))
})


# Test scale_color_ti function
test_that("scale_color_ti returns the correct discrete scale", {
  scale <- scale_color_ti(palette = "default", .colors = ti_colors, .palettes = ti_palettes)
  expect_s3_class(scale, "ScaleDiscrete")
  expect_true("colour" %in% scale$aesthetics)
  expect_equal(scale$na.value, ti_colors$na_value)
})

test_that("scale_color_ti reverses palettes correctly", {
  scale <- scale_color_ti(palette = "default", reverse = TRUE, .colors = ti_colors, .palettes = ti_palettes)
  expect_equal(scale$scale_name, rev(ti_palettes$default))
})


test_that("scale_color_ti passes additional arguments", {
  scale <- scale_color_ti(palette = "default", continuous = TRUE, .aesthetic = "color", .colors = ti_colors, .palettes = ti_palettes, guide = "none")
  expect_equal(scale$guide, "none")
})

# Test scale_fill_ti function by ensuring it calls scale_color_ti with correct params
test_that("scale_fill_ti returns the correct ggplot2 scale with default parameters", {
  scale <- scale_fill_ti()
  expect_s3_class(scale, "ScaleDiscrete")
  expect_equal(scale$aesthetics, "fill")
})

test_that("scale_fill_ti handles reversing palettes", {
  scale <- scale_fill_ti(palette = "default", reverse = TRUE)
  reversed_palette <- rev(ti_palettes$default)
  # Assuming scale_color_ti is correct, we test if scale_fill_ti passes the correct arguments to scale_color_ti
  expect_equal(scale$scale_name, reversed_palette)
})


test_that("scale_fill_ti handles continuous scales", {
  scale <- scale_fill_ti(palette = "default", continuous = TRUE)
  expect_s3_class(scale, "ScaleContinuous")
})



