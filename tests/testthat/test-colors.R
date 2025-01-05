# Test ti_colors hex codes
test_that("ti_colors hex codes are correct", {
  expect_equal(ti_colors$dark_text, "#00101A")
  expect_equal(ti_colors$light_text, "#323F47")
  expect_equal(ti_colors$background, "#F8FBFD")
  expect_equal(ti_colors$dark_teal, "#1a6b85")
  expect_equal(ti_colors$teal, "#5fa8c8")
  expect_equal(ti_colors$mint, "#a3d3c0")
  expect_equal(ti_colors$muted_gold, "#f3d88a")
  expect_equal(ti_colors$orange, "#f7a06a")
  expect_equal(ti_colors$dark_orange, "#de7b56")
  expect_equal(ti_colors$crimson, "#a63a33")
  expect_equal(ti_colors$positive, "#5fa188")
  expect_equal(ti_colors$pos_neg_neutral, "#9caeb9")
  expect_equal(ti_colors$negative, "#6f2a2e")
  expect_equal(ti_colors$republican, "#b02325")
  expect_equal(ti_colors$rep_dem_neutral, "#FEFCF7")
  expect_equal(ti_colors$democrat, "#0d39a0")
  expect_equal(ti_colors$gold, "#F9C55C")
  expect_equal(ti_colors$na_value, "#848B90")
  expect_equal(ti_colors$grid_lines, "#F0F8FA")
  expect_equal(ti_colors$light_green, "#d8eaa0")
})

# Test ti_palettes color sequences
test_that("ti_palettes returns correct color hex codes", {
  expect_equal(
    ti_palettes$default,
    c(
      ti_colors$dark_teal,
      ti_colors$teal,
      ti_colors$mint,
      ti_colors$light_green,
      ti_colors$muted_gold,
      ti_colors$orange,
      ti_colors$dark_orange
    )
  )
  expect_equal(
    ti_palettes$teal_to_gold,
    c(
      ti_colors$dark_teal,
      ti_colors$teal,
      ti_colors$gold
    )
  )
  expect_equal(
    ti_palettes$teal_to_crimson,
    c(
      ti_colors$teal,
      ti_colors$muted_gold,
      ti_colors$orange,
      ti_colors$crimson
    )
  )
  expect_equal(
    ti_palettes$cool_colors,
    c(
      ti_colors$dark_teal,
      ti_colors$teal,
      ti_colors$mint
    )
  )
  expect_equal(
    ti_palettes$warm_colors,
    c(
      ti_colors$muted_gold,
      ti_colors$orange,
      ti_colors$crimson
    )
  )
  expect_equal(
    ti_palettes$skip_gold,
    c(
      ti_colors$dark_teal,
      ti_colors$teal,
      ti_colors$mint,
      ti_colors$orange,
      ti_colors$dark_orange
    )
  )
  expect_equal(
    ti_palettes$neg_to_pos,
    c(ti_colors$negative,
      ti_colors$pos_neg_neutral,
      ti_colors$positive)
  )
  expect_equal(
    ti_palettes$us_rep_to_dem,
    c(ti_colors$republican,
      ti_colors$rep_dem_neutral,
      ti_colors$democrat)
  )
  expect_equal(
    ti_palettes$divergent,
    c(
      ti_colors$dark_teal,
      ti_colors$teal,
      ti_colors$mint,
      ti_colors$muted_gold,
      ti_colors$dark_orange,
      ti_colors$crimson
    )
  )
})


# Test scale_color_ti function
test_that("scale_color_ti returns the correct discrete scale", {
  scale <- scale_color_ti(
    palette = "default",
    .colors = ti_colors,
    .palettes = ti_palettes
  )
  expect_s3_class(scale, "ScaleDiscrete")
  expect_true("colour" %in% scale$aesthetics)
  expect_equal(scale$na.value, ti_colors$na_value)
})

test_that("scale_color_ti reverses palettes correctly", {
  # Create scales with default and reversed palettes
  scale_default <- scale_color_ti(
    palette = "default",
    reverse = FALSE,
    .colors = ti_colors,
    .palettes = ti_palettes
  )

  scale_reversed <- scale_color_ti(
    palette = "default",
    reverse = TRUE,
    .colors = ti_colors,
    .palettes = ti_palettes
  )

  # Get colors from both scales for comparison
  n_colors <- 4  # Number of colors in default palette
  default_colors <- scale_default$palette(n_colors)
  reversed_colors <- scale_reversed$palette(n_colors)

  # Check if reversed colors match the expected order
  expect_equal(reversed_colors, rev(default_colors))
})


test_that("scale_color_ti passes additional arguments", {
  scale <- scale_color_ti(
    palette = "default",
    continuous = TRUE,
    .aesthetic = "color",
    .colors = ti_colors,
    .palettes = ti_palettes,
    guide = "none"
  )
  expect_equal(scale$guide, "none")
})

# Test scale_fill_ti function calls scale_color_ti with correct params
test_that("scale_fill_ti returns correct ggplot2 scale w/ default parameters", {
  scale <- scale_fill_ti()
  expect_s3_class(scale, "ScaleDiscrete")
  expect_equal(scale$aesthetics, "fill")
})

test_that("scale_fill_ti handles reversing palettes", {
  # Create scales with default and reversed palettes
  scale_default <- scale_fill_ti(
    palette = "default",
    reverse = FALSE
  )

  scale_reversed <- scale_fill_ti(
    palette = "default",
    reverse = TRUE
  )

  # Get colors from both scales for comparison
  n_colors <- 4  # Number of colors in default palette
  default_colors <- scale_default$palette(n_colors)
  reversed_colors <- scale_reversed$palette(n_colors)

  # Check if reversed colors match the expected order
  expect_equal(reversed_colors, rev(default_colors))
})


test_that("scale_fill_ti handles continuous scales", {
  scale <- scale_fill_ti(palette = "default", continuous = TRUE)
  expect_s3_class(scale, "ScaleContinuous")
})
