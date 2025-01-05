# This is an attempt to fix notes coming up on R CMD Check about no bindings for
# global variables
# nolint start
# https://community.rstudio.com/t/how-to-solve-no-visible-binding-for-global-variable-note/28887
# nolint end

globalVariables(c(
  "path",
  "family",
  "style",
  "ti_colors",
  "get_from_json"
))
