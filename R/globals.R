# This is an attempt to fix notes coming up on R CMD Check about no bindings for global variables
# https://community.rstudio.com/t/how-to-solve-no-visible-binding-for-global-variable-note/28887


utils::globalVariables(c(
  "path",
  "family",
  "style"
))
