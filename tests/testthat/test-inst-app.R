# File: tests/testthat/test-inst-apps.R
library(shinytest2)

test_that("shiny app works", {
  # Don't run these tests on the CRAN build servers
  skip_on_cran()

  appdir <- system.file(package = "neotomalakes", "app")
  shinytest2::test_app(appdir)
})
