testthat::test_that("Correct function output", {
  expect_s3_class(
    splnr_get_boundary("North Atlantic Ocean", "Ocean"), "sf"
  )
})


testthat::test_that("Correct function output", {
  expect_s3_class(
    splnr_get_boundary(Limits = c("xmin" = 150, "xmax" = 170, "ymin" = -40, "ymax" = -20)), "sf"
  )
})

testthat::test_that("Correct function output", {
  expect_s3_class(
    splnr_get_boundary(Limits = "Global"), "sf"
  )
})
