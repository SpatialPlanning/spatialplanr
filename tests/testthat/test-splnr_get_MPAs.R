testthat::test_that("Raw = FALSE (default) returns an sf object of planning units", {
  testthat::expect_s3_class(
    splnr_get_MPAs(dat_PUs, "Australia"), "sf"
  )
})

testthat::test_that("Raw = TRUE returns an sf object without requiring PlanUnits", {
  result <- splnr_get_MPAs(Countries = "Australia", Raw = TRUE)
  testthat::expect_s3_class(result, "sf")
})

testthat::test_that("Raw = FALSE errors when PlanUnits is NULL", {
  testthat::expect_error(
    splnr_get_MPAs(PlanUnits = NULL, Countries = "Australia", Raw = FALSE),
    regexp = "PlanUnits.*provided when Raw = FALSE"
  )
})

testthat::test_that("Raw = FALSE errors when PlanUnits is not an sf object", {
  testthat::expect_error(
    splnr_get_MPAs(PlanUnits = data.frame(x = 1), Countries = "Australia", Raw = FALSE),
    regexp = "PlanUnits.*sf"
  )
})

testthat::test_that("Raw must be a single logical value", {
  testthat::expect_error(
    splnr_get_MPAs(PlanUnits = dat_PUs, Countries = "Australia", Raw = "yes"),
    regexp = "Raw.*logical"
  )
  testthat::expect_error(
    splnr_get_MPAs(PlanUnits = dat_PUs, Countries = "Australia", Raw = c(TRUE, FALSE)),
    regexp = "Raw.*logical"
  )
})
