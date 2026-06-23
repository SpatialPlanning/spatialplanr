testthat::test_that("Correct function output", {
  expect_s3_class(
    dat_species_prob %>%
      splnr_targets_byInverseArea(target_min = 0.3, target_max = 0.8), "tbl_df"
  )
})


testthat::test_that("Correct function output", {
  expect_s3_class(
    splnr_targets_byCategory(
      dat = dat_category,
      catTarg = c("Group1" = 0.5, "Group2" = 0.2),
      catName = "category"
    ), "tbl_df"
  )
})


dat <- data.frame(IUCN_Category = c("EW", "EX", NA), target = c(0.3, 0.3, 0.3))
IUCN_target <- c("EX" = 0.8, "EW" = 0.6)

testthat::test_that("Correct function output", {
  expect_s3_class(
    splnr_targets_byIUCN(dat, IUCN_target), "data.frame"
  )
})


# testthat::test_that("Correct function output", {
#   expect_s3_class(
#     , "sf"
#   )
# })
