
targets <- dat_species_bin %>%
  sf::st_drop_geometry() %>%
  colnames() %>%
  data.frame() %>%
  setNames(c("feature")) %>%
  dplyr::mutate(target = 0.3)

CPA <- splnr_climate_priorityAreaApproach(
  features = dat_species_bin,
  metric = dat_clim,
  targets = targets,
  direction = -1,
  refugiaTarget = 1)

out_sf <- CPA$Features %>%
  dplyr::mutate(Cost_None = rep(1, 780)) %>%
  sf::st_join(dat_clim, join = sf::st_equals)

usedFeatures <- out_sf %>%
  sf::st_drop_geometry() %>%
  dplyr::select(-tidyselect::starts_with("Cost_"), -"metric") %>%
  names()

p1 <- prioritizr::problem(out_sf, usedFeatures, "Cost_None") %>%
  prioritizr::add_min_set_objective() %>%
  prioritizr::add_relative_targets(CPA$Targets$target) %>%
  prioritizr::add_binary_decisions() %>%
  prioritizr::add_default_solver(verbose = FALSE)

dat_solnClim <- prioritizr::solve.ConservationProblem(p1)


testthat::test_that("Correct function output", {
  expect_s3_class(
    splnr_plot_climData(df = dat_clim, colInterest = "metric")
    , "gg")
})


testthat::test_that("Correct function output", {
  expect_s3_class(
    splnr_plot_climKernelDensity(dat_solnClim, type = "Basic")
    , "gg")
})


testthat::test_that("Correct function output", {
  expect_s3_class(
    splnr_plot_climKernelDensity(soln = list(dat_solnClim), type = "Normal")
    , "gg")
})


