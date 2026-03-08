# 30 % target
pDat1 <- prioritizr::problem(dat_species_bin %>% dplyr::mutate(Cost = runif(n = dim(.)[[1]])),
                             features = c("Spp1", "Spp2", "Spp3", "Spp4", "Spp5"),
                             cost_column = "Cost"
) %>%
  prioritizr::add_min_set_objective() %>%
  prioritizr::add_relative_targets(0.3) %>%
  prioritizr::add_binary_decisions() %>%
  prioritizr::add_default_solver(verbose = FALSE)

soln1 <- pDat1 %>%
  prioritizr::solve.ConservationProblem()

# 50 % target
soln2 <- prioritizr::problem(dat_species_bin %>% dplyr::mutate(Cost = runif(n = dim(.)[[1]])),
                             features = c("Spp1", "Spp2", "Spp3", "Spp4", "Spp5"),
                             cost_column = "Cost"
) %>%
  prioritizr::add_min_set_objective() %>%
  prioritizr::add_relative_targets(0.5) %>%
  prioritizr::add_binary_decisions() %>%
  prioritizr::add_default_solver(verbose = FALSE) %>%
  prioritizr::solve.ConservationProblem()


# Zones

t2 <- matrix(NA, ncol = 2, nrow = 5) # create targets
t2[, 1] <- 0.1
t2[, 2] <- 0.05

z2 <- prioritizr::zones(
  "zone 1" = c("Spp1", "Spp2", "Spp3", "Spp4", "Spp5"),
  "zone 2" = c("Spp1", "Spp2", "Spp3", "Spp4", "Spp5")
)
# when giving sf input, we need as many cost columns as we have zones
soln_zone <- prioritizr::problem(
  dat_species_bin %>% dplyr::mutate(
    Cost1 = runif(n = dim(.)[[1]]),
    Cost2 = runif(n = dim(.)[[1]])
  ),
  z2,
  cost_column = c("Cost1", "Cost2")
) %>%
  prioritizr::add_min_set_objective() %>%
  prioritizr::add_relative_targets(t2) %>%
  prioritizr::add_binary_decisions() %>%
  prioritizr::add_default_solver(verbose = FALSE) %>%
  prioritizr::solve.ConservationProblem()


# Portfolio
soln_portfolio <- prioritizr::problem(dat_species_bin %>% dplyr::mutate(Cost = runif(n = dim(.)[[1]])),
                                      features = c("Spp1", "Spp2", "Spp3", "Spp4", "Spp5"),
                                      cost_column = "Cost") %>%
  prioritizr::add_min_set_objective() %>%
  prioritizr::add_relative_targets(0.3) %>%
  prioritizr::add_binary_decisions() %>%
  prioritizr::add_default_solver(verbose = FALSE) %>%
  prioritizr::add_cuts_portfolio(number_solutions = 5) %>%
  prioritizr::solve.ConservationProblem()



testthat::test_that("Correct function output", {
  expect_s3_class(
    splnr_plot_solution(soln1) +
      splnr_gg_add(PUs = dat_PUs, ggtheme = "Default")
    , "gg"
  )
})




testthat::test_that("Correct function output", {
  expect_s3_class(
    splnr_plot_solution(soln_zone,
                        zones = TRUE, colorVals = c("#c6dbef", "#3182bd", "black"),
                        legendLabels = c("Not selected", "Zone 1", "Zone 2"))
    , "gg"
  )
})




testthat::test_that("Correct function output", {
  expect_s3_class(
    splnr_plot_costOverlay(soln = soln1)
    , "gg"
  )
})



testthat::test_that("Correct function output", {
  expect_s3_class(
    splnr_plot_comparison(soln1, soln2)
    , "gg"
  )
})



testthat::test_that("Correct function output", {
  expect_s3_class(
    splnr_plot_selectionFreq(splnr_get_selFreq(solnMany = soln_portfolio, type = "portfolio"))
    , "gg"
  )
})



testthat::test_that("Correct function output", {
  expect_s3_class(
    splnr_plot_importanceScore(soln = soln1, pDat = pDat1, method = "Ferrier", decimals = 4)
    , "gg"
  )
})


testthat::test_that("Correct function output", {
  expect_s3_class(
    splnr_plot_importanceScore(soln = soln1, pDat = pDat1, method = "RWR", decimals = 4)
    , "gg"
  )
})

testthat::test_that("Correct function output", {
  expect_s3_class(
    splnr_plot_importanceScore(soln = soln1, pDat = pDat1, method = "RC", decimals = 4)
    , "gg"
  )
})

testthat::test_that("Correct function output", {
  # Suppress deprecation warning from ggcorrplot using aes_string()
  expect_s3_class(
    suppressWarnings(
      splnr_plot_corrMat(splnr_get_kappaCorrData(list(soln1, soln2), name_sol = c("soln1", "soln2")),
                         AxisLabels = c("Solution 1", "Solution 2")),
      classes = "lifecycle_warning_deprecated"
    )
    , "gg"
  )
})


