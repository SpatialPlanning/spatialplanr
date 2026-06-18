pDat <- prioritizr::problem(dat_species_bin %>% dplyr::mutate(Cost = runif(n = dim(.)[[1]])),
                            features = c("Spp1", "Spp2", "Spp3"),
                            cost_column = "Cost"
) %>%
  prioritizr::add_min_set_objective() %>%
  prioritizr::add_relative_targets(0.3) %>%
  prioritizr::add_binary_decisions() %>%
  prioritizr::add_default_solver(verbose = FALSE)

soln <- pDat %>%
  prioritizr::solve.ConservationProblem()


testthat::test_that("splnr_get_featureRep() returns a tibble for basic use", {
  expect_s3_class(
    splnr_get_featureRep(
      soln = soln,
      pDat = pDat
    ), "tbl_df"
  )
})


testthat::test_that("splnr_get_featureRep() returns correct columns", {
  df <- splnr_get_featureRep(soln = soln, pDat = pDat)
  expect_true(all(c("feature", "total_amount", "absolute_held",
                    "relative_held", "target", "incidental") %in% names(df)))
})


testthat::test_that("splnr_get_featureRep() returns one row per problem feature", {
  df <- splnr_get_featureRep(soln = soln, pDat = pDat)
  expect_equal(nrow(df), 3L)
  expect_setequal(df$feature, c("Spp1", "Spp2", "Spp3"))
})


testthat::test_that("splnr_get_featureRep() incidental_features adds extra rows", {
  # soln contains Spp4 and Spp5 because dat_species_bin has them, but they
  # were not declared as features in pDat.
  df <- splnr_get_featureRep(
    soln = soln,
    pDat = pDat,
    incidental_features = c("Spp4", "Spp5")
  )
  expect_equal(nrow(df), 5L)
  expect_true(all(c("Spp4", "Spp5") %in% df$feature))
})


testthat::test_that("splnr_get_featureRep() incidental features have target = 0 and incidental = TRUE", {
  df <- splnr_get_featureRep(
    soln = soln,
    pDat = pDat,
    incidental_features = c("Spp4", "Spp5")
  )
  incidental_rows <- df[df$feature %in% c("Spp4", "Spp5"), ]
  expect_true(all(incidental_rows$target == 0))
  expect_true(all(incidental_rows$incidental == TRUE))
})


testthat::test_that("splnr_get_featureRep() errors when incidental_features column missing from soln", {
  expect_error(
    splnr_get_featureRep(
      soln = soln,
      pDat = pDat,
      incidental_features = c("NonExistentColumn")
    ),
    regexp = "not present in 'soln'"
  )
})


testthat::test_that("splnr_get_featureRep() errors when incidental_features overlap with problem features", {
  expect_error(
    splnr_get_featureRep(
      soln = soln,
      pDat = pDat,
      incidental_features = c("Spp1")  # Spp1 is already in pDat
    ),
    regexp = "already features in 'pDat'"
  )
})


testthat::test_that("splnr_plot_featureRep() returns a ggplot for basic use", {
  expect_s3_class(
    splnr_plot_featureRep(splnr_get_featureRep(
      soln = soln,
      pDat = pDat), category = dat_category)
    , "gg"
  )
})




testthat::test_that("Correct function output", {
  s1 <- soln %>%
    tibble::as_tibble()

  df_rep_imp <- prioritizr::eval_feature_representation_summary(
    pDat,
    s1[, "solution_1"]
  ) %>%
    dplyr::select(feature, relative_held) %>%
    dplyr::mutate(relative_held = relative_held * 100)

  imp_layers <- c("Spp1", "Spp3")

  target <- data.frame(feature = c("Spp1", "Spp2", "Spp3", "Spp4", "Spp5")) %>%
    dplyr::mutate(class = dplyr::if_else(.data$feature %in% imp_layers,
                                         "important", "representative"
    )) %>%
    dplyr::mutate(target = dplyr::if_else(class == "important",
                                          50 / 100, 30 / 100
    ))

  df <- merge(df_rep_imp, target) %>%
    dplyr::select(-target) %>%
    na.omit() %>%
    dplyr::rename(value = relative_held) %>%
    dplyr::rename(group = class)

  colors <- c(
    "important" = "darkgreen",
    "representative" = "darkred"
  )
  legends <- c("Important", "Representative")

  expect_s3_class(
    (splnr_plot_circBplot(df,
                          legend_list = legends,
                          legend_color = colors,
                          impTarget = 50, repTarget = 30))
    , "gg"
  )
})

