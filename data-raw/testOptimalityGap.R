## Test time of # of features vs gap penalty vs PU number

gap_range <- c(0.01, 0.02, 0.03, 0.04, 0.05, 0.1, 0.2, 0.3, 0.5, 0.8)
feature_num <- c(50, 100, 500, 1000, 5000, 10000) # , 50000)#, 100000)

mat1 <- matrix(, nrow = length(feature_num) * length(gap_range), ncol = 3)
counter <- 1

for (j in 1:length(feature_num)) {
  for (i in 1:length(gap_range)) {
    print(counter)

    spp_names <- purrr::map_chr(1:feature_num[j], ~ paste0("spp", .))

    dat_species <- dat_PUs %>%
      dplyr::bind_cols(setNames(
        purrr::map(1:feature_num[j], ~ rbinom(nrow(dat_PUs), 1, 0.5)),
        spp_names
      )) %>%
      sf::st_sf()

    dat_problem <- prioritizr::problem(dat_species %>% dplyr::mutate(Cost = runif(n = dim(.)[[1]])),
      features = spp_names,
      cost_column = "Cost"
    ) %>%
      prioritizr::add_min_set_objective() %>%
      prioritizr::add_relative_targets(0.3) %>%
      prioritizr::add_binary_decisions() %>%
      prioritizr::add_default_solver(gap = gap_range[i], verbose = FALSE)

    dat_soln <- dat_problem %>%
      prioritizr::solve.ConservationProblem()

    runTime <- attr(dat_soln, "runtime")[[1]]

    mat1[counter, 1] <- gap_range[i]
    mat1[counter, 2] <- feature_num[j]
    mat1[counter, 3] <- runTime

    counter <- counter + 1
  }
}

feature_gap <- data.frame(mat1)
colnames(feature_gap) <- c("Gap", "FeatureNumber", "Runtime")

saveRDS(feature_gap, "feature_gap.rds")

# ggplot2::ggplot(feature_gap, ggplot2::aes(Gap, FeatureNumber, fill= Runtime)) +
#   ggplot2::geom_tile()


Bndry <- dplyr::tibble(x = 60, y = seq(-60, 0, by = 1)) %>%
  dplyr::bind_rows(dplyr::tibble(x = seq(60, 120, by = 1), y = 0)) %>%
  dplyr::bind_rows(dplyr::tibble(x = 120, y = seq(0, -60, by = -1))) %>%
  dplyr::bind_rows(dplyr::tibble(x = seq(120, 60, by = -1), y = -60)) %>%
  as.matrix() %>%
  list() %>%
  sf::st_polygon() %>%
  sf::st_sfc(crs = "EPSG:4326") %>%
  sf::st_sf() %>%
  sf::st_make_valid()

# Use boundary to create grid
PUs <- sf::st_make_grid(Bndry, cellsize = 0.125) %>%
  sf::st_sf()

feature_num <- c(25, 50, 100, 500) # , 1000)#, 5000, 10000)#, 50000, 100000)
PU_cellsize <- c(2, 1, 0.5, 0.25, 0.125)

mat2 <- matrix(, nrow = length(feature_num) * length(PU_cellsize), ncol = 3)
counter <- 1

for (k in 1:length(PU_cellsize)) {
  PUs <- sf::st_make_grid(Bndry, cellsize = PU_cellsize[k]) %>%
    sf::st_sf()

  for (j in 1:length(feature_num)) {
    print(counter)

    spp_names <- purrr::map_chr(1:feature_num[j], ~ paste0("spp", .))

    dat_species <- PUs %>%
      dplyr::bind_cols(setNames(
        purrr::map(1:feature_num[j], ~ rbinom(nrow(PUs), 1, 0.5)),
        spp_names
      )) %>%
      sf::st_sf()

    dat_problem <- prioritizr::problem(dat_species %>% dplyr::mutate(Cost = runif(n = dim(.)[[1]])),
      features = spp_names,
      cost_column = "Cost"
    ) %>%
      prioritizr::add_min_set_objective() %>%
      prioritizr::add_relative_targets(0.3) %>%
      prioritizr::add_binary_decisions() %>%
      prioritizr::add_default_solver(gap = 0.1, verbose = FALSE)

    dat_soln <- dat_problem %>%
      prioritizr::solve.ConservationProblem()

    runTime <- attr(dat_soln, "runtime")[[1]]

    mat2[counter, 1] <- PU_cellsize[k]
    mat2[counter, 2] <- feature_num[j]
    mat2[counter, 3] <- runTime

    counter <- counter + 1
  }
}

feature_PUs <- data.frame(mat2)
colnames(feature_PUs) <- c("PUNumber", "FeatureNumber", "Runtime")

saveRDS(feature_PUs, "feature_PUs.rds")

# features, PUs and optimalty gap
gap_range <- c(0.01, 0.03, 0.05, 0.1, 0.2, 0.3, 0.5, 0.8)
feature_num <- c(25, 50) # , 100, 500)

PU_cellsize <- c(2, 1, 0.5, 0.25, 0.125)

mat3 <- matrix(, nrow = length(gap_range) * length(PU_cellsize) * length(feature_num), ncol = 4)
counter <- 1

for (k in 1:length(PU_cellsize)) {
  PUs <- sf::st_make_grid(Bndry, cellsize = PU_cellsize[k]) %>%
    sf::st_sf()

  for (j in 1:length(feature_num)) {
    for (i in 1:length(gap_range)) {
      print(counter)

      spp_names <- purrr::map_chr(1:feature_num[j], ~ paste0("spp", .))

      dat_species <- PUs %>%
        dplyr::bind_cols(setNames(
          purrr::map(1:feature_num[j], ~ rbinom(nrow(PUs), 1, 0.5)),
          spp_names
        )) %>%
        sf::st_sf()

      dat_problem <- prioritizr::problem(dat_species %>% dplyr::mutate(Cost = runif(n = dim(.)[[1]])),
        features = spp_names,
        cost_column = "Cost"
      ) %>%
        prioritizr::add_min_set_objective() %>%
        prioritizr::add_relative_targets(0.3) %>%
        prioritizr::add_binary_decisions() %>%
        prioritizr::add_default_solver(gap = gap_range[i], verbose = FALSE)

      dat_soln <- dat_problem %>%
        prioritizr::solve.ConservationProblem()

      runTime <- attr(dat_soln, "runtime")[[1]]

      mat3[counter, 1] <- nrow(PUs)
      mat3[counter, 2] <- feature_num[j]
      mat3[counter, 3] <- gap_range[i]
      mat3[counter, 4] <- runTime

      counter <- counter + 1
    }
  }
}

feature_PUs_gap <- data.frame(mat3)
colnames(feature_PUs_gap) <- c("PUNumber", "FeatureNumber", "OptimalityGap", "Runtime")


saveRDS(feature_PUs_gap, "feature_PUs_gap.rds")

gc()
gc()

# penalty and optimalty gap
gap_range <- c(0.1, 0.2, 0.3, 0.5, 0.8)
feature_num <- c(25, 50)
PU_cellsize <- c(2, 1, 0.5) # ,0.25)#,0.125)
penalty_range <- c(1) # ,2, 0.5, 0.1)

mat4 <- matrix(,
  nrow = length(gap_range) * length(PU_cellsize) * length(feature_num) * length(penalty_range),
  ncol = 5
)
counter <- 1

for (k in 1:length(PU_cellsize)) {
  PUs <- sf::st_make_grid(Bndry, cellsize = PU_cellsize[k]) %>%
    sf::st_sf()

  for (j in 1:length(feature_num)) {
    for (i in 1:length(gap_range)) {
      spp_names <- purrr::map_chr(1:feature_num[j], ~ paste0("spp", .))

      dat_species <- PUs %>%
        dplyr::bind_cols(setNames(
          purrr::map(1:feature_num[j], ~ rbinom(nrow(PUs), 1, 0.5)),
          spp_names
        )) %>%
        sf::st_sf()

      for (l in 1:length(penalty_range)) {
        print(counter)

        dat_problem <- prioritizr::problem(dat_species %>% dplyr::mutate(Cost = runif(n = dim(.)[[1]])),
          features = spp_names,
          cost_column = "Cost"
        ) %>%
          prioritizr::add_min_set_objective() %>%
          prioritizr::add_relative_targets(0.3) %>%
          prioritizr::add_boundary_penalties(penalty_range[l]) %>%
          prioritizr::add_binary_decisions() %>%
          prioritizr::add_default_solver(gap = gap_range[i], verbose = FALSE)

        dat_soln <- dat_problem %>%
          prioritizr::solve.ConservationProblem()

        runTime <- attr(dat_soln, "runtime")[[1]]

        mat4[counter, 1] <- nrow(PUs)
        mat4[counter, 2] <- feature_num[j]
        mat4[counter, 3] <- gap_range[i]
        mat4[counter, 4] <- penalty_range[l]
        mat4[counter, 5] <- runTime

        print("here")

        saveRDS(mat4, "mat4.rds")

        counter <- counter + 1
      }
    }
  }
}

feat_PUs_gap_penalty <- data.frame(mat4)
colnames(feat_PUs_gap_penalty) <- c(
  "PUNumber", "FeatureNumber",
  "OptimalityGap", "BoundaryPenalty", "Runtime"
)

saveRDS(feat_PUs_gap_penalty, "feat_PUs_gap_penalty.rds")


# hexagonal vs square PUs in large problems

# zones vs no zones

# objective functions differences

# adding constraints differences

# raster vs shape file
