## code to prepare `DATASET` dataset goes here
# options("sp_evolution_status"=0)

library(magrittr)
suppressPackageStartupMessages(library(prioritizr))

set.seed(1222)

# I need a dataset I can use in the examples to demonstrate
# Create a dummy dataset for the moment. Could be better to use a real
# dataset from AquaMaps for the east coast of Australia.

# Create a dataset boundary
dat_bndry <- dplyr::tibble(x = 100, y = seq(-50, 0, by = 1)) %>%
  dplyr::bind_rows(dplyr::tibble(x = seq(100, 160, by = 1), y = 0)) %>%
  dplyr::bind_rows(dplyr::tibble(x = 160, y = seq(0, -50, by = -1))) %>%
  dplyr::bind_rows(dplyr::tibble(x = seq(160, 100, by = -1), y = -50)) %>%
  as.matrix() %>%
  list() %>%
  sf::st_polygon() %>%
  sf::st_sfc(crs = "EPSG:4326") %>%
  sf::st_sf() %>%
  sf::st_make_valid()

# Use boundary to create grid
dat_PUs <- sf::st_make_grid(dat_bndry, cellsize = 2) %>%
  sf::st_sf()

# Create some regionalisations
dat_region <- dat_PUs %>%
  sf::st_sf() %>%
  dplyr::mutate(Region = "Region1")
dat_region$Region[30:65] <- "Region2"
dat_region$Region[66:80] <- "Region3"


# Add some species probabilities
dat_species_prob <- dat_PUs %>%
  sf::st_sf() %>%
  dplyr::mutate(
    Spp1 = runif(n = dim(.)[[1]]),
    Spp2 = runif(n = dim(.)[[1]]),
    Spp3 = runif(n = dim(.)[[1]]),
    Spp4 = runif(n = dim(.)[[1]]),
    Spp5 = runif(n = dim(.)[[1]])
  )


# Convert the probabilities to binary data
col_name <- dat_species_prob %>%
  sf::st_drop_geometry() %>%
  colnames()

dat_species_bin <- dat_species_prob %>%
  dplyr::mutate(dplyr::across(
    -dplyr::any_of(c("geometry")), # Apply to all columns except geometry and cellID
    ~ dplyr::case_when(
      . >= 0.5 ~ 1,
      . < 0.5 ~ 0,
      is.na(.data) ~ 0
    )
  ))

# create second species_bin data set with unused species
dat_species_bin2 <- dat_species_bin %>%
  dplyr::mutate(Spp6 = 1)

# Add MPAs
# Add a random MPA
dat_mpas <- dat_PUs %>%
  sf::st_sf() %>%
  tibble::rowid_to_column("cellID") %>% # Add a cell ID reference
  dplyr::mutate(wdpa = ifelse((cellID > 33 & cellID < 38) |
    (cellID > 63 & cellID < 68) |
    (cellID > 93 & cellID < 98), 1, 0)) %>%
  dplyr::select(-"cellID")


# Add a problem object
dat_problem <- problem(dat_species_bin %>% dplyr::mutate(Cost = runif(n = dim(.)[[1]])),
  features = c("Spp1", "Spp2", "Spp3", "Spp4", "Spp5"),
  cost_column = "Cost"
) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.3) %>%
  add_binary_decisions() %>%
  add_default_solver(verbose = FALSE)

# Add a solution object
dat_soln <- dat_problem %>%
  solve.ConservationProblem()


# Add a 2nd solution object
dat_soln2 <- problem(dat_species_bin %>% dplyr::mutate(Cost = runif(n = dim(.)[[1]])),
  features = c("Spp1", "Spp2", "Spp3", "Spp4", "Spp5"),
  cost_column = "Cost"
) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.5) %>%
  add_binary_decisions() %>%
  add_default_solver(verbose = FALSE) %>%
  solve.ConservationProblem()

# Add a category tibble for the features
dat_category <- tibble::tibble(
  feature = c("Spp1", "Spp2", "Spp3", "Spp4", "Spp5"),
  category = c("Group1", "Group1", "Group1", "Group2", "Group2")
)

# Add a second category tibble for the features
dat_category2 <- tibble::tibble(
  feature = c("Spp1", "Spp2", "Spp3", "Spp4", "Spp5", "Spp6"),
  category = c("Group1", "Group1", "Group1", "Group2", "Group2", "Group3")
)

# create random climate metric example
climVec <- unlist(rep(seq(0, 1, length.out = 30), 26))

dat_clim <- dat_PUs %>%
  sf::st_sf() %>%
  dplyr::mutate(metric = climVec)

dat_bathy <- oceandatr::get_bathymetry(
  spatial_grid = dat_PUs,
  keep = FALSE,
  classify_bathymetry = FALSE
)


# The three datasets below are loaded from pre-existing .rda files rather than
# being built reproducibly in this script. Their original sources are:
#
#   spDataFiltered  — Species distribution data for the Coral Sea, derived from
#                     GBIF occurrence records filtered to the Coral Sea planning
#                     region. The raw GBIF download is not automated here because
#                     it requires a registered GBIF account and the download job
#                     can take hours. To reproduce: register at gbif.org, submit
#                     a download for the Coral Sea bounding box, and process with
#                     the workflow in data-raw/CreateHex.R.
#
#   MPAsCoralSea    — Marine Protected Areas for the Coral Sea, sourced from the
#                     WDPA (World Database on Protected Areas) via wdpar. The
#                     snapshot date of the underlying WDPA download is unknown;
#                     to refresh, run splnr_get_MPAs() with the Coral Sea PUs.
#
#   CoralSeaVelocity — Climate velocity data for the Coral Sea planning region.
#                      Source dataset and processing steps are not documented.
#                      To reproduce, contact the package maintainer.

load("data/spDataFiltered.rda")
spDataFiltered <- spDataFiltered %>%
  dplyr::select(-"cellID")

load("data/MPAsCoralSea.rda")
MPAsCoralSea <- MPAsCoralSea %>%
  dplyr::select(-"cellID")

load("data/CoralSeaVelocity.rda")
CoralSeaVelocity <- CoralSeaVelocity %>%
  dplyr::select(-"cellID")


# Save the data
usethis::use_data(dat_bndry,
  dat_PUs,
  dat_region,
  dat_species_prob,
  dat_species_bin,
  dat_species_bin2,
  dat_mpas,
  # dat_problem,
  # dat_soln,
  # dat_soln2,
  dat_category,
  dat_category2,
  dat_clim,
  dat_bathy,
  spDataFiltered,
  MPAsCoralSea,
  CoralSeaVelocity,
  overwrite = TRUE
)
