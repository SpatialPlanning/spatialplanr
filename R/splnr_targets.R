#' Assign targets by Inverse Area
#'
#' This function takes a min (`target_min`) and max (`target_max`) target range and calculates an inverse area target for each feature based on areal coverage.
#'
#' @param df A dataframe for the targets. Must contain the feature names (in order), and the targets will be added as an additional column.
#' @param dat An sf or SpatRaster object containing the features.
#' @param target_min The minimum target for inverse area (numeric between 0 and 1)
#' @param target_max The maximum target for inverse area (numeric between 0 and 1)
#' 
#' @return df A dataframe with added column `target_area` with the assigned targets
#' @export
#'
#' @importFrom rlang .data
#' @examples
#' # sf example
#' library(sf)
#' library(dplyr)
#' # Create 25 grid polygons (5x5 grid)
#' grid <- expand.grid(i = 0:4, j = 0:4)
#' polys <- mapply(function(i, j) st_polygon(list(matrix(c(i, j, i+1, j, i+1, j+1, i, j+1, i, j), ncol=2, byrow=TRUE))),
#'                 grid$i, grid$j, SIMPLIFY = FALSE)
#' # Create binary values for each cell for two species
#' # species1: only cell 1 (top-left)
#' species1 <- rep(0, 25)
#' species1[1] <- 1
#' # species2: cells 6-25 (all except first row)
#' species2 <- rep(0, 25)
#' species2[6:25] <- 1
#' df <- data.frame(species = c("species1", "species1"))
#' dat <- st_sf(species1 = species1, species2 = species2, geometry = st_sfc(polys))
#' plot(dat)
#' targets <- splnr_targets_byInverseArea(df = df, dat = dat, target_min = 0.3, target_max = 0.8)
#'
#' # SpatRaster example
#' library(terra)
#' # Create a 5x5 raster
#' r <- rast(nrows=5, ncols=5, xmin=0, xmax=5, ymin=0, ymax=5, crs = "")
#' # species1: occupies a 3x3 block (area = 1 cell)
#' m1 <- matrix(0, 5, 5); m1[1, 1] <- 1
#' # species2: occupies a 2x5 block (area = 20 cells)
#' m2 <- matrix(0, 5, 5); m2[1:5, 1:4] <- 1
#' r1 <- setValues(r, as.vector(m1))
#' r2 <- setValues(r, as.vector(m2))
#' names(r1) <- "species1"
#' names(r2) <- "species2"
#' dat <- c(r1, r2)
#' plot(dat)
#' df <- data.frame(species = c("spp1", "spp1"))
#' targets <- splnr_targets_byInverseArea(df = df, dat = dat, target_min = 0.3, target_max = 0.8)
#'
splnr_targets_byInverseArea <- function(df, dat, target_min, target_max) {
  # Common argument checks
  assertthat::assert_that(
    is.data.frame(df),
    is.numeric(target_min) && target_min >= 0 && target_min <= 1,
    is.numeric(target_max) && target_max >= 0 && target_max <= 1,
    target_min <= target_max
  )
  # Check if input is sf
  if (inherits(dat, c("sf", "data.frame"))) {
    PU_area_km2 <- as.numeric(sf::st_area(dat[1, 1]) / 1e+06) # Area of each planning unit
    total_PU_area <- nrow(dat) * PU_area_km2 # Total area of the study region
    df <- dat %>%
      sf::st_drop_geometry() %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ tidyr::replace_na(.x, 0))) %>%
      dplyr::summarise(dplyr::across(dplyr::everything(), ~ sum(., is.na(.), 0))) %>%
      tidyr::pivot_longer(dplyr::everything(), names_to = "species", values_to = "area_km2") %>%
      dplyr::mutate(
        species = stringr::str_replace_all(.data$species, pattern = "_", replacement = " "),
        area_km2 = .data$area_km2 * PU_area_km2,
        target_area = target_max - ((.data$area_km2 / total_PU_area) * (target_max - target_min))
      )
    return(df)
  } else if (inherits(dat, "SpatRaster")) {
    assertthat::assert_that(
      nrow(df) == terra::nlyr(dat)
    )
    PU_area_km2 <- terra::cellSize(dat, unit = "km")[1] # Area of each planning unit
    nlyr <- terra::nlyr(dat)
    selected_area <- numeric(nlyr)
    target_area <- numeric(nlyr)
    for (i in seq_len(nlyr)) {
      vals <- terra::values(dat[[i]])
      selected_area[i] <- as.numeric(sum(!is.na(vals) & vals != 0, na.rm = TRUE) * PU_area_km2) # Ensure numeric
      total_area <- as.numeric(sum(!is.na(vals), na.rm = TRUE) * PU_area_km2) # Ensure numeric
      target_area[i] <- as.numeric(target_max - ((selected_area[i] / total_area) * (target_max - target_min))) # Ensure numeric
    }
    df <- df %>% 
      dplyr::mutate(area_km2 = as.numeric(selected_area), 
                    target_area = as.numeric(target_area))
    return(df)
  } else {
    stop("Input must be an sf/data.frame or a SpatRaster object.")
  }
}


#' Assign targets to all features by category
#'
#' `splnr_targets_byCategory()` allows to assign targets for conservation planning based on species categories.
#'
#' @param dat A sf object with the features and categories
#' @param catTarg A named character vector with categories and target
#' @param catName An optional argument for the name of the category column in dat
#'
#' @return An sf object with targets added
#' @export
#'
#' @examples
#' dat <- splnr_targets_byCategory(
#'   dat = dat_category,
#'   catTarg = c("Group1" = 0.5, "Group2" = 0.2),
#'   catName = "category"
#' )
splnr_targets_byCategory <- function(dat, catTarg, catName = "category") {

  assertthat::assert_that(
    inherits(dat, c("sf", "data.frame")),
    is.character(catName),
    catName %in% names(dat),
    is.vector(catTarg),
    length(catTarg) > 0,
    all(names(catTarg) %in% unique(dat[[catName]]))
  )

  dat <- dat %>%
    dplyr::left_join(
      tibble::enframe(catTarg),
      by = dplyr::join_by(!!catName == "name")
    ) %>%
    dplyr::rename(target = "value")

  return(dat)
}


#' Assign targets by IUCN Red List categories
#'
#' `splnr_targets_byIUCN()` assigns targets for species used in conservation planning based on IUCN categories. Species IUCN categories can be extracted based using the `spatialplnr`function `splnr_get_IUCNRedList()`.
#' Accessing the IUCN database requires a login token from `rl_use_iucn()` that needs to be added to the environment using `Sys.setenv(IUCN_REDLIST_KEY = "[Your Token]")`. You can start by running `rredlist::rl_use_iucn()`.
#'
#' @param df A dataframe for the targets that contains IUCN categories for each species. Must contain the feature names (in order), and the targets will be added as an additional column.
#' @param IUCN_cat_targets Either a numeric or named numeric of targets to apply to IUCN categories
#' @param IUCN_col Optional string to indicate the name of the column with the IUCN categories
#'
#' @return dat A dataframe with added column `target_IUCN` with the assigned targets
#' @export
#'
#' @examples
#' df <- data.frame(IUCN_Category = c("EW", "EX", NA), target = c(0.3, 0.3, 0.3))
#' IUCN_cat_targets <- c("EX" = 0.8, "EW" = 0.6)
#' df <- splnr_targets_byIUCN(df, IUCN_cat_targets = IUCN_cat_targets, IUCN_col = "IUCN_category")
splnr_targets_byIUCN <- function(df, IUCN_cat_targets, IUCN_col = "IUCN_category") {
  assertthat::assert_that(
    is.na(IUCN_col) || is.character(IUCN_col),
    IUCN_col %in% names(df),
    (is.numeric(IUCN_cat_targets) && length(IUCN_cat_targets) == 1) || is.vector(IUCN_cat_targets)
  )
  if (is.vector(IUCN_cat_targets, mode = "numeric") & !is.null(names(IUCN_cat_targets))) {
    # If the target is a named vector, apply the relevant targets
    df <- df %>%
      dplyr::left_join(data.frame(IUCN_cat_targets, col1 = names(IUCN_cat_targets)), by = dplyr::join_by(!!rlang::sym(IUCN_col) == "col1")) %>%
      dplyr::mutate(target_IUCN = dplyr::coalesce(IUCN_cat_targets, .data$target_IUCN)) %>%
      dplyr::select(-IUCN_cat_targets)

  } else if (is.numeric(IUCN_cat_targets) & length(IUCN_cat_targets) == 1) {
    # If the target is a single numeric, apply to all IUCN categories.
    df <- df %>%
      dplyr::mutate(target_IUCN = dplyr::case_when(
        !!rlang::sym(IUCN_col) %in% c("EX", "EW", "CR", "EN", "VU") ~ IUCN_cat_targets,
        TRUE ~ df$target_IUCN
      ))
  }
  return(df)
}


#' Assign targets by IUCN Red List categories (simple version)
#'
#' This function assigns targets for species used in conservation planning based on IUCN categories.
#'
#' @param df A dataframe for the targets that contains IUCN categories for each species. Must contain the feature names (in order), and the targets will be added as an additional column.
#' @param IUCN_cat_targets Either a numeric or named numeric of targets to apply to IUCN categories
#' @param IUCN_col Optional string to indicate the name of the column with the IUCN categories
#'
#' @return df A dataframe with added column `target_IUCN` with the assigned targets
#' @export
#'
#' @examples
#' df <- data.frame(IUCN_category = c("EW", "EX", NA), target = c(0.3, 0.3, 0.3))
#' IUCN_cat_targets <- c("EX" = 0.8, "EW" = 0.6)
#' df <- splnr_targets_byIUCN2(df, IUCN_cat_targets = IUCN_cat_targets, IUCN_col = "IUCN_category")
splnr_targets_byIUCN2 <- function(df, IUCN_cat_targets, IUCN_col = "IUCN_category") {
  stopifnot(is.data.frame(df))
  stopifnot(IUCN_col %in% names(df))
  lookup <- data.frame(
    IUCN = names(IUCN_cat_targets),
    target_IUCN = as.numeric(IUCN_cat_targets),
    stringsAsFactors = FALSE
  )
  df <- merge(df, lookup, by.x = IUCN_col, by.y = "IUCN", all.x = TRUE)
  # Move 'species' column to the first position if it exists
  if ("species" %in% names(df)) {
    df <- df[, c("species", setdiff(names(df), "species"))]
  }
  return(df)
}


#' Combine IUCN and Inverse Area targets in a single data frame
#'
#' This function takes a data frame with columns target_area and target_IUCN,
#' and adds a new column target_combined, which is the maximum of target_area and target_IUCN for each row (ignoring NA in target_IUCN).
#'
#' @param mytargets Data frame with columns target_area and target_IUCN
#' @return mytargets Data frame with an additional column target_combined
#' @export
#'
#' @examples
#' # mytargets <- data.frame(target_area = c(0.3, 0.5), target_IUCN = c(NA, 0.6))
#' # mytargets <- combine_IUCN_InverseArea_targets(mytargets)
combine_IUCN_InverseArea_targets <- function(mytargets) {
  stopifnot(is.data.frame(mytargets))
  stopifnot("target_area" %in% names(mytargets))
  stopifnot("target_IUCN" %in% names(mytargets))
  mytargets$target_combined <- pmax(mytargets$target_area, mytargets$target_IUCN, na.rm = TRUE)
  return(mytargets)
}