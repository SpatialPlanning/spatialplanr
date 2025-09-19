#' Assign targets by Inverse Area
#'
#' This function takes a min (`target_min`) and max (`target_max`) target range and calculates an inverse area target for each feature based on areal coverage.
#'
#' @param df An `sf` dataframe with features to calculate
#' @param target_min The minimum target for inverse area
#' @param target_max The maximum target for inverse area
#'
#' @return An `sf` dataframe with Inverse Area Targets added in `Target`
#' @export
#'
#' @importFrom rlang .data
#' @examples
#' targets <- dat_species_prob %>%
#'   splnr_targets_byInverseArea(target_min = 0.3, target_max = 0.8)
splnr_targets_byInverseArea <- function(df, target_min, target_max) {

  assertthat::assert_that(
    inherits(df, c("sf", "data.frame")),
    is.numeric(target_min) && target_min >= 0 && target_min <= 1,
    is.numeric(target_max) && target_max >= 0 && target_max <= 1,
    target_min <= target_max
  )

  PU_area_km2 <- as.numeric(sf::st_area(df[1, 1]) / 1e+06) # Area of each planning unit

  total_PU_area <- nrow(df) * PU_area_km2 # Total area of the study region

  dat <- df %>%
    sf::st_drop_geometry() %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ tidyr::replace_na(.x, 0))) %>%
    dplyr::summarise(dplyr::across(dplyr::everything(), ~ sum(., is.na(.), 0))) %>%
    tidyr::pivot_longer(dplyr::everything(), names_to = "Species", values_to = "Area_km2") %>%
    dplyr::mutate(
      Species = stringr::str_replace_all(.data$Species, pattern = "_", replacement = " "),
      Area_km2 = .data$Area_km2 * PU_area_km2,
      target = target_max - ((.data$Area_km2 / total_PU_area) * (target_max - target_min))
    )

  return(dat)
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
splnr_targets_byCategory <- function(dat, catTarg, catName = "Category") {

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
#' @param dat A dataframe or sf object with IUCN categories
#' @param IUCN_target Either a numeric or named numeric of targets to apply to IUCN categories
#' @param IUCN_col Optional string to indicate the name of the column with the IUCN categories
#'
#' @return dataframe or sf object
#' @export
#'
#' @examples
#' dat <- data.frame(IUCN_Category = c("EW", "EX", NA), target = c(0.3, 0.3, 0.3))
#' IUCN_target <- c("EX" = 0.8, "EW" = 0.6)
#' dat <- splnr_targets_byIUCN(dat, IUCN_target)
splnr_targets_byIUCN <- function(dat, IUCN_target, IUCN_col = "IUCN_Category") {

  assertthat::assert_that(
    inherits(dat, c("sf", "data.frame")),
    is.na(IUCN_col) || is.character(IUCN_col),
    IUCN_col %in% names(dat),
    (is.numeric(IUCN_target) && length(IUCN_target) == 1) || is.vector(IUCN_target)
  )

  if ("target" %in% colnames(dat) == FALSE) {
    dat$target <- NA
  }

  if (is.vector(IUCN_target, mode = "numeric") & !is.null(names(IUCN_target))) {
    # If the target is a named vector, apply the relevant targets
    dat <- dat %>%
      dplyr::left_join(data.frame(IUCN_target, col1 = names(IUCN_target)), by = dplyr::join_by(!!rlang::sym(IUCN_col) == "col1")) %>%
      dplyr::mutate(target = dplyr::coalesce(IUCN_target, .data$target)) %>%
      dplyr::select(-IUCN_target)

  } else if (is.numeric(IUCN_target) & length(IUCN_target) == 1) {
    # If the target is a single numeric, apply to all IUCN categories.

    dat <- dat %>%
      dplyr::mutate(target = dplyr::case_when(
        !!rlang::sym(IUCN_col) %in% c("EX", "EW", "CR", "EN", "VU") ~ IUCN_target,
        TRUE ~ dat$target
      ))
  }
  return(dat)
}
