#' @title Assign Targets by Inverse Area
#'
#' @description
#' This function calculates inverse area targets for each conservation feature
#' within an `sf` dataframe, based on their areal coverage. The target is set
#' to be inversely proportional to the feature's area, ranging between a
#' specified minimum (`target_min`) and maximum (`target_max`).
#'
#' @details
#' The inverse area target approach aims to assign higher conservation targets
#' to features that have a smaller overall distribution or areal coverage within
#' the study region. This can be particularly useful for prioritizing rare or
#' range-restricted features.
#'
#' The calculation proceeds as follows:
#' 1. The area of a single planning unit is determined.
#' 2. The total area of the study region is estimated by multiplying the number
#'    of planning units by the individual planning unit area.
#' 3. For each feature (species), its total area across all planning units is
#'    calculated.
#' 4. The target for each feature is then scaled between `target_min` and
#'    `target_max` such that features with smaller areas receive targets closer
#'    to `target_max`, and features with larger areas receive targets closer
#'    to `target_min`.
#'
#' The input `df` is expected to be an `sf` object where columns (excluding
#' geometry) represent different features (e.g., species presence/absence) and
#' rows represent planning units.
#'
#' @param df An `sf` dataframe containing the features (e.g., species distribution
#'   data) for which to calculate inverse area targets. Each column (excluding
#'   geometry) should represent a feature, and each row a planning unit.
#' @param target_min A numeric value between 0 and 1 (inclusive) specifying the
#'   minimum target percentage. This will be the target for the most widespread feature.
#' @param target_max A numeric value between 0 and 1 (inclusive) specifying the
#'   maximum target percentage. This will be the target for the rarest feature.
#'
#' @return A `tibble` (data frame) with two columns: `Species` (or feature name)
#'   and `target` (the calculated inverse area target for each feature).
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr across everything mutate summarise
#' @importFrom rlang .data
#' @importFrom sf st_area st_drop_geometry
#' @importFrom stringr str_replace_all
#' @importFrom tidyr pivot_longer replace_na
#'
#' @examples
#' \dontrun{
#' # Assuming 'dat_species_prob' is an existing sf object in your package,
#' # representing species distribution in planning units.
#'
#' # Calculate inverse area targets with a range from 30% to 80%.
#' targets_inverse_area <- dat_species_prob %>%
#'   splnr_targets_byInverseArea(target_min = 0.3, target_max = 0.8)
#' print(targets_inverse_area)
#'
#' # Example with a different target range (e.g., 20% to 70%)
#' targets_custom_range <- dat_species_prob %>%
#'   splnr_targets_byInverseArea(target_min = 0.2, target_max = 0.7)
#' print(targets_custom_range)
#' }
splnr_targets_byInverseArea <- function(df, target_min, target_max) {

  # Assertions to validate input parameters.
  assertthat::assert_that(
    inherits(df, "sf"), # Ensure df is an sf object.
    msg = "'df' must be an 'sf' object."
  )
  assertthat::assert_that(
    is.numeric(target_min) && length(target_min) == 1 && target_min >= 0 && target_min <= 1,
    msg = "'target_min' must be a single numeric value between 0 and 1."
  )
  assertthat::assert_that(
    is.numeric(target_max) && length(target_max) == 1 && target_max >= 0 && target_max <= 1,
    msg = "'target_max' must be a single numeric value between 0 and 1."
  )
  assertthat::assert_that(
    target_min <= target_max,
    msg = "'target_min' must be less than or equal to 'target_max'."
  )
  assertthat::assert_that(
    "geometry" %in% names(df),
    msg = "'df' must contain a 'geometry' column."
  )

  # Calculate the area of a single planning unit in km².
  PU_area_km2 <- as.numeric(sf::st_area(df[1, ]) / 1e+06)

  # Calculate the total approximate area of the study region.
  total_PU_area <- nrow(df) * PU_area_km2

  # Process the dataframe to calculate inverse area targets.
  dat <- df %>%
    # Drop the geometry column to perform numerical operations on feature data.
    sf::st_drop_geometry() %>%
    # Replace any NA values with 0 across all columns, assuming NA means absence.
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ tidyr::replace_na(.x, 0))) %>%
    # Summarise each column by summing its values to get total area coverage for each feature.
    dplyr::summarise(dplyr::across(dplyr::everything(), ~ sum(.x, na.rm = TRUE))) %>%
    # Pivot the data longer, converting feature columns into rows.
    tidyr::pivot_longer(dplyr::everything(), names_to = "Species", values_to = "Area_units") %>% # Renamed to Area_units to reflect it's sum of presence
    # Mutate to calculate actual area in km² and the inverse area target.
    dplyr::mutate(
      # Replace underscores in species names with spaces for readability.
      Species = stringr::str_replace_all(.data$Species, pattern = "_", replacement = " "),
      # Convert 'Area_units' (sum of presences) to actual area in km².
      Area_km2 = .data$Area_units * PU_area_km2,
      # Calculate the inverse area target:
      # Starts at target_max, then subtracts a proportion of the range (target_max - target_min)
      # based on the feature's relative area. Larger relative area leads to a smaller target.
      target = target_max - ((.data$Area_km2 / total_PU_area) * (target_max - target_min))
    ) %>%
    dplyr::select("Species", "target") # Select only the Species and target columns for the final output.


  return(dat)
}


#' @title Assign Targets by Category
#'
#' @description
#' The `splnr_targets_byCategory()` function assigns conservation targets to
#' features (e.g., species) based on their assigned categories. This allows for
#' differentiated conservation goals for different groups of features.
#'
#' @details
#' This function is useful in conservation planning when different types of
#' features (e.g., endangered species, common species, ecosystem types) require
#' distinct conservation targets. It performs a left join with a provided
#' named vector (`catTarg`) where names correspond to categories in your data
#' and values are the desired targets.
#'
#' The `dat` input should be an `sf` object (or data frame) that contains a
#' column (`catName`) identifying the category for each feature.
#'
#' @param dat An `sf` object (or data frame) containing the features and their
#'   associated categories. Each row should represent a feature (e.g., a species)
#'   with its attributes, including the category.
#' @param catTarg A named numeric vector where names are the categories
#'   (e.g., `"Group1"`, `"Endangered"`) and values are the corresponding
#'   conservation targets (e.g., `0.5`, `0.8`).
#' @param catName A character string specifying the name of the column in `dat`
#'   that contains the category information. Defaults to `"Category"`.
#'
#' @return An `sf` object (or data frame) identical to the input `dat`, but with an
#'   additional column named `target` containing the assigned conservation target
#'   for each feature. Features whose categories are not found in `catTarg` will
#'   have `NA` in the `target` column unless they already have a 'target' column.
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr left_join rename
#' @importFrom rlang :=
#' @importFrom tibble enframe
#'
#' @examples
#' \dontrun{
#' # Assuming 'dat_category' is an existing sf object in your package
#' # with a column named "category" and other feature data.
#'
#' # Example: Assign targets based on predefined categories
#' targets_by_group <- splnr_targets_byCategory(
#'   dat = dat_category, # Assuming dat_category has a 'category' column
#'   catTarg = c("Group1" = 0.5, "Group2" = 0.2),
#'   catName = "category"
#' )
#' print(targets_by_group)
#'
#' # Example: Assign targets with a different category column name
#' dat_alt_cat <- data.frame(Feature = letters[1:5], Type = c("A", "B", "A", "C", "B"))
#' targets_by_type <- splnr_targets_byCategory(
#'   dat = dat_alt_cat,
#'   catTarg = c("A" = 0.7, "B" = 0.4),
#'   catName = "Type"
#' )
#' print(targets_by_type)
#' }
splnr_targets_byCategory <- function(dat, catTarg, catName = "Category") {

  # Assertions to validate input parameters.
  assertthat::assert_that(
    inherits(dat, "data.frame"), # Ensure dat is a data.frame (or sf object).
    msg = "'dat' must be a data.frame or sf object."
  )
  assertthat::assert_that(
    is.character(catName) && length(catName) == 1,
    msg = "'catName' must be a single character string."
  )
  assertthat::assert_that(
    catName %in% names(dat),
    msg = paste0("The specified 'catName' (\"", catName, "\") does not exist in the input dataframe 'dat'.")
  )
  assertthat::assert_that(
    is.numeric(catTarg), # Ensure catTarg is a numeric vector.
    msg = "'catTarg' must be a numeric vector."
  )
  assertthat::assert_that(
    length(catTarg) > 0,
    msg = "'catTarg' must not be empty."
  )
  assertthat::assert_that(
    !is.null(names(catTarg)),
    msg = "'catTarg' must be a named vector (e.g., c('Category1' = 0.5))."
  )
  assertthat::assert_that(
    all(names(catTarg) %in% unique(dat[[catName]])),
    msg = paste0("Not all names in 'catTarg' match unique values in the '", catName, "' column of 'dat'.")
  )

  # Join the input dataframe with the category targets.
  dat <- dat %>%
    # Convert the named vector `catTarg` into a two-column tibble (name, value).
    dplyr::left_join(
      tibble::enframe(catTarg),
      # Join by the category column in `dat` and the 'name' column from the enframe'd catTarg.
      by = dplyr::join_by(!!catName == "name")
    ) %>%
    # Rename the 'value' column (from enframe) to 'target'.
    dplyr::rename(target = "value")

  return(dat)
}


#' @title Assign Targets by IUCN Red List Categories
#'
#' @description
#' The `splnr_targets_byIUCN()` function assigns conservation targets for species
#' based on their IUCN Red List categories. This allows for prioritizing species
#' at higher risk of extinction with more stringent conservation goals.
#'
#' @details
#' This function is crucial for integrating species' extinction risk into
#' conservation planning. It allows you to specify targets either as a single
#' numeric value (applied to all 'threatened' IUCN categories) or as a named
#' numeric vector for specific categories.
#'
#' Species can be extracted based on IUCN categories using the `spatialplanr`
#' function `splnr_get_IUCNRedList()`.
#'
#' \strong{Important:} To access the IUCN database (e.g., via `splnr_get_IUCNRedList()`),
#' you need an API login token. This token, obtained from `rredlist::rl_use_iucn()`,
#' must be set as an environment variable named `IUCN_REDLIST_KEY`
#' (e.g., `Sys.setenv(IUCN_REDLIST_KEY = "[Your Token]")`).
#'
#' The function checks if a 'target' column already exists in `dat`. If not,
#' it creates one. If it exists, new targets are coalesced with existing ones,
#' allowing for sequential application or refinement of targets.
#'
#' The "threatened" IUCN categories considered for target assignment (when a
#' single `IUCN_target` is provided) are: "EX" (Extinct), "EW" (Extinct in the Wild),
#' "CR" (Critically Endangered), "EN" (Endangered), and "VU" (Vulnerable).
#'
#' @param dat A dataframe or `sf` object containing species information,
#'   including a column with IUCN categories.
#' @param IUCN_target Either:
#'   \itemize{
#'     \item A single numeric value (e.g., `0.3`) to apply this target to all
#'           threatened IUCN categories ("EX", "EW", "CR", "EN", "VU").
#'     \item A named numeric vector (e.g., `c("EX" = 0.8, "CR" = 0.6)`) to
#'           apply specific targets to particular IUCN categories.
#'   }
#' @param IUCN_col A character string specifying the name of the column in `dat`
#'   that contains the IUCN category information. Defaults to `"IUCN_Category"`.
#'
#' @return A dataframe or `sf` object identical to the input `dat`, but with an
#'   updated or newly added `target` column reflecting the assigned conservation goals.
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr case_when coalesce left_join mutate select
#' @importFrom rlang .data sym
#' @importFrom tibble as_tibble enframe
#'
#' @examples
#' \dontrun{
#' # Example 1: Assigning specific targets to categories
#' # Create a dummy dataframe resembling output from splnr_get_IUCNRedList
#' df_species_iucn <- data.frame(
#'   Species = c("Diomedea exulans", "Hippocampus kuda",
#'               "Squatina squatina", "Common Dolphin"),
#'   IUCN_Category = c("VU", "EN", "CR", "LC")
#' )
#'
#' iucn_specific_targets <- c("EX" = 0.9, "EW" = 0.8, "CR" = 0.75, "EN" = 0.6, "VU" = 0.5)
#'
#' df_with_iucn_targets <- splnr_targets_byIUCN(
#'   dat = df_species_iucn,
#'   IUCN_target = iucn_specific_targets,
#'   IUCN_col = "IUCN_Category"
#' )
#' print(df_with_iucn_targets)
#'
#' # Example 2: Assigning a single target to all threatened categories
#' df_single_target <- splnr_targets_byIUCN(
#'   dat = df_species_iucn,
#'   IUCN_target = 0.4, # Apply 40% target to all threatened species
#'   IUCN_col = "IUCN_Category"
#' )
#' print(df_single_target)
#'
#' # Example 3: When 'dat' already has a 'target' column
#' df_pre_targets <- data.frame(
#'   Species = c("A", "B", "C"),
#'   IUCN_Category = c("CR", "LC", "EN"),
#'   target = c(0.1, 0.2, 0.1) # Existing targets
#' )
#' iucn_update_targets <- c("CR" = 0.7) # Only update CR
#' df_updated_targets <- splnr_targets_byIUCN(df_pre_targets, iucn_update_targets)
#' print(df_updated_targets)
#' }
splnr_targets_byIUCN <- function(dat, IUCN_target, IUCN_col = "IUCN_Category") {

  # Assertions to validate input parameters.
  assertthat::assert_that(
    inherits(dat, "data.frame"), # Ensure dat is a data.frame or sf object.
    msg = "'dat' must be a data.frame or sf object."
  )
  assertthat::assert_that(
    is.character(IUCN_col) && length(IUCN_col) == 1,
    msg = "'IUCN_col' must be a single character string."
  )
  assertthat::assert_that(
    IUCN_col %in% names(dat),
    msg = paste0("The specified 'IUCN_col' (\"", IUCN_col, "\") does not exist in the input dataframe 'dat'.")
  )
  assertthat::assert_that(
    (is.numeric(IUCN_target) && length(IUCN_target) == 1) || (is.numeric(IUCN_target) && !is.null(names(IUCN_target))),
    msg = "'IUCN_target' must be either a single numeric value or a named numeric vector."
  )
  if (is.numeric(IUCN_target)) {
    assertthat::assert_that(
      all(IUCN_target >= 0 & IUCN_target <= 1),
      msg = "All values in 'IUCN_target' must be between 0 and 1."
    )
  }

  # Ensure a 'target' column exists in 'dat'. If not, initialize with NA.
  if (!("target" %in% colnames(dat))) {
    dat$target <- NA
  }

  # Apply targets based on whether IUCN_target is a named vector or a single numeric.
  if (is.numeric(IUCN_target) && !is.null(names(IUCN_target))) {
    # If IUCN_target is a named vector, join and coalesce targets.
    dat <- dat %>%
      # Convert the named IUCN_target vector to a data frame for joining.
      dplyr::left_join(data.frame(IUCN_target_value = IUCN_target,
                                  IUCN_Category = names(IUCN_target)),
                       by = dplyr::join_by(!!rlang::sym(IUCN_col) == "IUCN_Category")) %>%
      # Use coalesce to update 'target' only where new IUCN_target_value is not NA.
      dplyr::mutate(target = dplyr::coalesce(.data$IUCN_target_value, .data$target)) %>%
      # Remove the temporary IUCN_target_value column.
      dplyr::select(-.data$IUCN_target_value)

  } else if (is.numeric(IUCN_target) && length(IUCN_target) == 1) {
    # If IUCN_target is a single numeric, apply to specific threatened IUCN categories.
    dat <- dat %>%
      dplyr::mutate(target = dplyr::case_when(
        # Apply the single target if the IUCN_col matches any of the threatened categories.
        !!rlang::sym(IUCN_col) %in% c("EX", "EW", "CR", "EN", "VU") ~ IUCN_target,
        TRUE ~ .data$target # Otherwise, keep the existing target value.
      ))
  }
  return(dat)
}
