#' @title Preprocess Feature Data for Climate Priority Area Approach
#'
#' @description
#' This internal function prepares feature data for the Climate Priority Area (CPA)
#' approach. It divides each feature's distribution into "climate-smart" (CS) and
#' "non-climate-smart" (NCS) areas based on a user-defined percentile cutoff for a
#' given climate metric.
#'
#' @details
#' The CPA approach aims to prioritize areas that are both important for
#' biodiversity features and are considered resilient to climate change. This
#' preprocessing step identifies, for each individual feature, which of its
#' occupied planning units fall within the most climate-smart `percentile` of
#' the climate metric.
#'
#' For each feature, the function performs the following steps:
#' 1. Joins the feature data with the climate metric data.
#' 2. Filters to include only planning units where the feature is present.
#' 3. Calculates the specified `percentile` cutoff for the climate metric within
#'    this filtered set of planning units.
#' 4. Creates two new binary columns for each feature:
#'    - `_CS` (Climate-Smart): Indicates planning units where the feature is
#'      present AND the climate metric meets the climate-smart criteria (e.g.,
#'      top 5% for direction 1, bottom 5% for direction -1).
#'    - `_NCS` (Non-Climate-Smart): Indicates planning units where the feature
#'      is present BUT the climate metric does NOT meet the climate-smart criteria.
#'
#' The `direction` parameter is crucial:
#' - `direction = 1`: Higher values of the `metric` indicate more climate-smart
#'   areas (e.g., lower warming rates). The function identifies areas with metric
#'   values greater than or equal to the `(100 - percentile)`th quantile.
#' - `direction = -1`: Lower values of the `metric` indicate more climate-smart
#'   areas (e.g., less acidification). The function identifies areas with metric
#'   values less than or equal to the `percentile`th quantile.
#'
#' @param features An `sf` object representing conservation features. Each column
#'   (excluding geometry) should typically be a binary representation of a feature's
#'   presence (1) or absence (0) in each Planning Unit.
#' @param metric An `sf` object containing climate metric information. It must
#'   have a column named 'metric' with the climate metric values for each Planning Unit.
#' @param percentile A numeric value (0-100) representing the cutoff threshold for
#'   determining climate-smart areas. For example, `percentile = 5` means the
#'   most climate-smart 5% of areas (based on `direction`) are considered.
#'   This value represents the lower limit of the threshold (e.g., lower 5th
#'   percentile of warming or upper 95th percentile of acidification).
#' @param direction An integer specifying the direction of climate-smartness:
#'   \itemize{
#'     \item `1`: Higher metric values mean more climate-smart.
#'     \item `-1`: Lower metric values mean more climate-smart.
#'   }
#'
#' @return An `sf` dataframe with new columns for each original feature, split
#'   into `_CS` (climate-smart) and `_NCS` (non-climate-smart) areas, indicating
#'   the binary presence (1) or absence (0) of that feature within those climate
#'   categories. This dataframe retains the original geometry.
#' @keywords internal
#' @noRd
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr across bind_cols filter if_else mutate select
#' @importFrom rlang .data sym
#' @importFrom sf st_drop_geometry st_join st_as_sf
#' @importFrom stats quantile
#' @importFrom tidyselect matches
#' @importFrom tidyr replace_na
#'
#' @examples
#' \dontrun{
#' # Assuming 'dat_species_bin' and 'dat_clim' are existing sf objects
#' # in your package. 'dat_species_bin' has binary species data,
#' # and 'dat_clim' has a 'metric' column.
#'
#' # Example: Identify climate-smart areas for species where
#' # higher metric values mean more climate-smart (e.g., lower warming).
#' out_sf_cs_areas <- splnr_climate_priorityArea_preprocess(
#'   features = dat_species_bin,
#'   percentile = 5,
#'   metric = dat_clim,
#'   direction = 1
#' )
#' print(out_sf_cs_areas)
#'
#' # Example: Identify climate-smart areas where
#' # lower metric values mean more climate-smart (e.g., less acidification).
#' out_sf_ncs_areas <- splnr_climate_priorityArea_preprocess(
#'   features = dat_species_bin,
#'   percentile = 10,
#'   metric = dat_clim,
#'   direction = -1
#' )
#' print(out_sf_ncs_areas)
#' }
splnr_climate_priorityArea_preprocess <- function(features,
                                                  percentile,
                                                  metric,
                                                  direction) {
  # Assertions to validate input parameters.
  assertthat::assert_that(
    inherits(features, "sf"),
    msg = "'features' must be an 'sf' object."
  )
  assertthat::assert_that(
    inherits(metric, "sf"),
    msg = "'metric' must be an 'sf' object."
  )
  assertthat::assert_that(
    "metric" %in% names(metric),
    msg = "'metric' sf object must contain a column named 'metric'."
  )
  assertthat::assert_that(
    is.numeric(percentile) && length(percentile) == 1 && percentile >= 0 && percentile <= 100,
    msg = "'percentile' must be a single numeric value between 0 and 100."
  )
  assertthat::assert_that(
    direction %in% c(1, -1),
    msg = "'direction' must be either 1 (higher metric = more climate-smart) or -1 (lower metric = more climate-smart)."
  )
  assertthat::assert_that(
    sf::st_crs(features) == sf::st_crs(metric),
    msg = "CRS of 'features' and 'metric' must be the same."
  )

  # Extract column names of the features (excluding geometry).
  spp <- features %>%
    sf::st_drop_geometry() %>%
    names()

  # Initialize an empty list to store processed data for each feature.
  imptList <- list()
  for (i in 1:length(spp)) {
    # Select one feature at a time from the 'features' data and join with the 'metric' data.
    df <- features %>%
      dplyr::select(!!rlang::sym(spp[i])) %>%
      sf::st_join(metric, join = sf::st_equals)

    # Check for NAs in the 'metric' column after joining and print a warning if found.
    if (any(is.na(df$metric))) {
      message(paste0("Warning: There are NAs in the metric data for feature '", spp[i], "'. These will be removed from quantile calculation."))
    }

    # Filter to select only rows where the current biodiversity feature is present (value = 1).
    filteredDF <- df %>%
      dplyr::filter(!!rlang::sym(spp[i]) == 1)

    # Handle cases where filteredDF might be empty (feature not present in any unit or only in NAs)
    if (nrow(filteredDF) == 0) {
      warning(paste0("Feature '", spp[i], "' is not present in any Planning Unit with valid metric data. Skipping climate-smart area calculation for this feature."))
      # Create an empty sf object with expected columns for binding later
      temp_df <- df %>%
        dplyr::mutate(V1 = 0, V2 = 0) %>% # Add V1 and V2 columns with 0 values
        dplyr::mutate(!!rlang::sym(paste0(spp[i], "_CS")) := 0) %>%
        dplyr::select(!!rlang::sym(paste0(spp[i], "_CS")))
      if (i > 1 && "geometry" %in% names(temp_df)) { # Remove geometry if not the first iteration
        temp_df <- temp_df %>% sf::st_drop_geometry()
      }
      imptList[[i]] <- temp_df
      next # Skip to the next iteration of the loop
    }


    # Determine the percentile cutoff based on the specified direction.
    if (direction == 1) {
      # If higher values are more climate-smart, calculate the (100 - percentile)th quantile.
      prct <- (100 - percentile) / 100
      qntl <- stats::quantile(filteredDF$metric, prct, na.rm = TRUE)[[1]] # Get the percentile, ignoring NAs.

      # Mutate to create V1 (climate-smart indicator) and V2 (feature presence indicator).
      df <- df %>%
        dplyr::mutate(
          V1 = dplyr::if_else(.data$metric >= qntl, true = 1, false = 0),
          V2 = dplyr::if_else(!!rlang::sym(spp[i]) == 1, true = 1, false = 0)
        )
    } else if (direction == -1) {
      # If lower values are more climate-smart, calculate the percentileth quantile.
      prct <- percentile / 100
      qntl <- stats::quantile(filteredDF$metric, prct, na.rm = TRUE)[[1]] # Get the percentile, ignoring NAs.

      # Mutate to create V1 (climate-smart indicator) and V2 (feature presence indicator).
      df <- df %>%
        dplyr::mutate(
          V1 = dplyr::if_else(.data$metric <= qntl, true = 1, false = 0),
          V2 = dplyr::if_else(!!rlang::sym(spp[i]) == 1, true = 1, false = 0)
        )
    } else {
      # If an invalid direction is provided, print an error (should be caught by assertthat).
      if (i == 1) { # Only print for the first iteration to avoid redundant messages.
        stop("Please enter a valid direction: either 1 or -1.")
      }
    }

    # Drop geometry for subsequent bind_cols operations if not the first iteration.
    if (i > 1 && "geometry" %in% names(df)){
      df <- df %>%
        sf::st_drop_geometry()
    }

    # Calculate the Climate-Smart (CS) area for the current feature.
    imptList[[i]] <- df %>%
      dplyr::mutate(!!rlang::sym(paste0(spp[i], "_CS")) := .data$V1 * .data$V2) %>% # CS = climate-smart areas (feature present AND within climate-smart percentile)
      dplyr::select(!!rlang::sym(paste0(spp[i], "_CS"))) # Select only the CS column.
  }

  # Combine all individual feature CS data frames into a single dataframe.
  imptList <- do.call(dplyr::bind_cols, imptList)

  # Initialize an empty list to store processed data for each feature's NCS component.
  repList <- list()
  for (i in 1:length(spp)) {

    # Select 1 species at a time from original features data and join with metric.
    df1 <- features %>%
      dplyr::select(!!rlang::sym(spp[i])) %>%
      sf::st_join(metric, join = sf::st_equals)

    # Select the CS column for the current feature from the previously created imptList.
    df2 <- imptList %>%
      dplyr::select(!!rlang::sym(paste0(spp[i], "_CS")))

    # Join the original feature data with its CS status.
    df3 <- sf::st_join(df1, df2, join = sf::st_equals)

    # Drop geometry for subsequent bind_cols operations if not the first iteration.
    if (i > 1 && "geometry" %in% names(df3)){ # Check if geometry exists before dropping
      df3 <- df3 %>%
        sf::st_drop_geometry()
    }

    # Calculate the Non-Climate-Smart (NCS) area for the current feature.
    repList[[i]] <- df3 %>%
      dplyr::mutate(!!rlang::sym(paste0(spp[i], "_NCS")) := dplyr::if_else(
        !!rlang::sym(paste0(spp[i], "_CS")) == 1, # If the area is CS, then NCS is 0.
        true = 0,
        false = .data[[spp[i]]] # Otherwise, NCS is 1 if feature is present, 0 if not.
      )) %>%
      # Select both NCS and CS columns for the current feature.
      dplyr::select(tidyselect::matches("_NCS|_CS"))
  }

  # Combine all individual feature NCS and CS data frames into a single dataframe.
  # Ensure geometry is handled correctly and convert back to sf object.
  repList <- do.call(dplyr::bind_cols, repList) %>%
    # Re-add geometry from the original features, as bind_cols might drop it.
    sf::st_set_geometry(features$geometry) %>%
    # Select all columns, ensuring the order.
    dplyr::select(tidyselect::everything()) %>%
    sf::st_as_sf()


  return(repList)
}


#' @title Assign Targets for Climate Priority Area Approach
#'
#' @description
#' This internal function calculates and assigns conservation targets for features
#' when using the Climate Priority Area (CPA) approach. It differentiates targets
#' for climate-smart (CS) and non-climate-smart (NCS) areas.
#'
#' @details
#' This function is a core component of the `splnr_climate_priorityAreaApproach()`.
#' It takes the initial targets for each feature and adjusts them based on the
#' proportion of the feature found in climate-smart areas, and a `refugiaTarget`.
#'
#' The logic for target adjustment is as follows:
#' 1. For each feature, it calculates the total number of planning units it occupies.
#' 2. It determines the proportion of the feature's total presence that falls
#'    within climate-smart areas (i.e., `_CS` areas).
#' 3. If this climate-smart proportion is greater than the feature's original target,
#'    the target for the `_CS` component is adjusted proportionally, and the
#'    `_NCS` component's target is set to 0 (meaning all necessary representation
#'    can be met within CS areas).
#' 4. Otherwise, the target for the `_CS` component is set to `refugiaTarget`
#'    (typically 100% or 1), and the remaining part of the original target is
#'    assigned to the `_NCS` component. This ensures that the entire `refugiaTarget`
#'    for the CS portion is met, and any shortfall is covered by NCS areas.
#'
#' The output `data.frame` contains the new adjusted targets for each feature,
#' now split into `_CS` and `_NCS` components.
#'
#' @param targets A `data.frame` with two columns: `feature` (character, listing
#'   the original feature names) and `target` (numeric, the conservation target
#'   for each feature as a proportion, e.g., 0.3).
#' @param climateSmartDF An `sf` object (or data frame) produced by
#'   `splnr_climate_priorityArea_preprocess()`. This dataframe contains the
#'   original features split into `_CS` and `_NCS` components.
#' @param refugiaTarget A numeric value (0-1) representing the target proportion
#'   assigned specifically to climate-smart areas (refugia). Defaults to `1` (100%).
#'
#' @return A `data.frame` with two columns: `feature` (character, now including
#'   `_CS` and `_NCS` suffixes for each original feature) and `target` (the
#'   newly calculated targets for each climate-split feature).
#' @keywords internal
#' @noRd
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr across bind_rows case_when filter full_join if_else pull select summarize
#' @importFrom rlang .data sym
#' @importFrom sf st_drop_geometry
#' @importFrom stringr str_c str_ends
#' @importFrom tibble  tribble
#' @importFrom tidyr replace_na pivot_longer
#'
#' @examples
#' \dontrun{
#' # Assuming 'dat_species_bin' and 'dat_clim' are existing sf objects.
#'
#' # Define initial targets for species features.
#' initial_targets <- dat_species_bin %>%
#'   sf::st_drop_geometry() %>%
#'   colnames() %>%
#'   data.frame() %>%
#'   setNames(c("feature")) %>%
#'   dplyr::mutate(target = 0.3)
#'
#' # Preprocess features to get CS/NCS split.
#' preprocessed_features <- splnr_climate_priorityArea_preprocess(
#'   features = dat_species_bin,
#'   percentile = 5,
#'   metric = dat_clim,
#'   direction = 1
#' )
#'
#' # Assign targets using the climate-smart logic.
#' cpa_assigned_targets <- splnr_climate_priorityArea_assignTargets(
#'   targets = initial_targets,
#'   climateSmartDF = preprocessed_features,
#'   refugiaTarget = 1 # Aim for 100% representation in climate-smart areas if possible.
#' )
#' print(cpa_assigned_targets)
#' }
splnr_climate_priorityArea_assignTargets <- function(targets,
                                                     climateSmartDF,
                                                     refugiaTarget = 1) {
  # Assertions to validate input parameters.
  assertthat::assert_that(
    is.data.frame(targets),
    msg = "'targets' must be a data.frame."
  )
  assertthat::assert_that(
    "feature" %in% names(targets),
    msg = "'targets' data.frame must contain a 'feature' column."
  )
  assertthat::assert_that(
    "target" %in% names(targets),
    msg = "'targets' data.frame must contain a 'target' column."
  )
  assertthat::assert_that(
    inherits(climateSmartDF, "data.frame"), # can be sf or just df after dropping geom
    msg = "'climateSmartDF' must be a data.frame (or sf object)."
  )
  assertthat::assert_that(
    is.numeric(refugiaTarget) && length(refugiaTarget) == 1 && refugiaTarget >= 0 && refugiaTarget <= 1,
    msg = "'refugiaTarget' must be a single numeric value between 0 and 1."
  )
  # Check if climateSmartDF contains expected _CS and _NCS columns
  assertthat::assert_that(
    any(grepl("_CS$", names(climateSmartDF))) && any(grepl("_NCS$", names(climateSmartDF))),
    msg = "'climateSmartDF' must contain columns with '_CS' and '_NCS' suffixes (output of splnr_climate_priorityArea_preprocess)."
  )


  # Extract original species names from the 'targets' dataframe.
  spp <- targets %>%
    dplyr::select("feature") %>%
    dplyr::pull()

  # Calculate total planning units for each climate-split feature type.
  featDF <- climateSmartDF %>%
    sf::st_drop_geometry() %>% # Drop geometry to perform numerical summaries.
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ tidyr::replace_na(.x, 0))) %>% # Replace NAs with 0.
    dplyr::summarize(dplyr::across(tidyselect::everything(), sum)) %>% # Sum up presence counts for each feature component.
    tidyr::pivot_longer(tidyselect::everything(), names_to = "feature", values_to = "planunit") # Pivot to long format.

  finalList <- list() # Initialize an empty list to store adjusted targets for each feature.
  for (i in 1:length(spp)) {
    # Filter to get the original target for the current feature.
    filteredTarget <- targets %>%
      dplyr::filter(.data$feature == spp[i])

    trgt <- filteredTarget$target # Extract the target value.

    # Define the climate-split feature names for the current original feature.
    vars <- c(
      stringr::str_c(spp[i], "_CS"),
      stringr::str_c(spp[i], "_NCS")
    )

    # Join the feature unit counts with the original targets.
    suppressMessages({ # Suppress join messages.
      assignTarget <- featDF %>%
        dplyr::filter(.data$feature %in% vars) %>%
        dplyr::full_join(filteredTarget)
    })

    # Calculate the total number of planning units where the original feature is present.
    # This sum includes both CS and NCS components for that feature.
    sumUnits <- sum(assignTarget$planunit, na.rm = TRUE)

    # Adjust 'planunit' for the original feature's row to represent its total units, then calculate proportion.
    assignTarget1 <- assignTarget %>%
      dplyr::mutate(planunit = dplyr::if_else(stringr::str_ends(.data$feature, spp[i]), true = sumUnits, false = .data$planunit)) %>%
      dplyr::mutate(proportion = .data$planunit / sumUnits)

    # Get the proportion of the feature that is in climate-smart areas.
    reltargetCS <- assignTarget1[assignTarget1$feature == paste0(spp[i], "_CS"), "proportion"] %>% dplyr::pull()

    # Conditional logic for assigning targets to CS and NCS components.
    if (reltargetCS > assignTarget1[assignTarget1$feature == spp[i], "target"]) {
      # If the climate-smart proportion is already greater than the original target,
      # set CS target proportionally and NCS target to 0.
      targetCS <- (assignTarget1[assignTarget1$feature == spp[i], "target"] %>% as.numeric()) / (assignTarget1[assignTarget1$feature == paste0(spp[i], "_CS"), "proportion"] %>% as.numeric())
      targetNCS <- 0
    } else {
      # Otherwise, set CS target to refugiaTarget (usually 100%), and assign remaining target to NCS.
      targetCS <- refugiaTarget
      # Calculate the remaining target for NCS areas.
      targetNCS <- ((assignTarget1[assignTarget1$feature == spp[i], "target"] %>% as.numeric()) - reltargetCS) / (assignTarget1[assignTarget1$feature == paste0(spp[i], "_NCS"), "proportion"] %>% as.numeric())
    }

    # Store the adjusted targets for the current feature's CS and NCS components.
    finalList[[i]] <- assignTarget1 %>%
      dplyr::mutate(target = dplyr::case_when(
        stringr::str_ends(.data$feature, "_CS") ~ targetCS, # Assign calculated CS target.
        stringr::str_ends(.data$feature, "_NCS") ~ targetNCS # Assign calculated NCS target.
      )) %>%
      dplyr::filter(.data$feature != spp[i]) %>% # Remove the original feature row.
      dplyr::select("feature", "target") # Select only feature name and adjusted target.
  }

  # Combine all adjusted targets for all features into a single dataframe.
  finalDF <- do.call(dplyr::bind_rows, finalList)

  return(finalDF)
}

#' @title Run the Climate Priority Area (CPA) Approach
#'
#' @description
#' `splnr_climate_priorityAreaApproach()` implements the Climate Priority Area
#' approach by splitting conservation features into climate-smart (CS) and
#' non-climate-smart (NCS) components and adjusting their targets accordingly.
#' This allows conservation planning to prioritize areas with higher climate resilience.
#'
#' @details
#' This function orchestrates the steps required for the CPA approach:
#' 1. **Preprocessing:** It calls `splnr_climate_priorityArea_preprocess()` to
#'    categorize each feature's occurrences into CS and NCS areas based on a
#'    climate metric and a specified `percentile` cutoff.
#' 2. **Target Assignment:** It then calls `splnr_climate_priorityArea_assignTargets()`
#'    to calculate and assign new targets for these CS and NCS feature components.
#'    This ensures that conservation goals reflect the desired emphasis on climate-smart
#'    areas (e.g., aiming for 100% representation of features in highly resilient areas).
#'
#' The output of this function is a list containing the modified features (now
#' split into CS/NCS components) and their corresponding adjusted targets, ready
#' to be used in a `prioritizr` conservation problem.
#'
#' @param features An `sf` object representing conservation features (e.g., species
#'   distribution data). Each column (excluding geometry) should typically be a
#'   binary representation of a feature's presence (1) or absence (0) in each
#'   Planning Unit.
#' @param metric An `sf` object containing climate metric information. It must
#'   have a column named 'metric' with the climate metric values per Planning Unit.
#' @param targets A `data.frame` with two columns: `feature` (character, listing
#'   the original feature names) and `target` (numeric, the initial conservation
#'   target for each feature as a proportion, e.g., 0.3).
#' @param direction An integer specifying the direction of climate-smartness:
#'   \itemize{
#'     \item `1`: Higher metric values mean more climate-smart areas.
#'     \item `-1`: Lower metric values mean more climate-smart areas.
#'   }
#' @param percentile A numeric value (0-100) representing the cutoff threshold for
#'   determining climate-smart areas. For example, `percentile = 5` means the
#'   most climate-smart 5% of areas (based on `direction`) are considered.
#'   This value represents the lower limit of the threshold. Defaults to `5`.
#' @param refugiaTarget A numeric value (0-1) representing the target proportion
#'   assigned specifically to climate-smart areas (refugia). Defaults to `1` (100%).
#'
#' @return A `list` with two components:
#'   \itemize{
#'     \item `Features`: An `sf` object containing the binary information per
#'           Planning Unit for each feature, now split into `_CS` (climate-smart)
#'           and `_NCS` (non-climate-smart) components. This is ready to be
#'           passed to `prioritizr` when creating a conservation problem.
#'     \item `Targets`: A `data.frame` with the adjusted targets for the
#'           climate-split features. This is also ready for `prioritizr`.
#'   }
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom rlang .data
#' @importFrom sf st_crs
#'
#' @examples
#' \dontrun{
#' # Assuming 'dat_species_bin' and 'dat_clim' are existing sf objects
#' # in your package.
#'
#' # Define initial targets for species features.
#' initial_targets <- dat_species_bin %>%
#'   sf::st_drop_geometry() %>%
#'   colnames() %>%
#'   data.frame() %>%
#'   setNames(c("feature")) %>%
#'   dplyr::mutate(target = 0.3)
#'
#' # Run the Climate Priority Area Approach where lower climate metric
#' # values mean more climate-smart areas.
#' CPA_Approach_result <- splnr_climate_priorityAreaApproach(
#'   features = dat_species_bin,
#'   metric = dat_clim,
#'   targets = initial_targets,
#'   direction = -1, # Example: lower metric values are more climate-smart
#'   percentile = 5,
#'   refugiaTarget = 1
#' )
#'
#' # Access the processed features and targets:
#' out_sf_cpa <- CPA_Approach_result$Features
#' targets_cpa <- CPA_Approach_result$Targets
#'
#' print(head(out_sf_cpa))
#' print(head(targets_cpa))
#' }
splnr_climate_priorityAreaApproach <- function(features,
                                               metric,
                                               targets,
                                               direction,
                                               percentile = 5,
                                               refugiaTarget = 1) {

  # Assertions to validate input parameters.
  assertthat::assert_that(
    inherits(features, "sf"),
    msg = "'features' must be an 'sf' object."
  )
  assertthat::assert_that(
    inherits(metric, "sf"),
    msg = "'metric' must be an 'sf' object."
  )
  assertthat::assert_that(
    "metric" %in% names(metric),
    msg = "'metric' sf object must contain a column named 'metric'."
  )
  assertthat::assert_that(
    is.data.frame(targets),
    msg = "'targets' must be a data.frame."
  )
  assertthat::assert_that(
    "feature" %in% names(targets),
    msg = "'targets' data.frame must contain a 'feature' column."
  )
  assertthat::assert_that(
    "target" %in% names(targets),
    msg = "'targets' data.frame must contain a 'target' column."
  )
  assertthat::assert_that(
    direction %in% c(-1, 1),
    msg = "'direction' must be either 1 (higher metric = more climate-smart) or -1 (lower metric = more climate-smart)."
  )
  assertthat::assert_that(
    is.numeric(percentile) && length(percentile) == 1 && percentile >= 0 && percentile <= 100,
    msg = "'percentile' must be a single numeric value between 0 and 100."
  )
  assertthat::assert_that(
    is.numeric(refugiaTarget) && length(refugiaTarget) == 1 && refugiaTarget >= 0 && refugiaTarget <= 1,
    msg = "'refugiaTarget' must be a single numeric value between 0 and 1."
  )
  assertthat::assert_that(
    sf::st_crs(features) == sf::st_crs(metric),
    msg = "CRS of 'features' and 'metric' must be the same."
  )

  # Preprocess features to split them into climate-smart (CS) and non-climate-smart (NCS) areas.
  CPAFeatures <- splnr_climate_priorityArea_preprocess(
    features = features,
    metric = metric,
    direction = direction,
    percentile = percentile
  )

  # Assign adjusted targets for the CS and NCS feature components.
  CPATargets <- splnr_climate_priorityArea_assignTargets(
    targets = targets,
    climateSmartDF = CPAFeatures, # Use the preprocessed features for target assignment.
    refugiaTarget = refugiaTarget
  )
  # Return a list containing both the processed features and their adjusted targets.
  return(list(Features = CPAFeatures, Targets = CPATargets))
}

  ##### Feature Approach ####

#' @title Preprocess Data for Feature Climate-Smart Approach
#'
#' @description
#' This internal function creates a "climate layer" by identifying the most
#' climate-smart areas across the entire planning region, based on a percentile
#' cutoff for a given climate metric. This layer is then attached to the
#' original features data.
#'
#' @details
#' The Feature Approach to climate-smart conservation aims to prioritize a fixed
#' proportion of the most climate-resilient areas (the `climate_layer`) and
#' ensure that conservation features are represented within this layer.
#'
#' This preprocessing step involves:
#' 1. Identifying the `percentile` cutoff for the global climate `metric` data
#'    (not per feature, as in CPA).
#' 2. Creating a binary `climate_layer` where planning units meeting the
#'    climate-smart criteria are marked as 1, and others as 0.
#' 3. Joining this `climate_layer` back to the original `features` data.
#'
#' The `direction` parameter functions similarly to the CPA approach:
#' - `direction = 1`: Higher values of the `metric` are considered more climate-smart.
#'   The `climate_layer` will include areas with metric values >= the `percentile`th quantile.
#' - `direction = -1`: Lower values of the `metric` are considered more climate-smart.
#'   The `climate_layer` will include areas with metric values <= the `percentile`th quantile.
#'
#' @param features An `sf` object representing conservation features.
#' @param metric An `sf` object containing climate metric information. It must
#'   have a column named 'metric' with the climate metric values per Planning Unit.
#' @param percentile A numeric value (0-100) representing the cutoff threshold for
#'   determining whether an area is a climate priority area or not. This is applied
#'   globally to the `metric` data.
#' @param direction An integer specifying the direction of climate-smartness:
#'   \itemize{
#'     \item `1`: Higher metric values mean more climate-smart.
#'     \item `-1`: Lower metric values mean more climate-smart.
#'   }
#'
#' @return An `sf` dataframe identical to the input `features`, but with an
#'   additional binary column named `climate_layer` indicating which Planning
#'   Units are considered climate-smart.
#' @keywords internal
#' @noRd
#' @importFrom assertthat assert_that
#' @importFrom dplyr mutate select
#' @importFrom rlang .data
#' @importFrom sf st_join
#' @importFrom stats quantile
#'
#' @examples
#' \dontrun{
#'
#' # Example: Create a climate layer where higher metric values are climate-smart.
#' feature_preprocessed_data <- splnr_climate_feature_preprocess(
#'   features = dat_species_bin,
#'   percentile = 5, # Top 5% most climate-smart areas
#'   metric = dat_clim,
#'   direction = 1
#' )
#' print(feature_preprocessed_data)
#'
#' # Example: Create a climate layer where lower metric values are climate-smart.
#' feature_preprocessed_data_alt <- splnr_climate_feature_preprocess(
#'   features = dat_species_bin,
#'   percentile = 10, # Bottom 10% most climate-smart areas
#'   metric = dat_clim,
#'   direction = -1
#' )
#' print(feature_preprocessed_data_alt)
#' }
splnr_climate_feature_preprocess <- function(features,
                                             percentile,
                                             metric,
                                             direction) {
  # Assertions to validate input parameters.
  assertthat::assert_that(
    inherits(features, "sf"),
    msg = "'features' must be an 'sf' object."
  )
  assertthat::assert_that(
    inherits(metric, "sf"),
    msg = "'metric' must be an 'sf' object."
  )
  assertthat::assert_that(
    "metric" %in% names(metric),
    msg = "'metric' sf object must contain a column named 'metric'."
  )
  assertthat::assert_that(
    is.numeric(percentile) && length(percentile) == 1 && percentile >= 0 && percentile <= 100,
    msg = "'percentile' must be a single numeric value between 0 and 100."
  )
  assertthat::assert_that(
    direction %in% c(1, -1),
    msg = "'direction' must be either 1 (higher metric = more climate-smart) or -1 (lower metric = more climate-smart)."
  )
  assertthat::assert_that(
    sf::st_crs(features) == sf::st_crs(metric),
    msg = "CRS of 'features' and 'metric' must be the same."
  )


  # Check for NAs in the 'metric' column and print a warning if found.
  if (any(is.na(metric$metric))) { # Accessing 'metric' directly from 'metric' sf object
    message("Warning: There are some NAs in the metric data. These will be removed from percentile calculation.")
  }

  # Convert percentile to proportion for quantile calculation.
  prct <- percentile / 100
  # Calculate the quantile cutoff based on the global metric values.
  qntl <- stats::quantile(metric$metric, prct, na.rm = TRUE)[[1]]

  # Create the climate layer based on the direction.
  if (direction == 1) {
    message("Higher values mean more climate-smart areas.") # Inform user about the direction.
    # Create the binary climate_layer: 1 if metric >= quantile, 0 otherwise.
    df <- metric %>%
      dplyr::mutate(climate_layer = ifelse(.data$metric >= qntl, yes = 1, no = 0))
  } else if (direction == -1) {
    message("Lower values mean more climate-smart areas.") # Inform user about the direction.
    # Create the binary climate_layer: 1 if metric <= quantile, 0 otherwise.
    df <- metric %>%
      dplyr::mutate(climate_layer = ifelse(.data$metric <= qntl, yes = 1, no = 0))
  } else {
    # This case should ideally be caught by assertthat, but included as a fallback.
    stop("Please enter a valid direction: either 1 or -1.")
  }

  # Select only the newly created 'climate_layer' column.
  climateSmartDF <- df %>%
    dplyr::select("climate_layer")

  # Attach the "climate_layer" to the original features dataframe.
  features <- features %>%
    sf::st_join(climateSmartDF, join = sf::st_equals)

  return(features)
}

#' @title Assign Targets for Feature Climate-Smart Approach
#'
#' @description
#' This internal function calculates and assigns conservation targets when using
#' the Feature Approach to climate-smart spatial planning. It adjusts targets
#' to ensure a specified `refugiaTarget` is met within the climate-smart layer.
#'
#' @details
#' This function is a key component of the `splnr_climate_featureApproach()`.
#' It takes the initial conservation targets for features and modifies them
#' by introducing a new target for the `climate_layer` itself.
#'
#' The target for the `climate_layer` is calculated as `refugiaTarget` divided
#' by the proportion of the total planning units that are designated as
#' "climate-smart" (i.e., `sum(climateSmartDF$climate_layer) / nrow(climateSmartDF)`).
#' This effectively scales up the `refugiaTarget` to ensure that when `prioritizr`
#' tries to select `refugiaTarget` proportion of the total planning units for the
#' `climate_layer`, it will actually aim to select that proportion *within* the
#' climate-smart areas.
#'
#' The new `climate_layer` target is then appended to the original feature targets.
#'
#' @param climateSmartDF An `sf` object (or data frame) containing the
#'   `climate_layer` column, typically produced by `splnr_climate_feature_preprocess()`.
#' @param refugiaTarget A numeric value (0-1) representing the target proportion
#'   of climate-smart areas that should be selected.
#' @param targets A `data.frame` with two columns: `feature` (character, listing
#'   the original feature names) and `target` (numeric, the initial conservation
#'   target for each feature as a proportion, e.g., 0.3).
#'
#' @return A `data.frame` with two columns: `feature` (character, including
#'   original feature names and "climate_layer") and `target` (the calculated
#'   targets for these features).
#' @keywords internal
#' @noRd
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows
#' @importFrom tibble tribble
#'
#' @examples
#' \dontrun{
#' # Assuming 'dat_species_bin' and 'dat_clim' are existing sf objects.
#'
#' # Define initial targets for species features.
#' initial_targets <- dat_species_bin %>%
#'   sf::st_drop_geometry() %>%
#'   colnames() %>%
#'   data.frame() %>%
#'   setNames(c("feature")) %>%
#'   dplyr::mutate(target = 0.3)
#'
#' # Preprocess features to get the climate layer.
#' preprocessed_features <- splnr_climate_feature_preprocess(
#'   features = dat_species_bin,
#'   percentile = 5,
#'   metric = dat_clim,
#'   direction = 1
#' )
#'
#' # Assign targets for the feature approach.
#' feature_assigned_targets <- splnr_climate_feature_assignTargets(
#'   climateSmartDF = preprocessed_features,
#'   refugiaTarget = 0.3, # Aim for 30% of climate-smart areas to be selected.
#'   targets = initial_targets
#' )
#' print(feature_assigned_targets)
#' }
splnr_climate_feature_assignTargets <- function(climateSmartDF,
                                                refugiaTarget,
                                                targets) {

  # Assertions to validate input parameters.
  assertthat::assert_that(
    inherits(climateSmartDF, "data.frame"),
    msg = "'climateSmartDF' must be a data.frame (or sf object)."
  )
  assertthat::assert_that(
    "climate_layer" %in% names(climateSmartDF),
    msg = "'climateSmartDF' must contain a 'climate_layer' column (output of splnr_climate_feature_preprocess)."
  )
  assertthat::assert_that(
    is.numeric(refugiaTarget) && length(refugiaTarget) == 1 && refugiaTarget >= 0 && refugiaTarget <= 1,
    msg = "'refugiaTarget' must be a single numeric value between 0 and 1."
  )
  assertthat::assert_that(
    is.data.frame(targets),
    msg = "'targets' must be a data.frame."
  )
  assertthat::assert_that(
    "feature" %in% names(targets),
    msg = "'targets' data.frame must contain a 'feature' column."
  )
  assertthat::assert_that(
    "target" %in% names(targets),
    msg = "'targets' data.frame must contain a 'target' column."
  )

  # Calculate the target for the 'climate_layer'.
  # This scales the refugiaTarget by the inverse of the proportion of planning units
  # that are climate-smart. This ensures that the desired refugiaTarget is met
  # specifically within the climate-smart areas.
  total_climate_smart_units <- sum(climateSmartDF$climate_layer, na.rm = TRUE)
  total_planning_units <- nrow(climateSmartDF)

  if (total_planning_units == 0) {
    stop("Input 'climateSmartDF' has no planning units. Cannot assign targets.")
  }

  proportion_climate_smart <- total_climate_smart_units / total_planning_units

  if (proportion_climate_smart == 0) {
    stop("No climate-smart planning units identified. Cannot assign a target to the climate layer.")
  }

  trgt <- refugiaTarget / proportion_climate_smart

  # Create a data frame for the climate layer's target.
  climate_layerDF <- tibble::tribble(
    ~feature, ~target,
    "climate_layer", trgt
  )

  # Combine the original targets with the new climate layer target.
  finalDF <- targets %>%
    dplyr::bind_rows(climate_layerDF)

  return(finalDF)
}

#' @title Run the Feature Climate-Smart Approach
#'
#' @description
#' `splnr_climate_featureApproach()` implements the Feature Approach to
#' climate-smart conservation planning. This involves defining a global
#' "climate-smart" layer and adjusting conservation targets to ensure that
#' a specified proportion of this layer is captured in the solution.
#'
#' @details
#' This function orchestrates the steps for the Feature Approach:
#' 1. **Preprocessing:** It calls `splnr_climate_feature_preprocess()` to
#'    identify a region-wide climate-smart layer based on a percentile cutoff
#'    of the climate metric. This layer is then added as a new binary feature
#'    to your conservation data.
#' 2. **Target Assignment:** It then calls `splnr_climate_feature_assignTargets()`
#'    to calculate and assign new targets. Crucially, a specific `refugiaTarget`
#'    is set for the newly created `climate_layer` feature, ensuring that a
#'    certain proportion of the most climate-resilient areas are included in
#'    the final conservation plan.
#'
#' The output is a list containing the modified features (now including the
#' `climate_layer`) and their corresponding adjusted targets, ready to be used
#' in a `prioritizr` conservation problem.
#'
#' @param features An `sf` object representing conservation features (e.g., species
#'   distribution data).
#' @param metric An `sf` object containing climate metric information. It must
#'   have a column named 'metric' with the climate metric values per Planning Unit.
#' @param targets A `data.frame` with two columns: `feature` (character, listing
#'   the original feature names) and `target` (numeric, the initial conservation
#'   target for each feature as a proportion, e.g., 0.3).
#' @param direction An integer specifying the direction of climate-smartness:
#'   \itemize{
#'     \item `1`: Higher metric values mean more climate-smart areas.
#'     \item `-1`: Lower metric values mean more climate-smart areas.
#'   }
#' @param percentile A numeric value (0-100) representing the cutoff threshold for
#'   determining whether an area is a climate priority area or not. This is applied
#'   globally to the `metric` data. Defaults to `35`.
#' @param refugiaTarget A numeric value (0-1) representing the target proportion
#'   assigned to the overall climate-smart layer. Defaults to `0.3` (30%).
#'
#' @return A `list` with two components:
#'   \itemize{
#'     \item `Features`: An `sf` object containing the binary information per
#'           Planning Unit for each original feature, plus the new `climate_layer`
#'           feature. This is ready to be passed to `prioritizr`.
#'     \item `Targets`: A `data.frame` with the adjusted targets for all features,
#'           including the `climate_layer`. This is also ready for `prioritizr`.
#'   }
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom rlang .data
#' @importFrom sf st_crs
#'
#' @examples
#' \dontrun{
#' # Assuming 'dat_species_bin' and 'dat_clim' are existing sf objects
#' # in your package.
#'
#' # Define initial targets for species features.
#' initial_targets <- dat_species_bin %>%
#'   sf::st_drop_geometry() %>%
#'   colnames() %>%
#'   data.frame() %>%
#'   setNames(c("feature")) %>%
#'   dplyr::mutate(target = 0.3)
#'
#' # Run the Feature Approach where higher climate metric values mean
#' # more climate-smart areas.
#' Feature_Approach_result <- splnr_climate_featureApproach(
#'   features = dat_species_bin,
#'   metric = dat_clim,
#'   targets = initial_targets,
#'   direction = 1, # Example: higher metric values are more climate-smart
#'   percentile = 35,
#'   refugiaTarget = 0.3
#' )
#'
#' # Access the processed features and targets:
#' out_sf_feature <- Feature_Approach_result$Features
#' targets_feature <- Feature_Approach_result$Targets
#'
#' print(head(out_sf_feature))
#' print(head(targets_feature))
#' }
splnr_climate_featureApproach <- function(features,
                                          metric,
                                          targets,
                                          direction,
                                          percentile = 35,
                                          refugiaTarget = 0.3) {

  # Assertions to validate input parameters.
  assertthat::assert_that(
    inherits(features, "sf"),
    msg = "'features' must be an 'sf' object."
  )
  assertthat::assert_that(
    inherits(metric, "sf"),
    msg = "'metric' must be an 'sf' object."
  )
  assertthat::assert_that(
    "metric" %in% names(metric),
    msg = "'metric' sf object must contain a column named 'metric'."
  )
  assertthat::assert_that(
    is.data.frame(targets),
    msg = "'targets' must be a data.frame."
  )
  assertthat::assert_that(
    "feature" %in% names(targets),
    msg = "'targets' data.frame must contain a 'feature' column."
  )
  assertthat::assert_that(
    "target" %in% names(targets),
    msg = "'targets' data.frame must contain a 'target' column."
  )
  assertthat::assert_that(
    direction %in% c(-1, 1),
    msg = "'direction' must be either 1 (higher metric = more climate-smart) or -1 (lower metric = more climate-smart)."
  )
  assertthat::assert_that(
    is.numeric(percentile) && length(percentile) == 1 && percentile >= 0 && percentile <= 100,
    msg = "'percentile' must be a single numeric value between 0 and 100."
  )
  assertthat::assert_that(
    is.numeric(refugiaTarget) && length(refugiaTarget) == 1 && refugiaTarget >= 0 && refugiaTarget <= 1,
    msg = "'refugiaTarget' must be a single numeric value between 0 and 1."
  )
  assertthat::assert_that(
    sf::st_crs(features) == sf::st_crs(metric),
    msg = "CRS of 'features' and 'metric' must be the same."
  )


  # Preprocess features to create the climate_layer.
  featureFeatures <- splnr_climate_feature_preprocess(
    features = features,
    metric = metric,
    direction = direction,
    percentile = percentile
  )

  # Assign targets, including the new climate_layer.
  featureTargets <- splnr_climate_feature_assignTargets(
    targets = targets,
    climateSmartDF = featureFeatures, # Use the preprocessed features for target assignment.
    refugiaTarget = refugiaTarget
  )
  # Return a list containing both the processed features and their adjusted targets.
  return(list(Features = featureFeatures, Targets = featureTargets))
}

  ##### Percentile Approach ####

#' @title Preprocessing for the Percentile Climate-Smart Approach
#'
#' @description
#' This internal function filters the distributions of each conservation feature
#' to include only their occurrences within "climate-smart" areas. These areas
#' are defined by a percentile cutoff of a climate metric applied to each
#' feature's distribution.
#'
#' @details
#' The Percentile Approach to climate-smart conservation ensures that a certain
#' proportion of each feature's occurrence within its most climate-resilient
#' habitats is protected. This preprocessing step identifies these areas.
#'
#' For each feature, the function performs the following:
#' 1. Joins the feature data with the climate metric data.
#' 2. Filters to include only planning units where the feature is present.
#' 3. Calculates the `percentile` cutoff for the climate metric *within that
#'    specific feature's distribution*.
#' 4. Creates a new binary column for each feature (`_filtered`) indicating
#'    planning units where the feature is present AND the climate metric meets
#'    the climate-smart criteria (e.g., top 5% for direction 1, bottom 5% for direction -1).
#'    All other planning units for that feature are set to 0 in this new column.
#'
#' The `direction` parameter defines what constitutes "climate-smart":
#' - `direction = 1`: Higher values of the `metric` are more climate-smart.
#'   The function selects areas with metric values greater than or equal to the
#'   `(100 - percentile)`th quantile of the feature's occupied metric values.
#' - `direction = -1`: Lower values of the `metric` are more climate-smart.
#'   The function selects areas with metric values less than or equal to the
#'   `percentile`th quantile of the feature's occupied metric values.
#'
#' @param features An `sf` object representing conservation features. Each column
#'   (excluding geometry) should typically be a binary representation of a feature's
#'   presence (1) or absence (0) in each Planning Unit.
#' @param metric An `sf` object containing climate metric information. It must
#'   have a column named 'metric' with the climate metric values per Planning Unit.
#' @param percentile A numeric value (0-100) representing the cutoff threshold for
#'   determining whether an area is a climate priority area or not. This is applied
#'   *per feature* to its distribution.
#' @param direction An integer specifying the direction of climate-smartness:
#'   \itemize{
#'     \item `1`: Higher metric values mean more climate-smart.
#'     \item `-1`: Lower metric values mean more climate-smart.
#'   }
#'
#' @return An `sf` dataframe where each column represents an original feature,
#'   but its values are now filtered (`_filtered` suffix implicitly removed in `rename_all`)
#'   to 1 only in Planning Units that are part of its climate-smart percentile.
#'   All other values are 0. The dataframe retains the original geometry.
#' @keywords internal
#' @noRd
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr across bind_cols filter if_else mutate select
#' @importFrom rlang .data sym
#' @importFrom sf st_as_sf st_drop_geometry st_join st_set_geometry
#' @importFrom stats quantile
#' @importFrom stringr str_sub
#' @importFrom tidyselect everything
#'
#' @examples
#' \dontrun{
#' # Assuming 'dat_species_bin' and 'dat_clim' are existing sf objects
#' # in your package.
#'
#' # Example: Filter species distributions to their top 5% most climate-smart areas,
#' # where higher metric values are considered more climate-smart.
#' percentile_preprocessed_data <- splnr_climate_percentile_preprocess(
#'   features = dat_species_bin,
#'   metric = dat_clim,
#'   percentile = 5,
#'   direction = 1
#' )
#' print(percentile_preprocessed_data)
#'
#' # Example: Filter to bottom 10% most climate-smart areas,
#' # where lower metric values are considered more climate-smart.
#' percentile_preprocessed_data_alt <- splnr_climate_percentile_preprocess(
#'   features = dat_species_bin,
#'   metric = dat_clim,
#'   percentile = 10,
#'   direction = -1
#' )
#' print(percentile_preprocessed_data_alt)
#' }
splnr_climate_percentile_preprocess <- function(features,
                                                metric,
                                                percentile,
                                                direction) {
  # Assertions to validate input parameters.
  assertthat::assert_that(
    inherits(features, "sf"),
    msg = "'features' must be an 'sf' object."
  )
  assertthat::assert_that(
    inherits(metric, "sf"),
    msg = "'metric' must be an 'sf' object."
  )
  assertthat::assert_that(
    "metric" %in% names(metric),
    msg = "'metric' sf object must contain a column named 'metric'."
  )
  assertthat::assert_that(
    is.numeric(percentile) && length(percentile) == 1 && percentile >= 0 && percentile <= 100,
    msg = "'percentile' must be a single numeric value between 0 and 100."
  )
  assertthat::assert_that(
    direction %in% c(1, -1),
    msg = "'direction' must be either 1 (higher metric = more climate-smart) or -1 (lower metric = more climate-smart)."
  )
  assertthat::assert_that(
    sf::st_crs(features) == sf::st_crs(metric),
    msg = "CRS of 'features' and 'metric' must be the same."
  )

  # Check for NAs in the 'metric' column and print a warning if found.
  if (any(is.na(metric$metric))) {
    message("Warning: There are some NAs in the metric data. Please check.")
  }

  # Get the list of feature names (excluding geometry).
  spp <- features %>%
    sf::st_drop_geometry() %>%
    names()

  percentileList <- list() # Initialize an empty list to store processed data for each feature.
  for (i in 1:length(spp)) {

    # Select 1 feature at a time from original features data and join with the metric layer.
    df <- features %>%
      dplyr::select(!!rlang::sym(spp[i]),) %>%
      sf::st_join(metric, join = sf::st_equals)

    # Check for NAs in the metric column of the joined data frame for the current feature
    # and print a warning if found.
    if (any(is.na(df$metric))) {
      message(paste0("Warning: NAs found in 'metric' for feature '", spp[i], "'. These will be excluded from percentile calculation."))
    }

    # Filter to select only areas where the current feature is present (value = 1).
    filteredDF <- df %>%
      dplyr::filter(!!rlang::sym(spp[i]) == 1)

    # Handle cases where filteredDF might be empty (feature not present in any unit or only in NAs)
    if (nrow(filteredDF) == 0) {
      warning(paste0("Feature '", spp[i], "' is not present in any Planning Unit with valid metric data. Skipping percentile calculation for this feature."))
      # Create an empty df with expected column for binding later
      temp_df <- df %>%
        sf::st_drop_geometry() %>% # Drop geometry for consistency with other iterations
        dplyr::mutate(V1 = 0, V2 = 0) %>% # Add V1 and V2 columns with 0 values
        dplyr::mutate(!!rlang::sym(paste0(spp[i], "_filtered")) := 0) %>%
        dplyr::select(!!rlang::sym(paste0(spp[i], "_filtered")))
      percentileList[[i]] <- temp_df
      next # Skip to the next iteration of the loop
    }


    # Convert percentile to proportion.
    prct <- percentile / 100
    # Calculate the quantile cutoff within the current feature's distribution.
    qntl <- stats::quantile(filteredDF$metric, prct, na.rm = TRUE)[[1]]

    # Apply filtering based on direction to create temporary V1 and V2 columns.
    if (direction == 1) {
      if (i == 1) { # Only print for the first iteration.
        message("Higher values mean more climate-smart areas.")
      }
      df1 <- df %>%
        dplyr::mutate(
          V1 = ifelse(.data$metric >= qntl, yes = 1, no = 0), # 1 if metric is in climate-smart percentile, 0 otherwise.
          V2 = ifelse(!!rlang::sym(spp[i]) == 1, yes = 1, no = 0) # 1 if feature is present, 0 otherwise.
        )
    } else if (direction == -1) {
      if (i == 1) { # Only print for the first iteration.
        message("Lower values mean more climate-smart areas.")
      }
      df1 <- df %>%
        dplyr::mutate(
          V1 = ifelse(.data$metric <= qntl, yes = 1, no = 0), # 1 if metric is in climate-smart percentile, 0 otherwise.
          V2 = ifelse(!!rlang::sym(spp[i]) == 1, yes = 1, no = 0) # 1 if feature is present, 0 otherwise.
        )
    } else {
      # This case should ideally be caught by assertthat, but included as a fallback.
      if (i == 1) {
        stop("Please enter a valid direction: either 1 or -1.")
      }
    }

    # Drop geometry for subsequent bind_cols operations if not the first iteration.
    # We will re-add the geometry from the original 'features' object at the end.
    if (i > 1 && "geometry" %in% names(df1)){
      df1 <- df1 %>% sf::st_drop_geometry()
    } else if (i == 1 && "geometry" %in% names(df1)){
      # For the first iteration, keep geometry if it exists, and make sure it's the right one.
      # This block ensures that the geometry is the actual sf geometry and not just a column name.
      # The do.call(dplyr::bind_cols, ...) will handle the geometry column from the first element.
      df1 <- df1 %>% dplyr::select(-"geometry") # Temporarily remove geometry
    }

    # Calculate the final filtered feature layer (1 if present in climate-smart percentile, 0 otherwise).
    percentileList[[i]] <- df1 %>%
      dplyr::mutate(!!rlang::sym(paste0(spp[i], "_filtered")) := .data$V1 * .data$V2) %>% # V1*V2 is 1 if area is within percentile AND feature is present.
      dplyr::select(!!rlang::sym(paste0(spp[i], "_filtered"))) # Select only the filtered column.
  }

  # Combine all processed data frames into a single dataframe.
  # Rename columns to remove the "_filtered" suffix for easier use.

  resultDF <- do.call(dplyr::bind_cols, percentileList) %>%
    dplyr::rename_all(~ stringr::str_remove(.x, "_filtered$")) %>% # Remove "_filtered" suffix.
    # Re-add the geometry column from the original features object.
    sf::st_set_geometry(features$geometry) %>%
    dplyr::select(tidyselect::everything()) %>% # Select all columns, ensuring order.
    sf::st_as_sf() # Convert back to sf object.

  return(resultDF)
}

#' @title Assign Targets for Percentile Climate-Smart Approach
#'
#' @description
#' This internal function calculates and assigns conservation targets when using
#' the Percentile Approach to climate-smart spatial planning. It adjusts targets
#' to account for the filtering of feature distributions to only their
#' climate-smart areas.
#'
#' @details
#' This function is a key component of the `splnr_climate_percentileApproach()`.
#' It takes the original conservation targets for features and adjusts them based
#' on how much of each feature's total occurrence was retained after filtering
#' for climate-smart areas in `splnr_climate_percentile_preprocess()`.
#'
#' The target adjustment logic is as follows:
#' 1. Calculates the "original" total number of planning units where each feature is present.
#' 2. Calculates the "filtered" total number of planning units where each feature
#'    is present *within its climate-smart percentile* (from `climateSmartDF`).
#' 3. Determines the `proportion` of the filtered presence relative to the
#'    original presence for each feature (`filtered / original`).
#' 4. The new target for each feature is then calculated by dividing its original
#'    target by this `proportion`. This scales up the target to ensure that the
#'    original conservation goal is still met, but specifically within the
#'    identified climate-smart areas.
#' 5. Targets are capped at `1` (100%) to prevent values greater than 1.
#'
#' @param features An `sf` object representing original conservation features.
#'   This is used to determine the total (unfiltered) presence of each feature.
#' @param targets A `data.frame` with two columns: `feature` (character, listing
#'   the original feature names) and `target` (numeric, the initial conservation
#'   target for each feature as a proportion, e.g., 0.3).
#' @param climateSmartDF An `sf` object (or data frame) produced by
#'   `splnr_climate_percentile_preprocess()`. This dataframe contains the
#'   features filtered to their climate-smart areas.
#'
#' @return A `data.frame` with two columns: `feature` (character, with original
#'   feature names) and `target` (the newly calculated, adjusted targets).
#' @keywords internal
#' @noRd
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr across everything left_join mutate select summarize
#' @importFrom rlang .data
#' @importFrom sf st_drop_geometry
#' @importFrom tibble as_tibble
#' @importFrom tidyr replace_na pivot_longer
#'
#' @examples
#' \dontrun{
#' # Assuming 'dat_species_bin' and 'dat_clim' are existing sf objects.
#'
#' # Define initial targets for species features.
#' initial_targets <- dat_species_bin %>%
#'   sf::st_drop_geometry() %>%
#'   colnames() %>%
#'   data.frame() %>%
#'   setNames(c("feature")) %>%
#'   dplyr::mutate(target = 0.3)
#'
#' # Preprocess features to get climate-smart filtered areas.
#' preprocessed_features_percentile <- splnr_climate_percentile_preprocess(
#'   features = dat_species_bin,
#'   metric = dat_clim,
#'   percentile = 35,
#'   direction = 1
#' )
#'
#' # Assign targets for the percentile approach.
#' percentile_assigned_targets <- splnr_climate_percentile_assignTargets(
#'   features = dat_species_bin, # Original features for 'original' counts
#'   targets = initial_targets,
#'   climateSmartDF = preprocessed_features_percentile
#' )
#' print(percentile_assigned_targets)
#' }
splnr_climate_percentile_assignTargets <- function(features,
                                                   climateSmartDF,
                                                   targets) {
  # Assertions to validate input parameters.
  assertthat::assert_that(
    inherits(features, "sf"),
    msg = "'features' must be an 'sf' object."
  )
  assertthat::assert_that(
    is.data.frame(targets),
    msg = "'targets' must be a data.frame."
  )
  assertthat::assert_that(
    "feature" %in% names(targets),
    msg = "'targets' data.frame must contain a 'feature' column."
  )
  assertthat::assert_that(
    "target" %in% names(targets),
    msg = "'targets' data.frame must contain a 'target' column."
  )
  assertthat::assert_that(
    inherits(climateSmartDF, "data.frame"), # can be sf or just df after dropping geom
    msg = "'climateSmartDF' must be a data.frame (or sf object)."
  )
  # Check that column names in climateSmartDF match expected original feature names
  # (after removing any potential _filtered suffix from the internal preprocessing)
  original_feature_names <- features %>% sf::st_drop_geometry() %>% names()
  filtered_feature_names <- climateSmartDF %>% sf::st_drop_geometry() %>% names()
  assertthat::assert_that(
    all(original_feature_names %in% filtered_feature_names),
    msg = "Feature names in 'climateSmartDF' do not match original feature names in 'features'."
  )


  # Get the total number of planning units where each feature is originally present.
  # Suppress messages from dplyr::summarize with `suppressMessages`.
  suppressMessages({
    df <- features %>%
      sf::st_drop_geometry() %>% # Drop geometry for aggregation.
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ tidyr::replace_na(.x, 0))) %>% # Replace NAs with 0.
      tibble::as_tibble() %>% # Convert to tibble for consistent behavior.
      dplyr::summarize(dplyr::across(dplyr::everything(), sum)) %>% # Sum up occurrences for each feature.
      tidyr::pivot_longer(tidyselect::everything(), names_to = "feature", values_to = "original") %>% # Pivot to long format.
      dplyr::left_join(targets, by = "feature") # Join with original targets.

    # Get the total number of planning units selected using the climate-smart filtering.
    df1 <- climateSmartDF %>%
      sf::st_drop_geometry() %>% # Drop geometry for aggregation.
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ tidyr::replace_na(.x, 0))) %>% # Replace NAs with 0.
      tibble::as_tibble() %>% # Convert to tibble.
      dplyr::summarize(dplyr::across(dplyr::everything(), sum)) %>% # Sum up occurrences of filtered features.
      tidyr::pivot_longer(tidyselect::everything(), names_to = "feature", values_to = "filtered") # Pivot to long format.

    # Join the original counts with the filtered counts, calculate proportion, and adjust targets.
    df <- df %>%
      dplyr::left_join(df1, by = "feature") %>% # Join filtered counts.
      dplyr::mutate(
        # Calculate the proportion of the filtered presence relative to the original presence.
        proportion = dplyr::if_else(.data$original > 0, .data$filtered / .data$original, 0),
        # Calculate new target: original target divided by proportion.
        # Handle cases where proportion might be zero to avoid division by zero.
        target = dplyr::if_else(.data$proportion > 0, .data$target / .data$proportion, .data$target)
      ) %>%
      dplyr::select("feature", "target") %>% # Select only feature and adjusted target.
      dplyr::mutate(target = ifelse(.data$target > 1, 1, .data$target)) # Cap targets at 1 (100%).
  })

  return(df)
}

#' @title Run the Percentile Climate-Smart Approach
#'
#' @description
#' `splnr_climate_percentileApproach()` implements the Percentile Approach to
#' climate-smart conservation planning. This involves filtering features to
#' their most climate-resilient areas and adjusting their conservation targets
#' to account for this reduced feature distribution.
#'
#' @details
#' This function orchestrates the steps for the Percentile Approach:
#' 1. **Preprocessing:** It calls `splnr_climate_percentile_preprocess()` to
#'    identify, for each feature, its occurrences within the most climate-resilient
#'    `percentile` of its distribution based on a climate metric. This effectively
#'    "filters" the feature data to only include its climate-smart components.
#' 2. **Target Assignment:** It then calls `splnr_climate_percentile_assignTargets()`
#'    to calculate and assign new targets for these filtered features. The targets
#'    are scaled up to ensure that the original conservation goals are still met,
#'    but specifically by selecting areas from the climate-smart portions of the
#'    features' distributions.
#'
#' The output is a list containing the modified features (filtered to their
#' climate-smart occurrences) and their corresponding adjusted targets, ready
#' to be used in a `prioritizr` conservation problem.
#'
#' @param features An `sf` object representing conservation features (e.g., species
#'   distribution data).
#' @param metric An `sf` object containing climate metric information. It must
#'   have a column named 'metric' with the climate metric values per Planning Unit.
#' @param targets A `data.frame` with two columns: `feature` (character, listing
#'   the original feature names) and `target` (numeric, the initial conservation
#'   target for each feature as a proportion, e.g., 0.3).
#' @param direction An integer specifying the direction of climate-smartness:
#'   \itemize{
#'     \item `1`: Higher metric values mean more climate-smart areas.
#'     \item `-1`: Lower metric values mean more climate-smart areas.
#'   }
#' @param percentile A numeric value (0-100) representing the cutoff threshold for
#'   determining whether an area is a climate priority area or not. This is applied
#'   *per feature* to its distribution. Defaults to `35`.
#'
#' @return A `list` with two components:
#'   \itemize{
#'     \item `Features`: An `sf` object containing the binary information per
#'           Planning Unit for each feature, now filtered to include only its
#'           climate-smart occurrences. This is ready to be passed to `prioritizr`.
#'     \item `Targets`: A `data.frame` with the adjusted targets for the
#'           filtered features. This is also ready for `prioritizr`.
#'   }
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate select
#' @importFrom rlang .data
#' @importFrom sf st_crs
#'
#' @examples
#' \dontrun{
#' # Assuming 'dat_species_bin' and 'dat_clim' are existing sf objects
#' # in your package.
#'
#' # Define initial targets for species features.
#' initial_targets <- dat_species_bin %>%
#'   sf::st_drop_geometry() %>%
#'   colnames() %>%
#'   data.frame() %>%
#'   setNames(c("feature")) %>%
#'   dplyr::mutate(target = 0.3)
#'
#' # Run the Percentile Approach where higher climate metric values mean
#' # more climate-smart areas.
#' Percentile_Approach_result <- splnr_climate_percentileApproach(
#'   features = dat_species_bin,
#'   metric = dat_clim,
#'   targets = initial_targets,
#'   direction = 1, # Example: higher metric values are more climate-smart
#'   percentile = 35
#' )
#'
#' # Access the processed features and targets:
#' out_sf_percentile <- Percentile_Approach_result$Features
#' targets_percentile <- Percentile_Approach_result$Targets
#'
#' print(head(out_sf_percentile))
#' print(head(targets_percentile))
#' }
splnr_climate_percentileApproach <- function(features,
                                             metric,
                                             targets,
                                             direction,
                                             percentile = 35) {

  # Assertions to validate input parameters.
  assertthat::assert_that(
    inherits(features, "sf"),
    msg = "'features' must be an 'sf' object."
  )
  assertthat::assert_that(
    inherits(metric, "sf"),
    msg = "'metric' must be an 'sf' object."
  )
  assertthat::assert_that(
    "metric" %in% names(metric),
    msg = "'metric' sf object must contain a column named 'metric'."
  )
  assertthat::assert_that(
    is.data.frame(targets),
    msg = "'targets' must be a data.frame."
  )
  assertthat::assert_that(
    "feature" %in% names(targets),
    msg = "'targets' data.frame must contain a 'feature' column."
  )
  assertthat::assert_that(
    "target" %in% names(targets),
    msg = "'targets' data.frame must contain a 'target' column."
  )
  assertthat::assert_that(
    direction %in% c(-1, 1),
    msg = "'direction' must be either 1 (higher metric = more climate-smart) or -1 (lower metric = more climate-smart)."
  )
  assertthat::assert_that(
    is.numeric(percentile) && length(percentile) == 1 && percentile >= 0 && percentile <= 100,
    msg = "'percentile' must be a single numeric value between 0 and 100."
  )
  assertthat::assert_that(
    sf::st_crs(features) == sf::st_crs(metric),
    msg = "CRS of 'features' and 'metric' must be the same."
  )

  # Preprocess features to filter them to their climate-smart areas.
  percentileFeatures <- splnr_climate_percentile_preprocess(
    features = features,
    metric = metric,
    direction = direction,
    percentile = percentile
  )

  # Assign adjusted targets for the filtered features.
  percentileTargets <- splnr_climate_percentile_assignTargets(
    features = features, # Original features are needed to get original counts.
    targets = targets,
    climateSmartDF = percentileFeatures # Use the preprocessed features for target assignment.
  )
  # Return a list containing both the processed features and their adjusted targets.
  return(list(Features = percentileFeatures, Targets = percentileTargets))
}
