# Internal helper ---------------------------------------------------------

# Converts a user-facing `percentile` (0–100, meaning "top X% of areas") and
# a `direction` flag into the quantile fraction passed to `stats::quantile()`.
#
# Convention (shared by all three climate approaches):
#   percentile = 35, direction =  1  →  keep areas >= 65th percentile (top 35%)
#   percentile = 35, direction = -1  →  keep areas <= 35th percentile (bottom 35%)
#
# @param percentile Numeric scalar 0–100.
# @param direction  1 (higher = more climate-smart) or -1 (lower = more climate-smart).
# @return Numeric scalar in [0, 1] suitable for `stats::quantile()`.
.splnr_climate_prct <- function(percentile, direction) {
  if (direction == 1) (100 - percentile) / 100 else percentile / 100
}

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
#' 1. Joins the feature data with the climate metric data (once, before the loop).
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
#' @importFrom dplyr across bind_cols if_else mutate select summarize
#' @importFrom rlang .data
#' @importFrom sf st_as_sf st_drop_geometry st_geometry st_set_geometry
#' @importFrom stats quantile
#' @importFrom tidyr replace_na
#'
#' @examples
#' \dontrun{
#' out_sf_cs_areas <- splnr_climate_priorityArea_preprocess(
#'   features = dat_species_bin,
#'   percentile = 5,
#'   metric = dat_clim,
#'   direction = 1
#' )
#' }
splnr_climate_priorityArea_preprocess <- function(features,
                                                  percentile,
                                                  metric,
                                                  direction,
                                                  metric_col = "metric") {

  assertthat::assert_that(
    inherits(features, "sf"),
    msg = "'features' must be an 'sf' object."
  )
  assertthat::assert_that(
    inherits(metric, "sf"),
    msg = "'metric' must be an 'sf' object."
  )
  assertthat::assert_that(
    is.character(metric_col) && length(metric_col) == 1,
    msg = "'metric_col' must be a single character string."
  )
  assertthat::assert_that(
    metric_col %in% names(metric),
    msg = paste0("'metric' sf object must contain a column named '", metric_col, "'.")
  )

  # Normalise: rename the user-supplied column to the internal name "metric"
  # so the rest of this function can use a single consistent name.
  # This is a no-op when metric_col == "metric" (the default).
  if (metric_col != "metric") {
    metric <- dplyr::rename(metric, "metric" = !!rlang::sym(metric_col))
  }
  assertthat::assert_that(
    is.numeric(percentile) && length(percentile) == 1 &&
      percentile >= 0 && percentile <= 100,
    msg = "'percentile' must be a single numeric value between 0 and 100."
  )
  assertthat::assert_that(
    direction %in% c(1, -1),
    msg = "'direction' must be either 1 or -1."
  )
  assertthat::assert_that(
    sf::st_crs(features) == sf::st_crs(metric),
    msg = "CRS of 'features' and 'metric' must be the same."
  )

  # Feature column names (excluding geometry).
  spp <- sf::st_drop_geometry(features) %>% names()

  metric_vals_df <- sf::st_drop_geometry(dplyr::select(metric, "metric"))
  metric_vals_df[[".row_id"]] <- seq_len(nrow(metric_vals_df))

  joined_df <- sf::st_drop_geometry(features)
  joined_df[[".row_id"]] <- seq_len(nrow(joined_df))
  joined_df <- dplyr::left_join(joined_df, metric_vals_df, by = ".row_id")
  joined_df[[".row_id"]] <- NULL   # remove key column; no trace in output

  if (any(is.na(joined_df$metric))) {
    warning(
      "NAs present in the metric data. ",
      "These will be excluded from per-feature quantile calculations."
    )
  }

  # Percentile fraction is constant across all features; compute once.
  prct <- .splnr_climate_prct(percentile, direction)

  result_list <- vector("list", length(spp))

  for (i in seq_along(spp)) {
    feat_col    <- spp[i]
    feat_vals   <- joined_df[[feat_col]]
    metric_vals <- joined_df[["metric"]]

    # Indices where the feature is present AND metric is not NA.
    present_idx <- which(feat_vals == 1 & !is.na(metric_vals))

    if (length(present_idx) == 0) {
      warning(
        "Feature '", feat_col, "' has no planning units with valid metric ",
        "data. CS and NCS columns will be all zeros."
      )
      result_list[[i]] <- data.frame(
        a = integer(nrow(joined_df)),
        b = integer(nrow(joined_df))
      )
      names(result_list[[i]]) <- c(
        paste0(feat_col, "_CS"),
        paste0(feat_col, "_NCS")
      )
      next
    }

    # Quantile cutoff calculated only over the feature's occupied planning units.
    qntl <- stats::quantile(metric_vals[present_idx], prct, na.rm = TRUE)[[1]]

    # Binary climate-smart indicator across ALL planning units.
    climate_smart <- if (direction == 1) {
      dplyr::if_else(!is.na(metric_vals) & metric_vals >= qntl, 1L, 0L)
    } else {
      dplyr::if_else(!is.na(metric_vals) & metric_vals <= qntl, 1L, 0L)
    }

    # CS  = feature present AND in climate-smart zone.
    # NCS = feature present AND NOT in climate-smart zone.
    # Both derived in one pass; no second loop or second join needed.
    cs_col  <- dplyr::if_else(feat_vals == 1, climate_smart,       0L)
    ncs_col <- dplyr::if_else(feat_vals == 1, 1L - climate_smart,  0L)

    result_list[[i]] <- data.frame(cs_col, ncs_col)
    names(result_list[[i]]) <- c(
      paste0(feat_col, "_CS"),
      paste0(feat_col, "_NCS")
    )
  }

  # Bind all CS/NCS columns and restore the original geometry.
  out <- dplyr::bind_cols(result_list) %>%
    sf::st_set_geometry(sf::st_geometry(features)) %>%
    sf::st_as_sf()

  # Sanity check: output must have the same number of rows as the input.
  # This catches any future regression where a join produces duplicate rows.
  assertthat::assert_that(
    nrow(out) == nrow(features),
    msg = paste0(
      "Internal error in splnr_climate_priorityArea_preprocess(): output has ",
      nrow(out), " rows but 'features' has ", nrow(features), " rows. ",
      "Please report this as a bug."
    )
  )

  return(out)
}


#' @title Assign Targets for Climate Priority Area Approach
#'
#' @description
#' This internal function calculates and assigns conservation targets for features
#' when using the Climate Priority Area (CPA) approach. It differentiates targets
#' for climate-smart (CS) and non-climate-smart (NCS) areas.
#'
#' @details
#' For each feature the function:
#' 1. Calculates the total planning units occupied (CS + NCS).
#' 2. Determines the proportion of the feature's presence in CS areas.
#' 3. If the CS proportion already exceeds the original target, the CS target is
#'    scaled down proportionally and the NCS target is set to 0.
#' 4. Otherwise the CS target is set to `refugiaTarget` and the remaining
#'    shortfall is assigned to NCS (capped at 1).
#'
#' @param targets A `data.frame` with columns `feature` and `target`.
#' @param climateSmartDF An `sf` object produced by
#'   `splnr_climate_priorityArea_preprocess()`.
#' @param refugiaTarget A numeric value (0-1). Defaults to `1`.
#'
#' @return A `data.frame` with columns `feature` and `target` for each
#'   `_CS` and `_NCS` component.
#' @keywords internal
#' @noRd
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr across bind_rows mutate pull select summarize
#' @importFrom rlang .data
#' @importFrom sf st_drop_geometry
#' @importFrom tidyr replace_na pivot_longer
#'
#' @examples
#' \dontrun{
#' cpa_targets <- splnr_climate_priorityArea_assignTargets(
#'   targets        = initial_targets,
#'   climateSmartDF = preprocessed_features,
#'   refugiaTarget  = 1
#' )
#' }
splnr_climate_priorityArea_assignTargets <- function(targets,
                                                     climateSmartDF,
                                                     refugiaTarget = 1) {

  assertthat::assert_that(
    is.data.frame(targets),
    msg = "'targets' must be a data.frame."
  )
  assertthat::assert_that(
    "feature" %in% names(targets),
    msg = "'targets' must contain a 'feature' column."
  )
  assertthat::assert_that(
    "target" %in% names(targets),
    msg = "'targets' must contain a 'target' column."
  )
  assertthat::assert_that(
    inherits(climateSmartDF, "data.frame"),
    msg = "'climateSmartDF' must be a data.frame or sf object."
  )
  assertthat::assert_that(
    is.numeric(refugiaTarget) && length(refugiaTarget) == 1 &&
      refugiaTarget >= 0 && refugiaTarget <= 1,
    msg = "'refugiaTarget' must be a single numeric value between 0 and 1."
  )
  assertthat::assert_that(
    any(grepl("_CS$",  names(climateSmartDF))) &&
      any(grepl("_NCS$", names(climateSmartDF))),
    msg = paste0(
      "'climateSmartDF' must contain '_CS' and '_NCS' columns ",
      "(output of splnr_climate_priorityArea_preprocess)."
    )
  )

  spp <- dplyr::pull(targets, "feature")

  # Sum each CS/NCS column to get planning-unit counts per component.
  featDF <- climateSmartDF %>%
    sf::st_drop_geometry() %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ tidyr::replace_na(.x, 0))) %>%
    dplyr::summarize(dplyr::across(dplyr::everything(), sum)) %>%
    tidyr::pivot_longer(
      tidyselect::everything(),
      names_to  = "feature",
      values_to = "planunit"
    )

  finalList <- vector("list", length(spp))
  skipped   <- 0L

  for (i in seq_along(spp)) {
    feat     <- spp[i]
    cs_name  <- paste0(feat, "_CS")
    ncs_name <- paste0(feat, "_NCS")

    trgt <- targets[targets$feature == feat, "target"]

    # Use exact equality (not str_ends) to avoid substring-matching bugs
    # where e.g. "fish" would match "bluefish".
    row_cs  <- featDF[featDF$feature == cs_name,  , drop = FALSE]
    row_ncs <- featDF[featDF$feature == ncs_name, , drop = FALSE]

    if (nrow(row_cs) == 0 || nrow(row_ncs) == 0) {
      warning(
        "Could not find CS or NCS counts for feature '", feat,
        "'. Skipping target assignment."
      )
      skipped <- skipped + 1L
      next
    }

    n_cs  <- row_cs[["planunit"]]
    n_ncs <- row_ncs[["planunit"]]
    total <- n_cs + n_ncs

    if (total == 0) {
      warning("Feature '", feat, "' has zero planning units. Targets set to 0.")
      finalList[[i]] <- data.frame(
        feature = c(cs_name, ncs_name),
        target  = c(0, 0)
      )
      next
    }

    prop_cs  <- n_cs  / total
    prop_ncs <- n_ncs / total

    if (prop_cs > trgt) {
      # All required representation can be met within CS areas alone.
      targetCS  <- trgt / prop_cs
      targetNCS <- 0
    } else {
      targetCS <- refugiaTarget

      if (prop_ncs == 0) {
        # No NCS units exist; NCS target is meaningless.
        targetNCS <- 0
      } else {
        # Assign the shortfall to NCS, capped at 1 for consistency with the
        # percentile approach.
        targetNCS <- min((trgt - prop_cs) / prop_ncs, 1)
      }
    }

    finalList[[i]] <- data.frame(
      feature = c(cs_name, ncs_name),
      target  = c(targetCS, targetNCS)
    )
  }

  finalDF <- dplyr::bind_rows(finalList)

  # Sanity check: each non-skipped feature should produce exactly one _CS and
  # one _NCS row.  Skipped features (warned above) are excluded from the count.
  expected_rows <- 2L * (nrow(targets) - skipped)
  assertthat::assert_that(
    nrow(finalDF) == expected_rows,
    msg = paste0(
      "Internal error in splnr_climate_priorityArea_assignTargets(): output has ",
      nrow(finalDF), " rows but expected ", expected_rows,
      " (2 per non-skipped feature). Please report this as a bug."
    )
  )

  return(finalDF)
}


#' @title Run the Climate Priority Area (CPA) Approach
#'
#' @description
#' `splnr_climate_priorityAreaApproach()` implements the Climate Priority Area
#' approach by splitting conservation features into climate-smart (CS) and
#' non-climate-smart (NCS) components and adjusting their targets accordingly.
#'
#' @details
#' This function orchestrates two steps:
#' 1. **Preprocessing** via `splnr_climate_priorityArea_preprocess()`.
#' 2. **Target Assignment** via `splnr_climate_priorityArea_assignTargets()`.
#'
#' @param features An `sf` object of conservation features (binary presence/absence).
#' @param metric An `sf` object with a column named `metric`.
#' @param targets A `data.frame` with columns `feature` and `target`.
#' @param direction `1` (higher = more climate-smart) or `-1` (lower = more climate-smart).
#' @param percentile Numeric (0-100). Defaults to `5`.
#' @param refugiaTarget Numeric (0-1). Defaults to `1`.
#' @param metric_col A single character string giving the name of the column in
#'   `metric` that contains the climate metric values. Defaults to `"metric"` for
#'   backwards compatibility. Use this argument when your climate data column has
#'   a different name (e.g. `"sst_trend"`).
#'
#' @return A `list` with:
#'   \itemize{
#'     \item `Features`: `sf` object with `_CS` and `_NCS` columns per feature.
#'     \item `Targets`: `data.frame` with adjusted targets.
#'   }
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom sf st_crs
#'
#' @examples
#' \dontrun{
#' initial_targets <- dat_species_bin %>%
#'   sf::st_drop_geometry() %>%
#'   colnames() %>%
#'   data.frame() %>%
#'   setNames(c("feature")) %>%
#'   dplyr::mutate(target = 0.3)
#'
#' CPA_result <- splnr_climate_priorityAreaApproach(
#'   features      = dat_species_bin,
#'   metric        = dat_clim,
#'   targets       = initial_targets,
#'   direction     = -1,
#'   percentile    = 5,
#'   refugiaTarget = 1
#' )
#' out_sf_cpa    <- CPA_result$Features
#' targets_cpa   <- CPA_result$Targets
#' }
splnr_climate_priorityAreaApproach <- function(features,
                                               metric,
                                               targets,
                                               direction,
                                               percentile = 5,
                                               refugiaTarget = 1,
                                               metric_col = "metric") {

  assertthat::assert_that(
    inherits(features, "sf"),
    msg = "'features' must be an 'sf' object."
  )
  assertthat::assert_that(
    inherits(metric, "sf"),
    msg = "'metric' must be an 'sf' object."
  )
  assertthat::assert_that(
    is.character(metric_col) && length(metric_col) == 1,
    msg = "'metric_col' must be a single character string."
  )
  assertthat::assert_that(
    metric_col %in% names(metric),
    msg = paste0("'metric' sf object must contain a column named '", metric_col, "'.")
  )
  assertthat::assert_that(
    is.data.frame(targets),
    msg = "'targets' must be a data.frame."
  )
  assertthat::assert_that(
    "feature" %in% names(targets),
    msg = "'targets' must contain a 'feature' column."
  )
  assertthat::assert_that(
    "target" %in% names(targets),
    msg = "'targets' must contain a 'target' column."
  )
  assertthat::assert_that(
    direction %in% c(-1, 1),
    msg = "'direction' must be either 1 or -1."
  )
  assertthat::assert_that(
    is.numeric(percentile) && length(percentile) == 1 &&
      percentile >= 0 && percentile <= 100,
    msg = "'percentile' must be a single numeric value between 0 and 100."
  )
  assertthat::assert_that(
    is.numeric(refugiaTarget) && length(refugiaTarget) == 1 &&
      refugiaTarget >= 0 && refugiaTarget <= 1,
    msg = "'refugiaTarget' must be a single numeric value between 0 and 1."
  )
  assertthat::assert_that(
    sf::st_crs(features) == sf::st_crs(metric),
    msg = "CRS of 'features' and 'metric' must be the same."
  )

  CPAFeatures <- splnr_climate_priorityArea_preprocess(
    features   = features,
    metric     = metric,
    direction  = direction,
    percentile = percentile,
    metric_col = metric_col
  )

  CPATargets <- splnr_climate_priorityArea_assignTargets(
    targets        = targets,
    climateSmartDF = CPAFeatures,
    refugiaTarget  = refugiaTarget
  )

  # Sanity check: Features must have the same number of rows as the input.
  assertthat::assert_that(
    nrow(CPAFeatures) == nrow(features),
    msg = paste0(
      "Internal error in splnr_climate_priorityAreaApproach(): Features has ",
      nrow(CPAFeatures), " rows but 'features' has ", nrow(features), " rows. ",
      "Please report this as a bug."
    )
  )
  # Sanity check: Targets must have 2 rows per original feature (_CS + _NCS).
  assertthat::assert_that(
    nrow(CPATargets) == 2L * nrow(targets),
    msg = paste0(
      "Internal error in splnr_climate_priorityAreaApproach(): Targets has ",
      nrow(CPATargets), " rows but expected ", 2L * nrow(targets),
      " (2 per feature). Please report this as a bug."
    )
  )

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
#' The Feature Approach identifies a region-wide climate-smart layer and adds it
#' as a new binary feature column (`climate_layer`) to the features data.
#'
#' - `direction = 1`: areas with metric >= `percentile`th quantile are climate-smart.
#' - `direction = -1`: areas with metric <= `percentile`th quantile are climate-smart.
#'
#' @param features An `sf` object of conservation features.
#' @param metric An `sf` object with a column named `metric`.
#' @param percentile Numeric (0-100).
#' @param direction `1` or `-1`.
#'
#' @return The input `features` `sf` object with an additional binary
#'   `climate_layer` column.
#' @keywords internal
#' @noRd
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr if_else mutate select
#' @importFrom rlang .data
#' @importFrom sf st_crs
#' @importFrom stats quantile
#'
#' @examples
#' \dontrun{
#' feature_preprocessed <- splnr_climate_feature_preprocess(
#'   features   = dat_species_bin,
#'   percentile = 5,
#'   metric     = dat_clim,
#'   direction  = 1
#' )
#' }
splnr_climate_feature_preprocess <- function(features,
                                             percentile,
                                             metric,
                                             direction,
                                             metric_col = "metric") {

  assertthat::assert_that(
    inherits(features, "sf"),
    msg = "'features' must be an 'sf' object."
  )
  assertthat::assert_that(
    inherits(metric, "sf"),
    msg = "'metric' must be an 'sf' object."
  )
  assertthat::assert_that(
    is.character(metric_col) && length(metric_col) == 1,
    msg = "'metric_col' must be a single character string."
  )
  assertthat::assert_that(
    metric_col %in% names(metric),
    msg = paste0("'metric' sf object must contain a column named '", metric_col, "'.")
  )

  # Normalise: rename the user-supplied column to the internal name "metric"
  # so the rest of this function can use a single consistent name.
  # This is a no-op when metric_col == "metric" (the default).
  if (metric_col != "metric") {
    metric <- dplyr::rename(metric, "metric" = !!rlang::sym(metric_col))
  }
  assertthat::assert_that(
    is.numeric(percentile) && length(percentile) == 1 &&
      percentile >= 0 && percentile <= 100,
    msg = "'percentile' must be a single numeric value between 0 and 100."
  )
  assertthat::assert_that(
    direction %in% c(1, -1),
    msg = "'direction' must be either 1 or -1."
  )
  assertthat::assert_that(
    sf::st_crs(features) == sf::st_crs(metric),
    msg = "CRS of 'features' and 'metric' must be the same."
  )

  if (any(is.na(metric$metric))) {
    warning(
      "NAs present in the metric data. ",
      "These will be excluded from the percentile calculation."
    )
  }

  prct <- .splnr_climate_prct(percentile, direction)
  qntl <- stats::quantile(metric$metric, prct, na.rm = TRUE)[[1]]

  if (direction == 1) {
    df <- metric %>%
      dplyr::mutate(
        climate_layer = dplyr::if_else(.data$metric >= qntl, 1L, 0L)
      )
  } else {
    df <- metric %>%
      dplyr::mutate(
        climate_layer = dplyr::if_else(.data$metric <= qntl, 1L, 0L)
      )
  }

  climateSmartDF <- dplyr::select(df, "climate_layer")

  climate_col <- sf::st_drop_geometry(climateSmartDF)
  climate_col[[".row_id"]] <- seq_len(nrow(climate_col))

  features_df <- sf::st_drop_geometry(features)
  features_df[[".row_id"]] <- seq_len(nrow(features_df))
  features_df <- dplyr::left_join(features_df, climate_col, by = ".row_id")
  features_df[[".row_id"]] <- NULL   # remove key column; no trace in output

  features_out <- sf::st_set_geometry(features_df, sf::st_geometry(features)) %>%
    sf::st_as_sf()

  # Sanity check: output must have the same number of rows as the input.
  assertthat::assert_that(
    nrow(features_out) == nrow(features),
    msg = paste0(
      "Internal error in splnr_climate_feature_preprocess(): output has ",
      nrow(features_out), " rows but 'features' has ", nrow(features), " rows. ",
      "Please report this as a bug."
    )
  )

  return(features_out)
}


#' @title Assign Targets for Feature Climate-Smart Approach
#'
#' @description
#' This internal function calculates and assigns conservation targets when using
#' the Feature Approach to climate-smart spatial planning.
#'
#' @details
#' The target for the `climate_layer` is `refugiaTarget / proportion_climate_smart`,
#' which scales the refugia target relative to the fraction of planning units
#' that are climate-smart.
#'
#' @param climateSmartDF An `sf` object with a `climate_layer` column.
#' @param refugiaTarget Numeric (0-1).
#' @param targets A `data.frame` with columns `feature` and `target`.
#'
#' @return A `data.frame` with columns `feature` and `target`, including a row
#'   for `climate_layer`.
#' @keywords internal
#' @noRd
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows
#' @importFrom tibble tribble
#'
#' @examples
#' \dontrun{
#' feature_targets <- splnr_climate_feature_assignTargets(
#'   climateSmartDF = preprocessed_features,
#'   refugiaTarget  = 0.3,
#'   targets        = initial_targets
#' )
#' }
splnr_climate_feature_assignTargets <- function(climateSmartDF,
                                                refugiaTarget,
                                                targets) {

  assertthat::assert_that(
    inherits(climateSmartDF, "data.frame"),
    msg = "'climateSmartDF' must be a data.frame or sf object."
  )
  assertthat::assert_that(
    "climate_layer" %in% names(climateSmartDF),
    msg = paste0(
      "'climateSmartDF' must contain a 'climate_layer' column ",
      "(output of splnr_climate_feature_preprocess)."
    )
  )
  assertthat::assert_that(
    is.numeric(refugiaTarget) && length(refugiaTarget) == 1 &&
      refugiaTarget >= 0 && refugiaTarget <= 1,
    msg = "'refugiaTarget' must be a single numeric value between 0 and 1."
  )
  assertthat::assert_that(
    is.data.frame(targets),
    msg = "'targets' must be a data.frame."
  )
  assertthat::assert_that(
    "feature" %in% names(targets),
    msg = "'targets' must contain a 'feature' column."
  )
  assertthat::assert_that(
    "target" %in% names(targets),
    msg = "'targets' must contain a 'target' column."
  )

  total_planning_units    <- nrow(climateSmartDF)
  total_climate_smart     <- sum(climateSmartDF$climate_layer, na.rm = TRUE)

  if (total_planning_units == 0) {
    stop("'climateSmartDF' has no planning units. Cannot assign targets.")
  }
  if (total_climate_smart == 0) {
    stop("No climate-smart planning units identified. Cannot assign a climate_layer target.")
  }

  proportion_climate_smart <- total_climate_smart / total_planning_units
  trgt <- refugiaTarget / proportion_climate_smart

  climate_layerDF <- tibble::tribble(
    ~feature,        ~target,
    "climate_layer",  trgt
  )

  finalDF <- dplyr::bind_rows(targets, climate_layerDF)

  # Sanity check: output must have one row per original feature plus one row
  # for the climate_layer.
  assertthat::assert_that(
    nrow(finalDF) == nrow(targets) + 1L,
    msg = paste0(
      "Internal error in splnr_climate_feature_assignTargets(): output has ",
      nrow(finalDF), " rows but expected ", nrow(targets) + 1L,
      " (one per feature plus climate_layer). Please report this as a bug."
    )
  )

  return(finalDF)
}


#' @title Run the Feature Climate-Smart Approach
#'
#' @description
#' `splnr_climate_featureApproach()` implements the Feature Approach to
#' climate-smart conservation planning by defining a global climate-smart layer
#' and adjusting targets to ensure a specified proportion of that layer is captured.
#'
#' @details
#' This function orchestrates two steps:
#' 1. **Preprocessing** via `splnr_climate_feature_preprocess()`.
#' 2. **Target Assignment** via `splnr_climate_feature_assignTargets()`.
#'
#' @param features An `sf` object of conservation features.
#' @param metric An `sf` object with a column named `metric`.
#' @param targets A `data.frame` with columns `feature` and `target`.
#' @param direction `1` or `-1`.
#' @param percentile Numeric (0-100). Defaults to `35`.
#' @param refugiaTarget Numeric (0-1). Defaults to `0.3`.
#' @param metric_col A single character string giving the name of the column in
#'   `metric` that contains the climate metric values. Defaults to `"metric"` for
#'   backwards compatibility. Use this argument when your climate data column has
#'   a different name (e.g. `"sst_trend"`).
#'
#' @return A `list` with:
#'   \itemize{
#'     \item `Features`: `sf` object with original features plus `climate_layer`.
#'     \item `Targets`: `data.frame` with adjusted targets including `climate_layer`.
#'   }
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom sf st_crs
#'
#' @examples
#' \dontrun{
#' initial_targets <- dat_species_bin %>%
#'   sf::st_drop_geometry() %>%
#'   colnames() %>%
#'   data.frame() %>%
#'   setNames(c("feature")) %>%
#'   dplyr::mutate(target = 0.3)
#'
#' Feature_result <- splnr_climate_featureApproach(
#'   features      = dat_species_bin,
#'   metric        = dat_clim,
#'   targets       = initial_targets,
#'   direction     = 1,
#'   percentile    = 35,
#'   refugiaTarget = 0.3
#' )
#' out_sf_feature  <- Feature_result$Features
#' targets_feature <- Feature_result$Targets
#' }
splnr_climate_featureApproach <- function(features,
                                          metric,
                                          targets,
                                          direction,
                                          percentile = 35,
                                          refugiaTarget = 0.3,
                                          metric_col = "metric") {

  assertthat::assert_that(
    inherits(features, "sf"),
    msg = "'features' must be an 'sf' object."
  )
  assertthat::assert_that(
    inherits(metric, "sf"),
    msg = "'metric' must be an 'sf' object."
  )
  assertthat::assert_that(
    is.character(metric_col) && length(metric_col) == 1,
    msg = "'metric_col' must be a single character string."
  )
  assertthat::assert_that(
    metric_col %in% names(metric),
    msg = paste0("'metric' sf object must contain a column named '", metric_col, "'.")
  )
  assertthat::assert_that(
    is.data.frame(targets),
    msg = "'targets' must be a data.frame."
  )
  assertthat::assert_that(
    "feature" %in% names(targets),
    msg = "'targets' must contain a 'feature' column."
  )
  assertthat::assert_that(
    "target" %in% names(targets),
    msg = "'targets' must contain a 'target' column."
  )
  assertthat::assert_that(
    direction %in% c(-1, 1),
    msg = "'direction' must be either 1 or -1."
  )
  assertthat::assert_that(
    is.numeric(percentile) && length(percentile) == 1 &&
      percentile >= 0 && percentile <= 100,
    msg = "'percentile' must be a single numeric value between 0 and 100."
  )
  assertthat::assert_that(
    is.numeric(refugiaTarget) && length(refugiaTarget) == 1 &&
      refugiaTarget >= 0 && refugiaTarget <= 1,
    msg = "'refugiaTarget' must be a single numeric value between 0 and 1."
  )
  assertthat::assert_that(
    sf::st_crs(features) == sf::st_crs(metric),
    msg = "CRS of 'features' and 'metric' must be the same."
  )

  featureFeatures <- splnr_climate_feature_preprocess(
    features   = features,
    metric     = metric,
    direction  = direction,
    percentile = percentile,
    metric_col = metric_col
  )

  featureTargets <- splnr_climate_feature_assignTargets(
    targets        = targets,
    climateSmartDF = featureFeatures,
    refugiaTarget  = refugiaTarget
  )

  # Sanity check: Features must have the same number of rows as the input.
  assertthat::assert_that(
    nrow(featureFeatures) == nrow(features),
    msg = paste0(
      "Internal error in splnr_climate_featureApproach(): Features has ",
      nrow(featureFeatures), " rows but 'features' has ", nrow(features), " rows. ",
      "Please report this as a bug."
    )
  )
  # Sanity check: Targets must have one row per original feature plus climate_layer.
  assertthat::assert_that(
    nrow(featureTargets) == nrow(targets) + 1L,
    msg = paste0(
      "Internal error in splnr_climate_featureApproach(): Targets has ",
      nrow(featureTargets), " rows but expected ", nrow(targets) + 1L,
      " (one per feature plus climate_layer). Please report this as a bug."
    )
  )

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
#' For each feature the function:
#' 1. Joins the feature data with the climate metric (once, before the loop).
#' 2. Filters to planning units where the feature is present.
#' 3. Calculates the `percentile` cutoff within that feature's distribution.
#' 4. Creates a new binary column (`_filtered`) that is 1 only where the feature
#'    is present AND the metric meets the climate-smart threshold.
#'
#' - `direction = 1`: areas with metric >= `(100 - percentile)`th quantile.
#' - `direction = -1`: areas with metric <= `percentile`th quantile.
#'
#' @param features An `sf` object of conservation features.
#' @param metric An `sf` object with a column named `metric`.
#' @param percentile Numeric (0-100).
#' @param direction `1` or `-1`.
#'
#' @return An `sf` object with the same columns as `features` but values
#'   filtered to climate-smart occurrences only.
#' @keywords internal
#' @noRd
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr across bind_cols if_else mutate rename_with select summarize
#' @importFrom rlang .data
#' @importFrom sf st_as_sf st_drop_geometry st_geometry st_set_geometry
#' @importFrom stats quantile
#' @importFrom tidyselect everything
#'
#' @examples
#' \dontrun{
#' percentile_preprocessed <- splnr_climate_percentile_preprocess(
#'   features   = dat_species_bin,
#'   metric     = dat_clim,
#'   percentile = 5,
#'   direction  = 1
#' )
#' }
splnr_climate_percentile_preprocess <- function(features,
                                                metric,
                                                percentile,
                                                direction,
                                                metric_col = "metric") {

  assertthat::assert_that(
    inherits(features, "sf"),
    msg = "'features' must be an 'sf' object."
  )
  assertthat::assert_that(
    inherits(metric, "sf"),
    msg = "'metric' must be an 'sf' object."
  )
  assertthat::assert_that(
    is.character(metric_col) && length(metric_col) == 1,
    msg = "'metric_col' must be a single character string."
  )
  assertthat::assert_that(
    metric_col %in% names(metric),
    msg = paste0("'metric' sf object must contain a column named '", metric_col, "'.")
  )

  # Normalise: rename the user-supplied column to the internal name "metric"
  # so the rest of this function can use a single consistent name.
  # This is a no-op when metric_col == "metric" (the default).
  if (metric_col != "metric") {
    metric <- dplyr::rename(metric, "metric" = !!rlang::sym(metric_col))
  }
  assertthat::assert_that(
    is.numeric(percentile) && length(percentile) == 1 &&
      percentile >= 0 && percentile <= 100,
    msg = "'percentile' must be a single numeric value between 0 and 100."
  )
  assertthat::assert_that(
    direction %in% c(1, -1),
    msg = "'direction' must be either 1 or -1."
  )
  assertthat::assert_that(
    sf::st_crs(features) == sf::st_crs(metric),
    msg = "CRS of 'features' and 'metric' must be the same."
  )

  if (any(is.na(metric$metric))) {
    warning(
      "NAs present in the metric data. ",
      "These will be excluded from per-feature percentile calculations."
    )
  }

  spp <- sf::st_drop_geometry(features) %>% names()

  metric_vals_df <- sf::st_drop_geometry(dplyr::select(metric, "metric"))
  metric_vals_df[[".row_id"]] <- seq_len(nrow(metric_vals_df))

  joined_df <- sf::st_drop_geometry(features)
  joined_df[[".row_id"]] <- seq_len(nrow(joined_df))
  joined_df <- dplyr::left_join(joined_df, metric_vals_df, by = ".row_id")
  joined_df[[".row_id"]] <- NULL   # remove key column; no trace in output

  # Percentile fraction is constant; compute once outside the loop.
  prct <- .splnr_climate_prct(percentile, direction)

  percentileList <- vector("list", length(spp))

  for (i in seq_along(spp)) {
    feat_col    <- spp[i]
    feat_vals   <- joined_df[[feat_col]]
    metric_vals <- joined_df[["metric"]]

    present_idx <- which(feat_vals == 1 & !is.na(metric_vals))

    if (length(present_idx) == 0) {
      warning(
        "Feature '", feat_col, "' has no planning units with valid metric ",
        "data. Filtered column will be all zeros."
      )
      percentileList[[i]] <- data.frame(x = integer(nrow(joined_df)))
      names(percentileList[[i]]) <- feat_col
      next
    }

    qntl <- stats::quantile(metric_vals[present_idx], prct, na.rm = TRUE)[[1]]

    climate_smart <- if (direction == 1) {
      dplyr::if_else(!is.na(metric_vals) & metric_vals >= qntl, 1L, 0L)
    } else {
      dplyr::if_else(!is.na(metric_vals) & metric_vals <= qntl, 1L, 0L)
    }

    # Filtered column: 1 only where feature is present AND climate-smart.
    filtered_col <- dplyr::if_else(feat_vals == 1, climate_smart, 0L)

    percentileList[[i]] <- data.frame(filtered_col)
    names(percentileList[[i]]) <- feat_col
  }

  # Bind all filtered columns and restore the original geometry.
  resultDF <- dplyr::bind_cols(percentileList) %>%
    sf::st_set_geometry(sf::st_geometry(features)) %>%
    sf::st_as_sf()

  # Sanity check: output must have the same number of rows as the input.
  assertthat::assert_that(
    nrow(resultDF) == nrow(features),
    msg = paste0(
      "Internal error in splnr_climate_percentile_preprocess(): output has ",
      nrow(resultDF), " rows but 'features' has ", nrow(features), " rows. ",
      "Please report this as a bug."
    )
  )

  return(resultDF)
}


#' @title Assign Targets for Percentile Climate-Smart Approach
#'
#' @description
#' This internal function calculates and assigns conservation targets when using
#' the Percentile Approach to climate-smart spatial planning.
#'
#' @details
#' For each feature the new target = original target / (filtered_count / original_count),
#' capped at 1. This scales up the target to compensate for the reduced feature
#' distribution after filtering to climate-smart areas.
#'
#' @param features An `sf` object of original (unfiltered) conservation features.
#' @param targets A `data.frame` with columns `feature` and `target`.
#' @param climateSmartDF An `sf` object produced by
#'   `splnr_climate_percentile_preprocess()`.
#'
#' @return A `data.frame` with columns `feature` and `target`.
#' @keywords internal
#' @noRd
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr across everything if_else left_join mutate select summarize
#' @importFrom rlang .data
#' @importFrom sf st_drop_geometry
#' @importFrom tidyr replace_na pivot_longer
#'
#' @examples
#' \dontrun{
#' percentile_targets <- splnr_climate_percentile_assignTargets(
#'   features       = dat_species_bin,
#'   targets        = initial_targets,
#'   climateSmartDF = preprocessed_features_percentile
#' )
#' }
splnr_climate_percentile_assignTargets <- function(features,
                                                   climateSmartDF,
                                                   targets) {

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
    msg = "'targets' must contain a 'feature' column."
  )
  assertthat::assert_that(
    "target" %in% names(targets),
    msg = "'targets' must contain a 'target' column."
  )
  assertthat::assert_that(
    inherits(climateSmartDF, "data.frame"),
    msg = "'climateSmartDF' must be a data.frame or sf object."
  )

  original_feature_names <- sf::st_drop_geometry(features) %>% names()
  filtered_feature_names <- sf::st_drop_geometry(climateSmartDF) %>% names()
  assertthat::assert_that(
    all(original_feature_names %in% filtered_feature_names),
    msg = "Feature names in 'climateSmartDF' do not match those in 'features'."
  )

  # Original planning-unit counts per feature.
  df_orig <- features %>%
    sf::st_drop_geometry() %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ tidyr::replace_na(.x, 0))) %>%
    dplyr::summarize(dplyr::across(dplyr::everything(), sum)) %>%
    tidyr::pivot_longer(
      tidyselect::everything(),
      names_to  = "feature",
      values_to = "original"
    ) %>%
    dplyr::left_join(targets, by = "feature")

  # Filtered planning-unit counts per feature.
  df_filt <- climateSmartDF %>%
    sf::st_drop_geometry() %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ tidyr::replace_na(.x, 0))) %>%
    dplyr::summarize(dplyr::across(dplyr::everything(), sum)) %>%
    tidyr::pivot_longer(
      tidyselect::everything(),
      names_to  = "feature",
      values_to = "filtered"
    )

  df <- df_orig %>%
    dplyr::left_join(df_filt, by = "feature") %>%
    dplyr::mutate(
      proportion = dplyr::if_else(.data$original > 0,
                                  .data$filtered / .data$original, 0),
      target     = dplyr::if_else(.data$proportion > 0,
                                  .data$target / .data$proportion,
                                  .data$target),
      # Cap at 1 (100%) to prevent infeasible targets.
      target     = dplyr::if_else(.data$target > 1, 1, .data$target)
    ) %>%
    dplyr::select("feature", "target")

  # Sanity check: output must have the same number of rows as the input targets
  # (same features, adjusted targets — no rows added or removed).
  assertthat::assert_that(
    nrow(df) == nrow(targets),
    msg = paste0(
      "Internal error in splnr_climate_percentile_assignTargets(): output has ",
      nrow(df), " rows but expected ", nrow(targets),
      " (one per feature). Please report this as a bug."
    )
  )

  return(df)
}


#' @title Run the Percentile Climate-Smart Approach
#'
#' @description
#' `splnr_climate_percentileApproach()` implements the Percentile Approach to
#' climate-smart conservation planning by filtering features to their most
#' climate-resilient areas and adjusting targets accordingly.
#'
#' @details
#' This function orchestrates two steps:
#' 1. **Preprocessing** via `splnr_climate_percentile_preprocess()`.
#' 2. **Target Assignment** via `splnr_climate_percentile_assignTargets()`.
#'
#' @param features An `sf` object of conservation features.
#' @param metric An `sf` object with a column named `metric`.
#' @param targets A `data.frame` with columns `feature` and `target`.
#' @param direction `1` or `-1`.
#' @param percentile Numeric (0-100). Defaults to `35`.
#' @param metric_col A single character string giving the name of the column in
#'   `metric` that contains the climate metric values. Defaults to `"metric"` for
#'   backwards compatibility. Use this argument when your climate data column has
#'   a different name (e.g. `"sst_trend"`).
#'
#' @return A `list` with:
#'   \itemize{
#'     \item `Features`: `sf` object filtered to climate-smart occurrences.
#'     \item `Targets`: `data.frame` with adjusted targets.
#'   }
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom sf st_crs
#'
#' @examples
#' \dontrun{
#' initial_targets <- dat_species_bin %>%
#'   sf::st_drop_geometry() %>%
#'   colnames() %>%
#'   data.frame() %>%
#'   setNames(c("feature")) %>%
#'   dplyr::mutate(target = 0.3)
#'
#' Percentile_result <- splnr_climate_percentileApproach(
#'   features   = dat_species_bin,
#'   metric     = dat_clim,
#'   targets    = initial_targets,
#'   direction  = 1,
#'   percentile = 35
#' )
#' out_sf_percentile  <- Percentile_result$Features
#' targets_percentile <- Percentile_result$Targets
#' }
splnr_climate_percentileApproach <- function(features,
                                             metric,
                                             targets,
                                             direction,
                                             percentile = 35,
                                             metric_col = "metric") {

  assertthat::assert_that(
    inherits(features, "sf"),
    msg = "'features' must be an 'sf' object."
  )
  assertthat::assert_that(
    inherits(metric, "sf"),
    msg = "'metric' must be an 'sf' object."
  )
  assertthat::assert_that(
    is.character(metric_col) && length(metric_col) == 1,
    msg = "'metric_col' must be a single character string."
  )
  assertthat::assert_that(
    metric_col %in% names(metric),
    msg = paste0("'metric' sf object must contain a column named '", metric_col, "'.")
  )
  assertthat::assert_that(
    is.data.frame(targets),
    msg = "'targets' must be a data.frame."
  )
  assertthat::assert_that(
    "feature" %in% names(targets),
    msg = "'targets' must contain a 'feature' column."
  )
  assertthat::assert_that(
    "target" %in% names(targets),
    msg = "'targets' must contain a 'target' column."
  )
  assertthat::assert_that(
    direction %in% c(-1, 1),
    msg = "'direction' must be either 1 or -1."
  )
  assertthat::assert_that(
    is.numeric(percentile) && length(percentile) == 1 &&
      percentile >= 0 && percentile <= 100,
    msg = "'percentile' must be a single numeric value between 0 and 100."
  )
  assertthat::assert_that(
    sf::st_crs(features) == sf::st_crs(metric),
    msg = "CRS of 'features' and 'metric' must be the same."
  )

  percentileFeatures <- splnr_climate_percentile_preprocess(
    features   = features,
    metric     = metric,
    direction  = direction,
    percentile = percentile,
    metric_col = metric_col
  )

  percentileTargets <- splnr_climate_percentile_assignTargets(
    features       = features,
    targets        = targets,
    climateSmartDF = percentileFeatures
  )

  # Sanity check: Features must have the same number of rows as the input.
  assertthat::assert_that(
    nrow(percentileFeatures) == nrow(features),
    msg = paste0(
      "Internal error in splnr_climate_percentileApproach(): Features has ",
      nrow(percentileFeatures), " rows but 'features' has ", nrow(features), " rows. ",
      "Please report this as a bug."
    )
  )
  # Sanity check: Targets must have the same number of rows as the input targets.
  assertthat::assert_that(
    nrow(percentileTargets) == nrow(targets),
    msg = paste0(
      "Internal error in splnr_climate_percentileApproach(): Targets has ",
      nrow(percentileTargets), " rows but expected ", nrow(targets),
      " (one per feature). Please report this as a bug."
    )
  )

  return(list(Features = percentileFeatures, Targets = percentileTargets))
}
