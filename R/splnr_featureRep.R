#' @title Prepare Data to Plot How Well Targets Are Met
#'
#' @description
#' `splnr_get_featureRep()` calculates the representation of conservation
#' features within a `prioritizr` solution. This function determines how much
#' of each feature's total abundance (or area) is captured in the selected
#' planning units, and compares it against specified conservation targets.
#' It can also account for different climate-smart planning approaches.
#'
#' @details
#' This function processes the output of a `prioritizr` conservation problem
#' (`soln`) and its corresponding problem definition (`pDat`) to provide a
#' summary of feature representation. It is designed to work whether or not
#' explicit targets are provided, and can adjust calculations based on the
#' climate-smart approach used.
#'
#' The function calculates:
#' \itemize{
#'   \item `total_amount`: The total available amount/area of each feature across all planning units.
#'   \item `absolute_held`: The total amount/area of each feature captured in the
#'         *selected* planning units (where `solution_1` is 1).
#'   \item `relative_held`: The proportion of `absolute_held` relative to `total_amount`,
#'         indicating the percentage representation of the feature in the solution.
#'   \item `target`: The conservation target for each feature (either from the
#'         `pDat` problem definition or the `targets` dataframe).
#'   \item `incidental`: A logical flag indicating if a feature's representation
#'         was 'incidental' (i.e., its target was 0 or NA, but it was still
#'         partially or fully captured in the solution).
#' }
#'
#' \strong{Climate-Smart Considerations (`climsmart = TRUE`):}
#' If `climsmart` is `TRUE`, the function adjusts its calculations based on the
#' `climsmartApproach` parameter:
#' \itemize{
#'   \item `climsmartApproach = 1` (Climate Priority Area): The function sums the
#'         `absolute_held` and `total_amount` for features that were split into
#'         `_CS` (Climate-Smart) and `_NCS` (Non-Climate-Smart) components. This
#'         provides a single, aggregated representation for the original feature,
#'         allowing comparison with its original target.
#'   \item `climsmartApproach = 3` (Percentile Approach): The function directly
#'         uses the targets provided in the `targets` dataframe, which are
#'         expected to be adjusted for the percentile approach.
#'   \item For other `climsmartApproach` values or if `climsmart` is `FALSE`,
#'         targets are taken directly from the `prioritizr` problem's target data.
#' }
#'
#' The output dataframe is designed to be directly plottable by functions
#' like `splnr_plot_featureRep()`.
#'
#' @param soln An `sf` object representing the `prioritizr` solution, containing
#'   a column indicating selected planning units (default: `solution_1`).
#' @param pDat A `prioritizr` problem object, as defined by `prioritizr::problem()`.
#'   This object provides the original feature data and targets.
#' @param targets A `data.frame` (optional). If provided, it should contain a
#'   `feature` column (character) and a `target` column (numeric). This is used
#'   to override or supplement targets from `pDat`, especially for climate-smart
#'   approaches where targets might be pre-adjusted. Defaults to `NA`.
#' @param climsmart A logical value (`TRUE` or `FALSE`). If `TRUE`, special
#'   handling for climate-smart approaches is enabled. Defaults to `FALSE`.
#' @param climsmartApproach An integer (0, 1, 2, or 3) indicating the type of
#'   climate-smart approach used:
#'   \itemize{
#'     \item `0`: No climate-smart approach.
#'     \item `1`: Climate Priority Area approach (features split into CS/NCS).
#'     \item `2`: Feature approach (not explicitly handled in this function's
#'                  `climsmart` logic, targets taken from `pDat` by default).
#'     \item `3`: Percentile approach (features are filtered).
#'   }
#'   Defaults to `0`.
#' @param solnCol A character string specifying the name of the column in `soln`
#'   that contains the binary solution (1 for selected, 0 for not selected).
#'   Defaults to `"solution_1"`.
#'
#' @return A `tibble` dataframe containing the `feature` names, their
#'   `total_amount` (total units available), `absolute_held` (total units
#'   selected), `relative_held` (proportion held), `target` (conservation target),
#'   and `incidental` (TRUE if target was 0 or NA, but feature still present).
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows case_when filter group_by if_else left_join mutate pull select summarise ungroup
#' @importFrom prioritizr eval_feature_representation_summary
#' @importFrom rlang .data sym
#' @importFrom sf st_drop_geometry
#' @importFrom stats na.omit
#' @importFrom stringr str_remove_all
#' @importFrom tibble as_tibble tibble
#' @importFrom tidyselect any_of starts_with everything
#' @importFrom tidyr pivot_longer
#'
#' @examples
#' \dontrun{
#' # Assuming 'dat_species_bin' is an existing sf object with binary species data
#' # and 'Cost' column.
#'
#' # Create a dummy prioritizr problem for basic demonstration
#' pDat_basic <- prioritizr::problem(
#'   dat_species_bin %>% dplyr::mutate(Cost = runif(n = dim(.)[[1]])),
#'   features = c("Spp1", "Spp2", "Spp3", "Spp4", "Spp5"),
#'   cost_column = "Cost"
#' ) %>%
#'   prioritizr::add_min_set_objective() %>%
#'   prioritizr::add_relative_targets(0.3) %>%
#'   prioritizr::add_binary_decisions() %>%
#'   prioritizr::add_default_solver(verbose = FALSE)
#'
#' # Solve the problem
#' soln_basic <- pDat_basic %>%
#'   prioritizr::solve.ConservationProblem()
#'
#' # Get feature representation for a basic (non-climate-smart) solution
#' df_basic_rep <- splnr_get_featureRep(
#'   soln = soln_basic,
#'   pDat = pDat_basic
#' )
#' print(df_basic_rep)
#'
#' # Example with Climate Priority Area (CPA) approach
#' # Assuming 'dat_clim' is an sf object with a 'metric' column.
#' # These would typically come from splnr_climate_priorityAreaApproach()
#' # For example purposes, we'll create some dummy data and targets.
#'
#' # Simulate CPA processed features and targets
#' cpa_features_sim <- dat_species_bin %>%
#'   dplyr::mutate(
#'     Spp1_CS = ifelse(Spp1 == 1 & runif(n()) < 0.5, 1, 0),
#'     Spp1_NCS = ifelse(Spp1 == 1 & Spp1_CS == 0, 1, 0),
#'     Spp2_CS = ifelse(Spp2 == 1 & runif(n()) < 0.6, 1, 0),
#'     Spp2_NCS = ifelse(Spp2 == 1 & Spp2_CS == 0, 1, 0),
#'     Spp3_CS = ifelse(Spp3 == 1 & runif(n()) < 0.7, 1, 0),
#'     Spp3_NCS = ifelse(Spp3 == 1 & Spp3_CS == 0, 1, 0)
#'   ) %>%
#'   dplyr::select(Spp1_CS, Spp1_NCS, Spp2_CS, Spp2_NCS, Spp3_CS, Spp3_NCS, geometry)
#'
#' cpa_targets_sim <- data.frame(
#'   feature = c("Spp1_CS", "Spp1_NCS", "Spp2_CS", "Spp2_NCS", "Spp3_CS", "Spp3_NCS"),
#'   target = c(0.8, 0.2, 0.9, 0.1, 0.7, 0.3) # Example targets for CS/NCS parts
#' )
#'
#' # Create a problem with the simulated CPA features
#' pDat_cpa_sim <- prioritizr::problem(
#'   cpa_features_sim %>% dplyr::mutate(Cost = runif(n = dim(.)[[1]])),
#'   features = c("Spp1_CS", "Spp1_NCS", "Spp2_CS", "Spp2_NCS", "Spp3_CS", "Spp3_NCS"),
#'   cost_column = "Cost"
#' ) %>%
#'   prioritizr::add_min_set_objective() %>%
#'   prioritizr::add_relative_targets(cpa_targets_sim$target, cpa_targets_sim$feature) %>%
#'   prioritizr::add_binary_decisions() %>%
#'   prioritizr::add_default_solver(verbose = FALSE)
#'
#' # Solve the CPA problem
#' soln_cpa_sim <- pDat_cpa_sim %>%
#'   prioritizr::solve.ConservationProblem()
#'
#' # Get feature representation for CPA approach
#' df_cpa_rep <- splnr_get_featureRep(
#'   soln = soln_cpa_sim,
#'   pDat = pDat_cpa_sim,
#'   targets = cpa_targets_sim, # Pass the original CPA targets
#'   climsmart = TRUE,
#'   climsmartApproach = 1 # Indicate CPA approach
#' )
#' print(df_cpa_rep)
#' }
splnr_get_featureRep <- function(soln, pDat, targets = NA,
                                 climsmart = FALSE,
                                 climsmartApproach = 0,
                                 solnCol = "solution_1") {

  # --- Input Assertions ---
  # Ensure 'soln' is an sf object and not empty.
  assertthat::assert_that(
    inherits(soln, "sf"),
    msg = "'soln' must be an 'sf' object."
  )
  assertthat::assert_that(
    nrow(soln) > 0,
    msg = "'soln' dataframe must not be empty."
  )
  # Ensure 'pDat' is a prioritizr problem object.
  assertthat::assert_that(
    inherits(pDat, "ConservationProblem"),
    msg = "'pDat' must be a 'prioritizr::ConservationProblem' object."
  )
  # Ensure 'solnCol' is a character string and exists in 'soln'.
  assertthat::assert_that(
    is.character(solnCol) && length(solnCol) == 1,
    msg = "'solnCol' must be a single character string."
  )
  assertthat::assert_that(
    solnCol %in% names(soln),
    msg = paste0("Solution column '", solnCol, "' not found in 'soln'.")
  )
  # Ensure 'climsmart' is logical.
  assertthat::assert_that(
    is.logical(climsmart) && length(climsmart) == 1,
    msg = "'climsmart' must be a single logical value (TRUE or FALSE)."
  )
  # Ensure 'climsmartApproach' is a valid integer.
  assertthat::assert_that(
    is.numeric(climsmartApproach) && length(climsmartApproach) == 1 && climsmartApproach %in% c(0, 1, 2, 3),
    msg = "'climsmartApproach' must be an integer (0, 1, 2, or 3)."
  )
  # Validate 'targets' if provided (not NA).
  if (!all(is.na(targets))) {
    assertthat::assert_that(
      is.data.frame(targets),
      msg = "If 'targets' is provided, it must be a data.frame."
    )
    assertthat::assert_that(
      all(c("feature", "target") %in% names(targets)),
      msg = "If 'targets' is a data.frame, it must contain 'feature' and 'target' columns."
    )
  }

  # Extract feature names from the problem data (pDat).
  # prioritizr problems store feature names in pDat$data$features[[1]]
  s_cols <- pDat$data$features[[1]]

  # --- Process non-selected features (if any) ---
  # These are features present in the solution object but NOT part of the
  # core features defined in pDat. This often includes 'Cost' or other
  # temporary columns, or potentially features with 0 targets.

  # Select columns that are not 'Cost_', 'solution_' or 'metric', and not in s_cols.
  # These are considered "not selected" or ancillary features for initial processing.
  not_selected <- soln %>%
    dplyr::select(
      -tidyselect::starts_with(c("Cost", "solution_")), # Exclude Cost and solution columns
      -tidyselect::any_of(c("metric")), # Exclude 'metric' if it exists
      -tidyselect::any_of(s_cols) # Exclude primary features defined in pDat
    ) %>%
    sf::st_drop_geometry() # Drop geometry for numerical operations

  # Get column names of remaining 'not_selected' features.
  ns_cols <- colnames(not_selected)

  # Proceed if there are any non-selected features to process.
  if (length(ns_cols) > 0) {
    # Combine non_selected features with the solution column for filtering.
    ns1 <- not_selected %>%
      dplyr::select(tidyselect::all_of(ns_cols)) %>%
      dplyr::mutate(solution = dplyr::pull(soln, !!rlang::sym(solnCol))) # Add the solution column

    # Calculate the total amount of each non-selected feature.
    area_feature <- ns1 %>%
      dplyr::select(-c("solution")) %>% # Remove solution column for total sum
      tidyr::pivot_longer(cols = tidyselect::everything(), names_to = "feature", values_to = "total_amount") %>%
      dplyr::group_by(.data$feature) %>%
      dplyr::summarise(total_amount = sum(.data$total_amount, na.rm = TRUE)) # Sum total amount, handling NAs

    # Calculate the absolute amount of each non-selected feature in selected units.
    selected_feature <- ns1 %>%
      dplyr::filter(.data$solution == 1) %>% # Filter for selected planning units
      dplyr::select(-c("solution")) %>% # Remove solution column for sum
      tidyr::pivot_longer(cols = tidyselect::everything(), names_to = "feature", values_to = "absolute_held") %>%
      dplyr::group_by(.data$feature) %>%
      dplyr::summarise(absolute_held = sum(.data$absolute_held, na.rm = TRUE)) # Sum absolute held, handling NAs

    # Join total and selected amounts and calculate relative held.
    ns1 <- dplyr::left_join(area_feature, selected_feature, by = "feature") %>%
      dplyr::mutate(
        relative_held = dplyr::if_else(
          .data$total_amount > 0, # Avoid division by zero
          .data$absolute_held / .data$total_amount,
          0 # Set to 0 if total_amount is 0
        )
      )
  } else {
    # If no non-selected features, create an empty tibble with required columns.
    message("No non-selected features to process.")
    ns1 <- tibble::tibble(
      feature = character(),
      total_amount = numeric(),
      absolute_held = numeric(),
      relative_held = numeric()
    )
  }

  # --- Process primary selected features (from pDat) ---

  # Create a tibble with the solution column from the soln sf object.
  # This is needed as eval_feature_representation_summary expects a data.frame.
  soln_df <- soln %>%
    dplyr::rename(solution = !!rlang::sym(solnCol)) %>% # Rename solution column to 'solution'
    tibble::as_tibble() # Convert to tibble for prioritizr function

  # Evaluate feature representation summary using prioritizr's internal function.
  s1 <- prioritizr::eval_feature_representation_summary(pDat, soln_df[, "solution"]) %>%
    dplyr::select(-"summary") # Remove the 'summary' column, which is not needed here.

  # --- Apply Climate-Smart Logic ---

  # If climate-smart approach is enabled and is CPA (Approach 1).
  if (climsmart == TRUE && climsmartApproach == 1) {
    message("Processing features with Climate Priority Area (CPA) approach.")
    s1 <- s1 %>%
      dplyr::select(-.data$relative_held) %>% # Remove existing relative_held for recalculation
      # Remove _CS and _NCS suffixes to group related features.
      dplyr::mutate(
        feature = stringr::str_remove_all(.data$feature, "_CS"),
        feature = stringr::str_remove_all(.data$feature, "_NCS")
      ) %>%
      dplyr::group_by(.data$feature) %>% # Group by original feature names
      dplyr::summarise(
        total_amount = sum(.data$total_amount, na.rm = TRUE), # Sum total amounts for original feature
        absolute_held = sum(.data$absolute_held, na.rm = TRUE) # Sum absolute held for original feature
      ) %>%
      dplyr::ungroup() %>%
      # Recalculate relative held for the aggregated feature.
      dplyr::mutate(relative_held = dplyr::if_else(
        .data$total_amount > 0,
        .data$absolute_held / .data$total_amount,
        0
      )) %>%
      # Join with the provided 'targets' dataframe for CPA.
      # This assumes 'targets' dataframe contains adjusted targets for original features.
      dplyr::left_join(targets, by = "feature")

  } else if (climsmart == TRUE && climsmartApproach == 3) {
    # If climate-smart approach is enabled and is Percentile Approach (Approach 3).
    message("Processing features with Percentile Climate-Smart Approach.")
    # For percentile approach, directly join with provided 'targets' as they are
    # assumed to be already adjusted for the filtered features.
    s1 <- s1 %>%
      dplyr::left_join(targets, by = "feature")

  } else {
    # Default case: no climate-smart approach or other approaches.
    # Targets are taken directly from the prioritizr problem object.
    message("No specific climate-smart approach detected or standard approach used. Using targets from 'pDat'.")
    s1 <- s1 %>%
      dplyr::left_join(pDat$targets$data[["targets"]], by = "feature") %>%
      dplyr::select(-"type") # Remove 'type' column from prioritizr targets if it exists.
  }

  # Remove rows with NA in 'relative_held' which might occur if total_amount was zero.
  s1 <- s1 %>%
    stats::na.omit() # Remove any rows with NAs (e.g., if target was NA and not handled by above logic)

  # --- Combine and Finalize Results ---

  # Bind rows of primary features (s1) and non-selected features (ns1).
  # This ensures all features are represented in the final output.
  df <- dplyr::bind_rows(s1, ns1)

  # Add an 'incidental' flag: TRUE if a feature was included but its target was 0 or NA.
  # This helps distinguish features intentionally targeted from those incidentally selected.
  df <- df %>%
    dplyr::mutate(
      incidental = dplyr::if_else(
        .data$target > 0 & .data$absolute_held > 0, # If target > 0 AND something was held, it's NOT incidental
        FALSE,
        TRUE, # Otherwise, it's incidental (target 0, NA, or target > 0 but nothing held)
        missing = TRUE # Explicitly handle missing values for 'target'
      )
    )

  return(df)
}


#' @title Plot Feature Representation (Target Achievement)
#'
#' @description
#' `splnr_plot_featureRep()` creates a bar plot to visualize the representation
#' of features in a conservation solution, indicating how well targets are met.
#' It can categorize features, rename them for clarity, and optionally display
#' the target levels on the plot.
#'
#' @param df A [data.frame][base::data.frame] or [tibble][tibble::tibble]
#'   containing the feature representation information. This typically
#'   results from the `splnr_get_featureRep()` function and should include at
#'   least `feature` and `relative_held` columns, and optionally `target` and `incidental`.
#' @param category A named [data.frame][base::data.frame] or [tibble][tibble::tibble]
#'   that provides grouping information for features. It should contain a column
#'   that can be matched with the `feature` column in `df` (by default, a column
#'   named `feature`, or specified by `categoryFeatureCol`), and a column named
#'   `category` for grouping the plot output. If `NA` (default), no categorization is applied.
#' @param categoryFeatureCol A [character][base::character] string specifying the
#'   name of the column in the `category` data frame that contains the feature
#'   information to be matched with `df$feature`. This is used if the `category`
#'   data frame does not have a column explicitly named `'feature'`.
#' @param renameFeatures A [logical][base::logical] value. If `TRUE`, feature names
#'   in the plot will be replaced with common names provided in `namesToReplace`.
#' @param namesToReplace A [data.frame][base::data.frame] containing two columns:
#'   `'nameVariable'` (the original feature name) and `'nameCommon'` (the common name
#'   to replace it with). Required if `renameFeatures` is `TRUE`.
#' @param nr An [integer][base::integer] specifying the number of rows for the legend.
#' @param showTarget A [logical][base::logical] value. If `TRUE`, a transparent bar
#'   representing the target level for each feature will be shown on the plot.
#' @param plotTitle A [character][base::character] string for the title of the plot.
#'   Can be an empty string `""` (default).
#' @param sort_by A [character][base::character] string specifying the column
#'   by which to sort the features on the x-axis. Accepted values include:
#'   `"category"`, `"feature"`, `"target"`, `"representation"` (`relative_held`),
#'   or `"difference"` (between representation and target).
#' @param ... Other arguments passed on to [ggplot2::theme()] to customize the plot's theme.
#'
#' @return A [ggplot2::ggplot] object representing the feature representation bar plot.
#' @export
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows filter if_else left_join mutate rename select
#' @importFrom ggplot2 aes element_blank element_rect element_text geom_bar ggplot labs
#' @importFrom ggplot2 guide_legend guides scale_fill_manual scale_y_continuous theme theme_bw
#' @importFrom rlang .data
#' @importFrom stringr str_c str_replace_all
#' @importFrom stats reorder
#' @importFrom tibble deframe tibble
#'
#' @examples
#' # For a full example, ensure 'dat_species_bin', 'dat_category' are available
#' # (e.g., from the 'prioritizrdata' package or defined in your package's data)
#'
#'
#' pDat <- prioritizr::problem(dat_species_bin %>% dplyr::mutate(Cost = runif(n = dim(.)[[1]])),
#'   features = c("Spp1", "Spp2", "Spp3", "Spp4", "Spp5"),
#'   cost_column = "Cost"
#' ) %>%
#'   prioritizr::add_min_set_objective() %>%
#'   prioritizr::add_relative_targets(0.3) %>%
#'   prioritizr::add_binary_decisions() %>%
#'   prioritizr::add_default_solver(verbose = FALSE)
#'
#' soln <- pDat %>%
#'   prioritizr::solve.ConservationProblem()
#'
#' # including incidental species coverage
#' df <- splnr_get_featureRep( # Assuming splnr_get_featureRep is available
#'   soln = soln,
#'   pDat = pDat
#' )
#'
#' # Basic plot with categories and targets shown
#' (splnr_plot_featureRep(df, category = dat_category, showTarget = TRUE))
#'
#' # Plot without categories, sorted by feature name
#' (splnr_plot_featureRep(df, showTarget = TRUE, sort_by = "feature"))
#'
#' # Example with feature renaming
#' names_to_replace_df <- tibble::tibble(
#'   nameVariable = c("Spp1", "Spp2"),
#'   nameCommon = c("Species One", "Species Two")
#' )
#' (splnr_plot_featureRep(df,
#'   category = dat_category,
#'   renameFeatures = TRUE,
#'   namesToReplace = names_to_replace_df,
#'   showTarget = TRUE
#' ))
splnr_plot_featureRep <- function(df,
                                  category = NA,
                                  categoryFeatureCol = NA,
                                  renameFeatures = FALSE,
                                  namesToReplace = NA,
                                  nr = 1,
                                  showTarget = NA,
                                  plotTitle = "",
                                  sort_by = "category",
                                  ...) {

  assertthat::assert_that(
    inherits(df, c("data.frame", "tbl_df")),
    is.logical(renameFeatures),
    is.logical(showTarget),
    is.character(plotTitle),
    # Check if category is NA-filled or a data frame/tibble
    all(is.na(category)) || inherits(category, c("data.frame", "tbl", "tbl_df")),
    is.numeric(nr), # nr should be numeric
    # Validate sort_by against allowed values
    sort_by %in% c("category", "feature", "target", "difference", "representation", "relative_held"),
    msg = "Invalid 'sort_by' value. Must be one of 'category', 'feature', 'target', 'difference', 'representation', or 'relative_held'."
  )

  if (renameFeatures) {
    assertthat::assert_that(
      is.data.frame(namesToReplace),
      "nameVariable" %in% colnames(namesToReplace),
      "nameCommon" %in% colnames(namesToReplace),
      msg = paste0(
        "When 'renameFeatures' is TRUE, 'namesToReplace' must be a data frame ",
        "with 'nameVariable' and 'nameCommon' columns."
      )
    )
  }

  if (inherits(category, c("data.frame", "tbl_df")) & !("feature" %in% colnames(category))) {
    assertthat::assert_that(
      is.character(categoryFeatureCol) && length(categoryFeatureCol) == 1,
      categoryFeatureCol %in% colnames(category),
      msg = paste0(
        "If 'category' is a data frame and does not have a 'feature' column, ",
        "'categoryFeatureCol' must be a character string specifying the column ",
        "in 'category' that contains feature information."
      )
    )
    category <- category %>%
      dplyr::rename(feature = categoryFeatureCol)
  }


  if (renameFeatures == TRUE) {
    # No assertthat::assert_that(is.data.frame(namesToReplace)) needed here,
    # as it's covered by the initial assertthat block.

    rpl <- namesToReplace %>%
      dplyr::filter(.data$nameVariable %in% df$feature) %>%
      dplyr::select("nameVariable", "nameCommon") %>%
      dplyr::mutate(nameVariable = stringr::str_c("^", .data$nameVariable, "$")) %>%
      tibble::deframe()

    df <- df %>%
      dplyr::mutate(feature = stringr::str_replace_all(.data$feature, rpl))

    # Only attempt to rename features in category if category is actually provided
    if (inherits(category, c("data.frame", "tbl_df")) && "feature" %in% colnames(category)) {
      category <- category %>%
        dplyr::mutate(feature = stringr::str_replace_all(.data$feature, rpl))
    }
  }


  if (inherits(category, c("data.frame", "tbl_df")) & ("feature" %in% colnames(category))) {
    df <- df %>%
      dplyr::left_join(category, by = "feature") %>%
      dplyr::arrange(.data$category, .data$feature) %>%
      dplyr::mutate(feature = factor(.data$feature, levels = .data$feature))
  } else {
    # If no category is provided or matched, ensure 'category' column exists for plotting
    if (!("category" %in% colnames(df))) {
      df <- df %>% dplyr::mutate(category = "Uncategorized")
    }
  }


  if (max(df$relative_held, na.rm = TRUE) <= 1) { # Check max before multiplying
    df <- df %>%
      dplyr::mutate(
        relative_held = .data$relative_held * 100,
        target = .data$target * 100
      )
  }

  # Ensure 'sort_by' columns exist before use
  if (sort_by == "difference" && !("target" %in% colnames(df) && "relative_held" %in% colnames(df))) {
    stop("Cannot sort by 'difference': 'target' and/or 'relative_held' columns are missing.")
  }
  if (sort_by == "representation" && !("relative_held" %in% colnames(df))) {
    stop("Cannot sort by 'representation': 'relative_held' column is missing.")
  }
  if (sort_by == "target" && !("target" %in% colnames(df))) {
    stop("Cannot sort by 'target': 'target' column is missing.")
  }

  # Calculate 'difference' and 'representation' if sorting by them
  if (sort_by %in% c("difference", "representation")) {
    df <- df %>%
      dplyr::mutate(
        difference = .data$relative_held - .data$target,
        representation = .data$relative_held
      )
  }

  uniqueCat <- unique(df$category[!is.na(df$category)])

  colr <- tibble::tibble(
    Category = uniqueCat,
    Colour = viridis::viridis(length(uniqueCat))
  ) %>%
    tibble::deframe()

  gg_target <- ggplot2::ggplot() +
    ggplot2::geom_bar(
      data = df %>% dplyr::mutate(relative_held = dplyr::if_else(.data$incidental == TRUE, NA_real_, .data$relative_held)), # Use NA_real_ for numeric NA
      stat = "identity", position = "identity",
      ggplot2::aes(
        x = stats::reorder(.data$feature, .data[[sort_by]]), y = .data$relative_held,
        fill = .data$category, colour = .data$category
      ),
      na.rm = TRUE
    ) +
    ggplot2::geom_bar( # Add features in the df that had a zero or missing target
      data = df %>% dplyr::mutate(relative_held = dplyr::if_else(.data$incidental == FALSE, NA_real_, .data$relative_held)), # Use NA_real_
      stat = "identity", position = "identity",
      ggplot2::aes(x = .data$feature, y = .data$relative_held), na.rm = TRUE,
      fill = "NA", colour = "black"
    ) +
    ggplot2::labs(title = plotTitle, x = "Feature", y = "Representation of features \nin total selected area (%)") +
    ggplot2::theme_bw() +
    # Ensure ymax is calculated correctly and handled for empty df
    ggplot2::scale_y_continuous(
      limits = c(0, max(df$relative_held, na.rm = TRUE, 0) + 10), # Ensure at least 0 if all NA
      expand = c(0, 0)
    ) +
    ggplot2::scale_fill_manual(
      aesthetics = c("fill", "colour"),
      values = colr,
      guide = ggplot2::guide_legend(nrow = nr)
    ) +
    ggplot2::guides(colour = "none") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5, size = 16, colour = "black"),
      axis.text.y = ggplot2::element_text(size = 16, colour = "black"),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_text(size = 16),
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 16),
      legend.position = "top",
      legend.direction = "horizontal",
      legend.background = ggplot2::element_rect(fill = "NA"),
      title = ggplot2::element_text(size = 16),
      ...
    )

  if (!(is.na(showTarget)) && showTarget == TRUE) { # Explicitly check for TRUE
    assertthat::assert_that(
      "target" %in% colnames(df),
      msg = "Cannot show target: 'target' column is missing from the data frame."
    )
    gg_target <- gg_target +
      ggplot2::geom_bar(
        data = df %>% dplyr::mutate(relative_held = dplyr::if_else(.data$incidental == TRUE, NA_real_, .data$relative_held)), # Use NA_real_
        stat = "identity", position = "identity", ggplot2::aes(x = .data$feature, y = .data$target), na.rm = TRUE,
        alpha = 0.3, colour = "grey50", fill = "white"
      )
  }

  return(gg_target)
}


#' @title Plot Circular Barplot for Feature Representation
#'
#' @description
#' `splnr_plot_circBplot()` creates a circular bar plot to visualize feature
#' representation, categorized by groups. It's particularly useful for
#' displaying how different categories of features meet certain targets in a radial layout.
#'
#' @param df A [data.frame][base::data.frame] or [tibble][tibble::tibble] that
#'   **must** contain the following columns:
#'   \itemize{
#'     \item `feature`: [character][base::character] or [factor][base::factor] unique identifier for each individual bar (e.g., species names).
#'     \item `value`: [numeric][base::numeric] the value to be plotted on the y-axis (bar height, typically percentage representation).
#'     \item `group`: [character][base::character] or [factor][base::factor] for grouping factors (e.g., "important", "representative").
#'   }
#' @param legend_color A [named vector][base::vector] of colors. Names must correspond
#'   to the unique values in the `group` column of `df`, and values are the
#'   corresponding colors. For example: `c("group_name1" = "red", "group_name2" = "blue")`.
#' @param legend_list A [character vector][base::character] of labels for the legend.
#'   This should match the names used in `legend_color` or the levels of `group`.
#' @param indicateTargets A [logical][base::logical] value. If `TRUE`, horizontal
#'   lines indicating `impTarget` and `repTarget` will be drawn on the plot.
#' @param impTarget A [numeric][base::numeric] value representing the target
#'   percentage for 'important' features. Required if `indicateTargets` is `TRUE`.
#' @param repTarget A [numeric][base::numeric] value representing the target
#'   percentage for 'representative' features. Required if `indicateTargets` is `TRUE`.
#' @param colTarget A [character][base::character] string specifying the color
#'   for the target indicator lines.
#'
#' @return A [ggplot2::ggplot] object of the circular bar plot.
#' @export
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows group_by mutate rowwise summarize
#' @importFrom ggplot2 aes annotate coord_polar element_blank geom_abline geom_bar geom_segment
#' @importFrom ggplot2 geom_text ggplot guides labs scale_fill_manual theme theme_minimal unit ylim
#' @importFrom stats na.omit
#' @importFrom tibble as_tibble tibble
#'
#' @examples
#' # DISCLAIMER: THIS SOLUTION IS NOT ACTUALLY RUN WITH THESE TARGETS YET
#'
#' \dontrun{
#'
#' dat_problem <- prioritizr::problem(dat_species_bin %>% dplyr::mutate(Cost = runif(n = dim(.)[[1]])),
#'   features = c("Spp1", "Spp2", "Spp3", "Spp4", "Spp5"),
#'   cost_column = "Cost"
#' ) %>%
#'   prioritizr::add_min_set_objective() %>%
#'   prioritizr::add_relative_targets(0.3) %>%
#'   prioritizr::add_binary_decisions() %>%
#'   prioritizr::add_default_solver(verbose = FALSE)
#'
#' dat_soln <- dat_problem %>%
#'   prioritizr::solve.ConservationProblem()
#'
#' s1 <- dat_soln %>%
#'   tibble::as_tibble()
#'
#' p1 <- dat_problem
#'
#' # Assuming eval_feature_representation_summary is from prioritizr
#' df_rep_imp <- prioritizr::eval_feature_representation_summary(
#'   p1,
#'   s1[, "solution_1"]
#' ) %>%
#'   dplyr::select(feature, relative_held) %>%
#'   dplyr::mutate(relative_held = relative_held * 100)
#'
#' imp_layers <- c("Spp1", "Spp3")
#'
#' target <- data.frame(feature = c("Spp1", "Spp2", "Spp3", "Spp4", "Spp5")) %>%
#'   dplyr::mutate(class = dplyr::if_else(.data$feature %in% imp_layers,
#'     "important", "representative"
#'   )) %>%
#'   dplyr::mutate(target = dplyr::if_else(class == "important",
#'     50 / 100, 30 / 100
#'   ))
#'
#' df <- merge(df_rep_imp, target) %>%
#'   dplyr::select(-target) %>%
#'   stats::na.omit() %>% # Use stats::na.omit
#'   dplyr::rename(value = relative_held) %>%
#'   dplyr::rename(group = class)
#'
#' colors <- c(
#'   "important" = "darkgreen",
#'   "representative" = "darkred"
#' )
#' legends <- c("Important", "Representative")
#'
#' (splnr_plot_circBplot(df,
#'   legend_list = legends,
#'   legend_color = colors,
#'   impTarget = 50, repTarget = 30
#' ))
#' }
splnr_plot_circBplot <- function(df, legend_color, legend_list,
                                 indicateTargets = TRUE, impTarget = NA,
                                 repTarget = NA, colTarget = "red") {

  # assertthat checks for initial inputs
  assertthat::assert_that(
    inherits(df, c("data.frame", "tbl_df")),
    "feature" %in% colnames(df),
    "value" %in% colnames(df),
    "group" %in% colnames(df),
    is.numeric(df$value),
    msg = "Input 'df' must be a data frame or tibble with 'feature', 'value', and 'group' columns, and 'value' must be numeric."
  )

  assertthat::assert_that(
    is.vector(legend_color) && !is.null(names(legend_color)),
    all(unique(df$group) %in% names(legend_color)),
    msg = "'legend_color' must be a named vector where names match unique 'group' values in 'df'."
  )

  assertthat::assert_that(
    is.character(legend_list),
    length(unique(names(legend_color))) == length(legend_list),
    msg = "'legend_list' must be a character vector with the same number of elements as unique names in 'legend_color'."
  )

  assertthat::assert_that(
    is.logical(indicateTargets),
    is.character(colTarget),
    msg = "'indicateTargets' must be logical and 'colTarget' must be a character string."
  )

  if (indicateTargets) {
    assertthat::assert_that(
      is.numeric(impTarget) && length(impTarget) == 1 && !is.na(impTarget),
      is.numeric(repTarget) && length(repTarget) == 1 && !is.na(repTarget),
      msg = "When 'indicateTargets' is TRUE, 'impTarget' and 'repTarget' must be single non-NA numeric values."
    )
  }

  # Adding rows to each group, creating space between the groups
  groups <- unique(df$group)
  NA_rows <- list()
  for (i in 1:length(groups)) {
    NA_rows[[i]] <- data.frame(feature = NA, value = 0, group = groups[i])
  }

  data <- df %>%
    dplyr::bind_rows(do.call(dplyr::bind_rows, NA_rows)) %>%
    dplyr::group_by(.data$group) %>%
    dplyr::arrange(.data$feature)

  # Set a number of 'empty bar' to add at the end of each group
  empty_bar <- 2
  to_add <- data.frame(matrix(NA, empty_bar * length(unique(data$group)), ncol(data)))
  colnames(to_add) <- colnames(data)
  to_add$group <- rep(levels(as.factor(data$group)), each = empty_bar)
  data <- rbind(data, to_add)
  data <- data %>% dplyr::arrange(.data$group)
  data$id <- seq(1, nrow(data))

  # Labels for each of the bars (features)

  # Get the name and the y position of each label
  label_data <- data
  # Calculate the angle of the labels
  number_of_bar <- nrow(label_data)
  angle <- 90 - 360 * (label_data$id - 0.5) / number_of_bar # Subtracting 0.5 so the labels are not found in the extreme left or right
  # Calculate the alignment of labels: right or left
  # If I am on the left part of the plot, my labels have currently an angle < -90
  label_data$hjust <- ifelse(angle < -90, 1, 0)
  # Flip angle BY to make them readable
  label_data$angle <- ifelse(angle < -90, angle + 180, angle)

  # For the percentage lines
  grid_data <- data %>%
    dplyr::group_by(.data$group) %>%
    dplyr::summarize(start = min(.data$id), end = max(.data$id) - empty_bar, .groups = "drop") %>% # Added .groups = "drop" for dplyr > 1.0.0
    dplyr::rowwise() %>%
    dplyr::mutate(title = mean(c(.data$start, .data$end)))
  grid_data$end <- grid_data$end[c(nrow(grid_data), 1:nrow(grid_data) - 1)] + 1.5
  grid_data$start <- grid_data$end - 1
  grid_data <- grid_data[-1, ]

  # Make the plot
  p <- ggplot2::ggplot(data, ggplot2::aes(x = as.factor(.data$id), y = .data$value, fill = .data$group)) +

    # plotting the bars
    ggplot2::geom_bar(
      ggplot2::aes(x = as.factor(.data$id), y = .data$value, fill = .data$group),
      stat = "identity",
      position = "dodge"
    ) +

    # defining colors of the bars
    ggplot2::scale_fill_manual(
      name = "Features",
      values = legend_color,
      labels = legend_list
    ) +

    # Add text showing the value of each 100/75/50/25 lines
    ggplot2::geom_segment(
      data = grid_data,
      ggplot2::aes(x = .data$end, y = 25, xend = .data$start, yend = 25),
      colour = "grey50",
      alpha = 1,
      linewidth = 0.5,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_segment(
      data = grid_data,
      ggplot2::aes(x = .data$end, y = 50, xend = .data$start, yend = 50),
      colour = "grey50",
      alpha = 1,
      linewidth = 0.5,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_segment(
      data = grid_data,
      ggplot2::aes(x = .data$end, y = 75, xend = .data$start, yend = 75),
      colour = "grey50",
      alpha = 1,
      linewidth = 0.5,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_segment(
      data = grid_data,
      ggplot2::aes(x = .data$end, y = 100, xend = .data$start, yend = 100),
      colour = "grey50",
      alpha = 1,
      linewidth = 0.5,
      inherit.aes = FALSE
    ) +
    ggplot2::annotate("text",
                      x = rep(max(data$id - 1), 4),
                      y = c(25, 50, 75, 100),
                      label = c(25, 50, 75, 100),
                      color = "grey50",
                      size = 4,
                      angle = 0, # -5
                      fontface = "bold",
                      hjust = 0.5
    ) +

    # setting limitations of actual plot
    ggplot2::ylim(-130, 130) + # -140, 130
    ggplot2::theme_minimal() +
    ggplot2::coord_polar() +
    ggplot2::geom_text(
      data = label_data, ggplot2::aes(
        x = .data$id, y = .data$value + 10, label = .data$feature,
        hjust = .data$hjust
      ), color = "black",
      fontface = "bold", alpha = 0.6, size = 2.5, angle = label_data$angle,
      inherit.aes = FALSE
    ) +

    ggplot2::theme(
      legend.position = "bottom",
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      plot.margin = ggplot2::unit(rep(0.5, 4), "cm")
    )

  if (indicateTargets == TRUE) {
    # assertthat check ensures impTarget and repTarget are non-NA numeric here
    p <- p +
      ggplot2::geom_abline(slope = 0, intercept = impTarget, col = colTarget, lty = 2) +
      ggplot2::geom_abline(slope = 0, intercept = repTarget, col = colTarget, lty = 2)
  }
  return(p)
}
