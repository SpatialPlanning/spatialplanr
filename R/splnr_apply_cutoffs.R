#' @title Apply Cutoffs to Feature Data
#'
#' @description
#' `splnr_apply_cutoffs()` transforms numeric feature data in an `sf` dataframe
#' into binary (0 or 1) presence/absence values based on specified cutoffs.
#' It provides flexibility to either keep values above a cutoff as 1 (default)
#' or invert this logic to keep values below a cutoff as 1.
#'
#' @details
#' This function is crucial for standardizing feature data, such as species
#' probability distributions or habitat suitability scores, into a binary format
#' often required for conservation planning and spatial analysis (e.g., in
#' `prioritizr`).
#'
#' The function operates in two primary modes based on the `Cutoffs` parameter:
#' \itemize{
#'   \item \strong{Single Cutoff:} If `Cutoffs` is a single numeric value (e.g., `0.5`),
#'         this value is applied uniformly to \strong{all numeric columns} in the
#'         `features` dataframe, excluding the `geometry` column.
#'         For each numeric cell:
#'         - If `value >= Cutoffs`, it becomes `1`.
#'         - If `value < Cutoffs`, it becomes `0`.
#'         - `NA` values are always converted to `0`.
#'   \item \strong{Named Vector of Cutoffs:} If `Cutoffs` is a named numeric vector
#'         (e.g., `c("feature1" = 0.5, "feature2" = 0.3)`), each specified cutoff
#'         is applied individually to its corresponding named column in `features`.
#'         This allows for different thresholds for different features. The same
#'         transformation rules as above apply to each specified column.
#' }
#'
#' The `inverse` parameter provides additional control over the binarization:
#' \itemize{
#'   \item `inverse = FALSE` (default): Values \strong{at or above} the cutoff become `1`.
#'   \item `inverse = TRUE`: Values \strong{below} the cutoff become `1`. After initial
#'         binarization (where values >= cutoff are 1), the binary results are
#'         flipped (0s become 1s, and 1s become 0s) to achieve the inverse effect.
#' }
#' All `NA` values in the numeric columns are consistently converted to `0` during
#' the binarization process, regardless of the `inverse` setting.
#'
#' @param features An `sf` dataframe. It must contain a `geometry` column and
#'   at least one numeric column to which cutoffs will be applied.
#' @param Cutoffs A numeric value or a named numeric vector of cutoffs.
#'   \itemize{
#'     \item If a single unnamed numeric value, it's applied to all numeric columns.
#'     \item If a named numeric vector, names must correspond to numeric column names in `features`.
#'   }
#'   All cutoff values must be between `0` and `1`.
#' @param inverse A logical value (`TRUE` or `FALSE`). If `TRUE`, values below
#'   the `Cutoffs` are converted to `1` (and others to `0`). If `FALSE` (default),
#'   values at or above the `Cutoffs` are converted to `1`.
#'
#' @return A modified `sf` dataframe with the same structure and geometry as
#'   `features`, but with all targeted numeric columns transformed into binary
#'   (0 or 1) values based on the specified cutoffs and `inverse` setting.
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr across case_when mutate any_of
#' @importFrom rlang .data sym :=
#' @importFrom sf st_as_sf st_geometry
#' @importFrom tibble as_tibble
#'
#' @examples
#'
#' # Example 1: Single cutoff (0.5) applied to all numeric feature columns
#' # (Spp1_Prob, Spp2_Prob, and Cost will be binarized based on 0.5)
#' df_single_cutoff <- splnr_apply_cutoffs(dat_species_prob, Cutoffs = 0.5)
#' print(df_single_cutoff)
#'
#' # Example 2: Named cutoffs for specific columns
#' # Spp1_Prob >= 0.6 becomes 1, Spp2_Prob >= 0.4 becomes 1
#' df_named_cutoffs <- splnr_apply_cutoffs(
#'   dat_species_prob,
#'   Cutoffs = c("Spp1" = 0.6, "Spp2" = 0.4)
#' )
#' print(df_named_cutoffs)
#'
#' # Example 3: Single cutoff (0.5) with inverse logic
#' # Values BELOW 0.5 become 1.
#' df_inverse_cutoff <- splnr_apply_cutoffs(dat_species_prob, Cutoffs = 0.5, inverse = TRUE)
#' print(df_inverse_cutoff)
#'
#' # Example 4: Named cutoffs with inverse logic
#' df_named_inverse <- splnr_apply_cutoffs(
#'   dat_species_prob,
#'   Cutoffs = c("Spp1" = 0.7, "Spp2" = 0.3),
#'   inverse = TRUE
#' )
#' print(df_named_inverse)
splnr_apply_cutoffs <- function(features, Cutoffs, inverse = FALSE) {

  # --- Input Assertions ---
  # Ensure 'features' is an sf object.
  assertthat::assert_that(
    inherits(features, "sf"),
    msg = "Input 'features' must be an 'sf' object."
  )
  # Ensure 'Cutoffs' is numeric.
  assertthat::assert_that(
    is.numeric(Cutoffs),
    msg = "'Cutoffs' must be a numeric value or a named numeric vector."
  )
  # Ensure 'inverse' is a single logical value.
  assertthat::assert_that(
    is.logical(inverse) && length(inverse) == 1,
    msg = "'inverse' must be a single logical value (TRUE or FALSE)."
  )
  # Ensure all cutoff values are between 0 and 1.
  assertthat::assert_that(
    all(Cutoffs >= 0) && all(Cutoffs <= 1),
    msg = "All 'Cutoffs' values must be between 0 and 1 (inclusive)."
  )
  # Ensure 'features' dataframe contains a 'geometry' column.
  assertthat::assert_that(
    "geometry" %in% names(features),
    msg = "The 'features' dataframe must contain a 'geometry' column."
  )
  # Ensure the features dataframe is not empty.
  assertthat::assert_that(
    nrow(features) > 0,
    msg = "The 'features' dataframe must not be empty."
  )

  # Get the names of all numeric columns, excluding the geometry column.
  # This ensures that only relevant feature data columns are processed.
  numeric_cols <- names(features)[sapply(features, is.numeric) & names(features) != "geometry"]

  # Assert that there are numeric columns to operate on.
  assertthat::assert_that(
    length(numeric_cols) > 0,
    msg = "No numeric columns found in 'features' excluding geometry. Nothing to apply cutoffs to."
  )

  # Check if 'Cutoffs' is a named vector, and if so, validate that its names
  # correspond to existing numeric feature columns.
  if (!is.null(names(Cutoffs))) {
    assertthat::assert_that(
      all(names(Cutoffs) %in% numeric_cols),
      msg = "When 'Cutoffs' is a named vector, all names must match existing numeric feature column names in 'features'."
    )
  }

  # Convert sf object to tibble for data manipulation, then back to sf at the end.
  # This can sometimes prevent unexpected behavior with sf objects during extensive dplyr operations.
  features_as_tibble <- features %>%
    tibble::as_tibble()

  # --- Apply Cutoffs Logic ---

  # Case 1: Single cutoff value (unnamed vector of length 1).
  if (length(Cutoffs) == 1 && is.null(names(Cutoffs))) {
    message(paste0("Applying single cutoff of ", Cutoffs, " to all numeric feature columns."))

    # Apply the single cutoff to all identified numeric columns.
    # Values greater than or equal to the cutoff become 1, others become 0.
    # NAs are explicitly converted to 0.
    features_as_tibble <- features_as_tibble %>%
      dplyr::mutate(dplyr::across(
        dplyr::all_of(numeric_cols), # Apply only to the identified numeric columns.
        ~ dplyr::case_when(
          . >= Cutoffs ~ 1,
          . < Cutoffs ~ 0,
          is.na(.) ~ 0 # Handle NAs by converting them to 0.
        )
      ))

    # If 'inverse' is TRUE, flip the binary values (0s become 1s, 1s become 0s).
    if (inverse == TRUE) {
      message("Inverse logic applied: values below cutoff will be 1.")
      features_as_tibble <- features_as_tibble %>%
        dplyr::mutate(dplyr::across(dplyr::all_of(numeric_cols), ~ 1 - .))
    }

  } else if (length(Cutoffs) == length(names(Cutoffs))) {
    # Case 2: Named vector of cutoffs (each cutoff applies to a specific column).
    message("Applying named cutoffs to specific feature columns.")

    # Iterate through each named cutoff.
    for (col_name in names(Cutoffs)) {
      current_cutoff <- Cutoffs[col_name] # Get the cutoff value for the current column.
      message(paste0("  Applying cutoff ", current_cutoff, " to column '", col_name, "'."))

      # Apply the specific cutoff to the current column.
      # Values greater than or equal to the column's cutoff become 1, others become 0.
      # NAs are explicitly converted to 0.
      features_as_tibble <- features_as_tibble %>%
        dplyr::mutate(!!rlang::sym(col_name) := dplyr::case_when(
          !!rlang::sym(col_name) >= current_cutoff ~ 1,
          !!rlang::sym(col_name) < current_cutoff ~ 0,
          is.na(!!rlang::sym(col_name)) ~ 0 # Handle NAs by converting them to 0.
        ))

      # If 'inverse' is TRUE, flip the binary values for the current column.
      if (inverse == TRUE) {
        message(paste0("  Inverse logic applied for column '", col_name, "': values below cutoff will be 1."))
        features_as_tibble <- features_as_tibble %>%
          dplyr::mutate(!!rlang::sym(col_name) := 1 - !!rlang::sym(col_name))
      }
    }
  } else {
    # This scenario should ideally be caught by initial assertions, but included for robustness.
    stop("Invalid 'Cutoffs' parameter. It must be a single numeric value or a named numeric vector where names match feature columns.")
  }

  # Convert the modified tibble back to an sf object, preserving the original geometry.
  # Assuming the geometry column was preserved as is in 'features_as_tibble'.
  final_sf_features <- features_as_tibble %>%
    sf::st_as_sf()

  return(final_sf_features)
}
