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
#' The function operates in four modes based on the `Cutoffs` parameter:
#' \itemize{
#'   \item \strong{Single numeric scalar:} A single unnamed numeric value (e.g., `0.5`)
#'         is applied uniformly to \strong{all numeric columns} in `features`,
#'         excluding the `geometry` column.
#'   \item \strong{Single function:} A single unnamed function (e.g.,
#'         `\(x) quantile(x, 0.99)`) is called independently for each numeric
#'         column, with `x` being the non-`NA` values of that column. The
#'         returned scalar is then used as the threshold for that column.
#'         Because the function is evaluated per-column, different columns may
#'         receive different thresholds even though the same function is supplied.
#'   \item \strong{Named numeric vector:} A named numeric vector
#'         (e.g., `c("feature1" = 0.5, "feature2" = 0.3)`) applies each value
#'         to its corresponding named column only.
#'   \item \strong{Named list of numerics and/or functions:} A named list
#'         (e.g., `list("feature1" = 0.5, "feature2" = \(x) quantile(x, 0.99))`)
#'         applies each entry to its corresponding named column. Numeric entries
#'         are used directly; function entries are called with the non-`NA`
#'         values of that column and must return a single numeric in `[0, 1]`.
#' }
#'
#' For all modes, the binarisation rules are:
#' \itemize{
#'   \item If `value >= threshold`, it becomes `1`.
#'   \item If `value < threshold`, it becomes `0`.
#'   \item `NA` values are always converted to `0`.
#' }
#'
#' The `inverse` parameter flips the result after binarisation:
#' \itemize{
#'   \item `inverse = FALSE` (default): values \strong{at or above} the threshold
#'         become `1`.
#'   \item `inverse = TRUE`: values \strong{below} the threshold become `1`.
#' }
#'
#' All resolved threshold values (whether supplied directly or returned by a
#' function) must lie in `[0, 1]`. `NA` values are stripped from the column
#' vector before it is passed to a function-based cutoff.
#'
#' @param features An `sf` dataframe. It must contain a `geometry` column and
#'   at least one numeric column to which cutoffs will be applied.
#' @param Cutoffs One of:
#'   \itemize{
#'     \item A single unnamed numeric value in `[0, 1]` — applied to all
#'           numeric columns.
#'     \item A single unnamed function that accepts a numeric vector and returns
#'           a single numeric in `[0, 1]` — called independently per column
#'           with the non-`NA` values of that column.
#'     \item A named numeric vector — names must match numeric column names in
#'           `features`; each value is applied to its named column only.
#'     \item A named list of numerics and/or functions — names must match
#'           numeric column names in `features`; each entry is applied to its
#'           named column only.
#'   }
#' @param inverse A logical value (`TRUE` or `FALSE`). If `TRUE`, values below
#'   the threshold are converted to `1` (and others to `0`). If `FALSE`
#'   (default), values at or above the threshold are converted to `1`.
#'
#' @return A modified `sf` dataframe with the same structure and geometry as
#'   `features`, but with all targeted numeric columns transformed into binary
#'   (0 or 1) values based on the specified cutoffs and `inverse` setting.
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr across case_when mutate any_of all_of
#' @importFrom purrr map_lgl map_dbl
#' @importFrom rlang .data sym :=
#' @importFrom sf st_as_sf st_drop_geometry
#' @importFrom tibble as_tibble
#'
#' @examples
#'
#' # Example 1: Single numeric cutoff applied to all numeric feature columns
#' df_single_cutoff <- splnr_apply_cutoffs(dat_species_prob, Cutoffs = 0.5)
#' print(df_single_cutoff)
#'
#' # Example 2: Single function cutoff applied to all numeric feature columns
#' # Each column independently receives the 99th-percentile of its own values
#' # as its threshold.
#' df_fn_cutoff <- splnr_apply_cutoffs(
#'   dat_species_prob,
#'   Cutoffs = \(x) quantile(x, 0.99)
#' )
#' print(df_fn_cutoff)
#'
#' # Example 3: Named numeric cutoffs for specific columns
#' df_named_cutoffs <- splnr_apply_cutoffs(
#'   dat_species_prob,
#'   Cutoffs = c("Spp1" = 0.6, "Spp2" = 0.4)
#' )
#' print(df_named_cutoffs)
#'
#' # Example 4: Named list mixing fixed and function-based cutoffs
#' df_mixed_cutoffs <- splnr_apply_cutoffs(
#'   dat_species_prob,
#'   Cutoffs = list(
#'     "Spp1" = 0.5,
#'     "Spp2" = \(x) quantile(x, 0.99)
#'   )
#' )
#' print(df_mixed_cutoffs)
#'
#' # Example 5: Single numeric cutoff with inverse logic
#' df_inverse_cutoff <- splnr_apply_cutoffs(dat_species_prob, Cutoffs = 0.5, inverse = TRUE)
#' print(df_inverse_cutoff)
splnr_apply_cutoffs <- function(features, Cutoffs, inverse = FALSE) {

  # --- Input Assertions ---

  assertthat::assert_that(
    inherits(features, "sf"),
    msg = "Input 'features' must be an 'sf' object."
  )

  assertthat::assert_that(
    is.numeric(Cutoffs) || is.function(Cutoffs) || is.list(Cutoffs),
    msg = paste0(
      "'Cutoffs' must be a numeric scalar, a function, a named numeric vector, ",
      "or a named list of numerics and/or functions."
    )
  )

  assertthat::assert_that(
    is.logical(inverse) && length(inverse) == 1,
    msg = "'inverse' must be a single logical value (TRUE or FALSE)."
  )

  assertthat::assert_that(
    "geometry" %in% names(features),
    msg = "The 'features' dataframe must contain a 'geometry' column."
  )

  assertthat::assert_that(
    nrow(features) > 0,
    msg = "The 'features' dataframe must not be empty."
  )

  # Identify numeric columns, excluding geometry.
  # We work from the *dropped* frame so that names() and the logical map_lgl()
  # result are the same length — indexing names(features) with a logical vector
  # from st_drop_geometry() would be a length mismatch because names(features)
  # includes "geometry" while the dropped frame does not.
  features_plain <- sf::st_drop_geometry(features)
  numeric_cols <- names(features_plain)[purrr::map_lgl(features_plain, is.numeric)]

  assertthat::assert_that(
    length(numeric_cols) > 0,
    msg = "No numeric columns found in 'features' excluding geometry. Nothing to apply cutoffs to."
  )

  # For named vectors/lists, validate that all names exist as numeric columns.
  if (!is.null(names(Cutoffs))) {
    assertthat::assert_that(
      all(names(Cutoffs) %in% numeric_cols),
      msg = paste0(
        "When 'Cutoffs' is a named vector or list, all names must match ",
        "existing numeric feature column names in 'features'. ",
        "Unrecognised names: ",
        paste(setdiff(names(Cutoffs), numeric_cols), collapse = ", ")
      )
    )
  }

  # --- Helper: resolve a single cutoff entry to a scalar threshold ----------
  #
  # Why a helper? The resolution logic (numeric pass-through vs. function call
  # + validation) is identical whether we are in the "single" or "named" branch.
  # Extracting it avoids duplication and makes each branch easy to read.
  #
  # col_values: the raw (possibly NA-containing) column vector.
  # entry:      one element of Cutoffs — either a numeric(1) or a function.
  # col_name:   used only for error messages.

  resolve_cutoff <- function(entry, col_values, col_name) {

    if (is.numeric(entry)) {

      assertthat::assert_that(
        length(entry) == 1,
        msg = paste0(
          "Numeric cutoff for column '", col_name,
          "' must be a single value, not a vector of length ", length(entry), "."
        )
      )
      threshold <- entry

    } else if (is.function(entry)) {

      # Strip NAs before passing to the user's function so that common
      # aggregation functions (quantile, mean, etc.) work without the user
      # needing to remember na.rm = TRUE.
      clean_values <- col_values[!is.na(col_values)]

      threshold <- tryCatch(
        entry(clean_values),
        error = function(e) {
          stop(
            "Function-based cutoff for column '", col_name,
            "' raised an error: ", conditionMessage(e),
            call. = FALSE
          )
        }
      )

      assertthat::assert_that(
        is.numeric(threshold) && length(threshold) == 1 && is.finite(threshold),
        msg = paste0(
          "Function-based cutoff for column '", col_name,
          "' must return a single finite numeric value. ",
          "Got: ", deparse(threshold)
        )
      )

    } else {
      stop(
        "Each entry in 'Cutoffs' must be a numeric scalar or a function. ",
        "Entry for column '", col_name, "' is of class: ",
        paste(class(entry), collapse = "/"),
        call. = FALSE
      )
    }

    # Enforce [0, 1] on the resolved threshold regardless of source.
    assertthat::assert_that(
      threshold >= 0 && threshold <= 1,
      msg = paste0(
        "Resolved cutoff for column '", col_name, "' is ", threshold,
        ", which is outside the required [0, 1] range."
      )
    )

    threshold
  }

  # --- Determine which columns to process and their thresholds --------------
  #
  # We build a named numeric vector `thresholds` mapping column name ->
  # resolved scalar threshold. This unifies all four input modes into a single
  # downstream binarisation step, avoiding duplicated case_when logic.
  #
  # purrr::map_dbl() is used in preference to vapply() for consistency with
  # the tidyverse style used throughout this package.

  if (is.function(Cutoffs)) {
    # Single function: apply independently to every numeric column.
    message("Applying function-based cutoff independently to each numeric feature column.")
    thresholds <- purrr::map_dbl(
      stats::setNames(numeric_cols, numeric_cols),
      function(col) {
        thresh <- resolve_cutoff(Cutoffs, features_plain[[col]], col)
        message("  Column '", col, "': resolved threshold = ", thresh)
        thresh
      }
    )

  } else if (is.numeric(Cutoffs) && is.null(names(Cutoffs)) && length(Cutoffs) == 1) {
    # Single numeric scalar: same threshold for every numeric column.
    # Validate once against the first column (value check only; column data
    # is irrelevant for a numeric entry but resolve_cutoff requires it).
    message("Applying single cutoff of ", Cutoffs, " to all numeric feature columns.")
    resolve_cutoff(Cutoffs, features_plain[[numeric_cols[1]]], numeric_cols[1])
    thresholds <- stats::setNames(rep(Cutoffs, length(numeric_cols)), numeric_cols)

  } else {
    # Named numeric vector or named list: per-column thresholds.
    # Convert a named numeric vector to a list so resolve_cutoff handles both
    # uniformly — as.list() on a named numeric vector gives a named list of
    # numeric(1) scalars, which is.numeric() correctly identifies.
    message("Applying named cutoffs to specific feature columns.")
    cutoffs_list <- if (is.list(Cutoffs)) Cutoffs else as.list(Cutoffs)

    thresholds <- purrr::map_dbl(
      stats::setNames(names(cutoffs_list), names(cutoffs_list)),
      function(col) {
        thresh <- resolve_cutoff(cutoffs_list[[col]], features_plain[[col]], col)
        message("  Column '", col, "': resolved threshold = ", thresh)
        thresh
      }
    )
  }

  # --- Apply binarisation ---------------------------------------------------
  #
  # Convert to tibble for dplyr operations, then restore sf at the end.
  # We iterate over the resolved thresholds with a for-loop rather than
  # purrr::reduce() because each column receives a *different* threshold value
  # captured from the named vector. A for-loop is more readable here than
  # reduce() with a closure that captures the threshold by name.

  features_tbl <- tibble::as_tibble(features)

  for (col in names(thresholds)) {
    thresh <- thresholds[[col]]
    features_tbl <- features_tbl %>%
      dplyr::mutate(
        !!rlang::sym(col) := dplyr::case_when(
          is.na(!!rlang::sym(col))    ~ 0,
          !!rlang::sym(col) >= thresh ~ 1,
          TRUE                        ~ 0
        )
      )
  }

  # Apply inverse if requested: flip 0 <-> 1 on all processed columns.
  if (inverse) {
    message("Inverse logic applied: values below threshold will be 1.")
    features_tbl <- features_tbl %>%
      dplyr::mutate(
        dplyr::across(dplyr::all_of(names(thresholds)), ~ 1 - .)
      )
  }

  features_tbl %>%
    sf::st_as_sf()
}
