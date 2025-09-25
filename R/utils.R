
##### Utility Functions ####

#' @title Create Spatial Polygon from Coordinates
#'
#' @description
#' `splnr_create_polygon()` constructs an `sf` polygon object from a series
#' of longitude and latitude coordinates provided in a tibble.
#'
#' @details
#' This utility function simplifies the creation of spatial polygons from a
#' tabular format of coordinates. It takes a tibble where columns 'x' and 'y'
#' represent longitude and latitude, respectively. These coordinates are
#' converted into a matrix, then to an `sf` polygon, and finally to an `sf`
#' object with the specified Coordinate Reference System (CRS).
#'
#' The function assumes that the input coordinates (`x`) are initially in
#' WGS 84 (EPSG:4326) and then transforms them to the `cCRS` if a different
#' CRS is specified.
#'
#' @param x A `tibble` (or `tbl_df`) object with at least two columns,
#'   typically named `x` (for longitude) and `y` (for latitude), representing
#'   the vertices of the polygon in sequence. The first and last coordinate
#'   pair should be the same to form a closed polygon.
#' @param cCRS A character string specifying the target CRS for the output polygon
#'   in an EPSG code format (e.g., "EPSG:4326"). Defaults to "EPSG:4326" (WGS 84).
#'
#' @return An `sf` object representing the created polygon, with the specified CRS.
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows tibble
#' @importFrom sf st_polygon st_sfc st_transform st_sf
#'
#' @examples
#' # Example: Create a simple square polygon
#' square_coords <- dplyr::tibble(
#'   x = c(-50, 50, 50, -50, -50),
#'   y = c(120, 120, 180, 180, 120)
#' )
#' simple_polygon <- splnr_create_polygon(x = square_coords)
#' print(simple_polygon)
#'
#' # Example: Create a polygon and transform to a different CRS (e.g., a UTM zone)
#' \dontrun{
#' # Note: EPSG:32611 is UTM Zone 11N. Ensure it's appropriate for your coordinates.
#' transformed_polygon <- splnr_create_polygon(x = square_coords, cCRS = "EPSG:32611")
#' print(transformed_polygon)
#' }
splnr_create_polygon <- function(x, cCRS = "EPSG:4326") {

  # Assertions to validate input parameters.
  assertthat::assert_that(
    inherits(x, "data.frame") && !is.null(x$x) && !is.null(x$y),
    msg = "'x' must be a data.frame (or tibble) with 'x' and 'y' columns representing coordinates."
  )
  assertthat::assert_that(
    is.numeric(x$x) && is.numeric(x$y),
    msg = "Coordinates 'x' and 'y' in the input data frame must be numeric."
  )
  assertthat::assert_that(
    is.character(cCRS) && length(cCRS) == 1,
    msg = "'cCRS' must be a single character string specifying the CRS (e.g., 'EPSG:4326')."
  )

  # Convert the input tibble to a matrix, then to a list, which is the required
  # format for sf::st_polygon.
  # st_polygon expects a list of matrices, where each matrix defines a linear ring.
  polygon_matrix <- x %>%
    as.matrix() %>%
    list()

  # Create an sf polygon object from the matrix, then create an sfc (simple feature column)
  # with an initial CRS of EPSG:4326 (WGS 84 assumed for input lat/lon).
  polygon_sfc <- sf::st_polygon(polygon_matrix) %>%
    sf::st_sfc(crs = "EPSG:4326")

  # Transform the polygon to the target CRS specified by cCRS.
  # This is crucial for ensuring the output polygon is in the desired projection.
  transformed_polygon_sfc <- polygon_sfc %>%
    sf::st_transform(crs = cCRS)

  # Convert the sfc object to an sf (simple features) object, which is a data frame
  # with a geometry column.
  final_sf_polygon <- transformed_polygon_sfc %>%
    sf::st_sf()

  return(final_sf_polygon)
}


#' @title Remove NAs from Spatial Data Using Nearest Neighbour
#'
#' @description
#' `splnr_replace_NAs()` replaces missing (NA) values in a specified column
#' of an `sf` dataframe with the value from the nearest spatial neighbor.
#'
#' @details
#' This function is useful for imputing missing data in spatial contexts.
#' It identifies all planning units with `NA` values in the `vari` column.
#' For each of these, it finds the geographically closest planning unit that
#' *does not* have an `NA` value in `vari`, and then copies that non-missing
#' value. This approach leverages the spatial autocorrelation often present
#' in environmental and species data.
#'
#' The `st_nearest_feature()` function from the `sf` package is used for
#' determining the closest neighbor.
#'
#' @param df An `sf` dataframe. This dataframe must contain a geometry column
#'   and the `vari` column with potential NA values.
#' @param vari A character string specifying the name of the column in `df`
#'   from which NA values are to be removed and replaced. This column must
#'   exist in `df`.
#'
#' @return An `sf` object identical to the input `df`, but with NA values
#'   in the `vari` column replaced by values from their nearest non-NA neighbors.
#'   If no NAs are found, the original `df` is returned unchanged.
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows mutate pull select
#' @importFrom rlang .data sym :=
#' @importFrom sf st_nearest_feature
#' @importFrom tibble rowid_to_column
#'
#' @examples
#' \dontrun{
#' # Assuming 'dat_species_prob' is an existing sf object in your package.
#' # For demonstration, let's artificially introduce some NAs in 'Spp2'.
#' df_with_na <- dat_species_prob %>%
#'   dplyr::mutate(Spp2 = ifelse(runif(n()) < 0.2, NA, Spp2))
#'
#' # Replace NAs in 'Spp2' using nearest neighbor imputation.
#' df_no_na <- splnr_replace_NAs(df = df_with_na, vari = "Spp2")
#' print(sum(is.na(df_no_na$Spp2))) # Should be 0 if successful
#' }
splnr_replace_NAs <- function(df, vari) {

  # Assertions to validate input parameters.
  assertthat::assert_that(
    inherits(df, "sf"), # Ensure df is an sf object.
    msg = "'df' must be an 'sf' object."
  )
  assertthat::assert_that(
    is.character(vari) && length(vari) == 1,
    msg = "'vari' must be a single character string specifying the column name."
  )
  assertthat::assert_that(
    vari %in% names(df),
    msg = paste0("Column '", vari, "' not found in the input dataframe 'df'.")
  )
  assertthat::assert_that(
    !is.null(sf::st_geometry(df)),
    msg = "'df' must have a geometry column."
  )

  # Check if there are any NA values in the specified variable.
  if (sum(is.na(dplyr::pull(df, !!rlang::sym(vari)))) > 0) {

    # Add a unique row ID and a logical column 'isna' to identify NA rows.
    # This 'cellID' is crucial for reordering the dataframe correctly at the end.
    gp <- df %>%
      tibble::rowid_to_column("cellID") %>%
      dplyr::mutate(isna = is.na(!!rlang::sym(vari)))

    # Split the dataframe into two parts: those with NAs and those without.
    gp <- split(gp, f = as.factor(gp$isna))

    # Find the nearest feature (row) in the 'FALSE' group (no NAs) for each
    # feature (row) in the 'TRUE' group (with NAs).
    # 'd' will be a vector of indices corresponding to the nearest non-NA features.
    d <- sf::st_nearest_feature(gp$`TRUE`, gp$`FALSE`)

    # Replace the NA values in the 'TRUE' group with the corresponding values
    # from their nearest non-NA neighbors found in the 'FALSE' group.
    gp$`TRUE` <- gp$`TRUE` %>%
      dplyr::mutate(!!rlang::sym(vari) := dplyr::pull(gp$`FALSE`, !!rlang::sym(vari))[d])

    # Combine the 'FALSE' group (original non-NAs) and the modified 'TRUE' group (NAs replaced).
    # Then, remove the temporary 'isna' and 'cellID' columns and reorder by original 'cellID'.
    df <- rbind(gp$`FALSE`, gp$`TRUE`) %>%
      dplyr::select(-"isna") %>%
      dplyr::arrange(.data$cellID) %>%
      dplyr::select(-"cellID") # Remove the temporary cellID column.
  }
  return(df)
}


#' @title Substitute Numbers for Names in Regionalizations
#'
#' @description
#' `splnr_match_names()` replaces numeric or integer values in a spatial
#' (sf) dataframe's column with corresponding character names, typically used
#' for regionalization data.
#'
#' @details
#' This function is designed for scenarios where spatial data contains numeric
#' identifiers for regions, and you have a mapping (a named character vector)
#' to convert these IDs into more descriptive names. It assumes that the `sf`
#' dataframe (`dat`) has only one non-geometry column that needs recoding.
#'
#' The function directly applies the mapping from the `nam` vector to the
#' specified column. The names of the `nam` vector should correspond to the
#' numeric/integer values in the `dat` column, and the values of `nam` will
#' be the new character names.
#'
#' @param dat An `sf` data frame with a single non-geometry column containing
#'   numeric or integer values that correspond to the names in `nam`.
#' @param nam A named character vector. The *names* of this vector should be
#'   the numeric/integer values found in `dat`'s column, and the *values* of
#'   this vector should be the desired character names for substitution.
#'
#' @return An `sf` dataframe where the numeric/integer values in the relevant
#'   column have been substituted with the corresponding character names from `nam`.
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr mutate
#' @importFrom rlang := sym
#' @importFrom stringr str_subset
#' @importFrom sf st_drop_geometry
#'
#' @examples
#' # Define the named character vector for mapping.
#' region_names <- c("Region1" = "SE Aust", "Region2" = "Tas", "Region3" = "NE Aust")
#'
#' # Apply the function to substitute numeric codes with names.
#' df_named_regions <- splnr_match_names(dat = dat_region, nam = region_names)
#' print(df_named_regions)
splnr_match_names <- function(dat, nam) {

  # Assertions to validate input parameters.
  assertthat::assert_that(
    inherits(dat, "sf"),
    msg = "'dat' must be an 'sf' object."
  )
  assertthat::assert_that(
    is.character(nam) && length(nam) > 0,
    msg = "'nam' must be a non-empty character vector."
  )
  assertthat::assert_that(
    !is.null(names(nam)),
    msg = "'nam' must be a named character vector (e.g., c('1' = 'Name1'))."
  )
  assertthat::assert_that(
    length(colnames(dat %>% sf::st_drop_geometry())) == 1,
    msg = "'dat' must contain exactly one non-geometry column to be recoded."
  )
  # assertthat::assert_that(
  #   is.numeric(dat %>% sf::st_drop_geometry() %>% dplyr::pull(1)) || is.integer(dat %>% sf::st_drop_geometry() %>% dplyr::pull(1)),
  #   msg = "The non-geometry column in 'dat' must be numeric or integer."
  # )
  assertthat::assert_that(
    all(as.character(unique(dat %>% sf::st_drop_geometry() %>% dplyr::pull(1))) %in% names(nam)),
    msg = "Not all unique numeric/integer values in 'dat's recoding column are present as names in 'nam'."
  )


  # Identify the name of the single non-geometry column that needs recoding.
  col_name <- stringr::str_subset(colnames(dat), "geometry", negate = TRUE)[[1]]

  # Use dplyr::mutate to replace the numeric/integer values in 'col_name'
  # with the corresponding names from the 'nam' vector.
  # The `!!rlang::sym(col_name)` syntax ensures that 'col_name' is treated as a variable.
  out <- dat %>%
    dplyr::mutate(!!col_name := nam[as.character(!!rlang::sym(col_name))]) # Convert col_name content to character to match names(nam)

  return(out)
}


#' @title Scale Spatial Layers to Between 0 and 1
#'
#' @description
#' `splnr_scale_01()` re-scales the numeric values in a specified column of an
#' `sf` dataframe to a range between 0 and 1. This is particularly useful for
#' normalizing data like probabilities or costs.
#'
#' @details
#' This function inspects the maximum value (`mx`) in the `col_name` column.
#' It then divides all values in that column by a `divi` factor to bring them
#' into the 0-1 range. The `divi` factor is determined heuristically:
#' - If `mx > 100`, `divi` is `1000`.
#' - If `mx > 10`, `divi` is `100`.
#' - If `mx > 1`, `divi` is `10`.
#' - If `mx <= 1`, no division is performed (`divi` is `1`), as the data is
#'   already within the desired range.
#'
#' This approach ensures that the data is scaled appropriately without
#' hardcoding a fixed division factor.
#'
#' @param dat An `sf` dataframe containing the column to be scaled.
#' @param col_name A character string specifying the name of the numeric column
#'   in `dat` that needs to be scaled.
#'
#' @return An `sf` dataframe identical to the input `dat`, but with the values
#'   in the `col_name` column re-scaled to be between 0 and 1.
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr mutate pull
#' @importFrom rlang := sym
#'
#' @examples
#' \dontrun{

#' # Scale the 'Spp1' column.
#' df_scaled_spp1 <- splnr_scale_01(dat = dat_species_prob, col_name = "Spp1")
#' print(df_scaled_spp1)
#'
#' # Example where max is already <= 1
#' df_already_scaled <- dat_species_prob %>% dplyr::mutate(Spp1 = Spp1 / 100)
#' df_no_change <- splnr_scale_01(dat = df_already_scaled, col_name = "Spp1")
#' print(df_no_change) # Spp1 values should remain unchanged
#' }
splnr_scale_01 <- function(dat, col_name) {

  # Assertions to validate input parameters.
  assertthat::assert_that(
    inherits(dat, "sf"), # Ensure dat is an sf object.
    msg = "'dat' must be an 'sf' object."
  )
  assertthat::assert_that(
    is.character(col_name) && length(col_name) == 1,
    msg = "'col_name' must be a single character string."
  )
  assertthat::assert_that(
    col_name %in% names(dat),
    msg = paste0("Column '", col_name, "' not found in the input dataframe 'dat'.")
  )
  assertthat::assert_that(
    is.numeric(dplyr::pull(dat, !!rlang::sym(col_name))),
    msg = paste0("Column '", col_name, "' must be numeric to be scaled.")
  )

  # Get the maximum value in the specified column, ignoring NA values.
  mx <- max(dplyr::pull(dat, !!rlang::sym(col_name)), na.rm = TRUE)

  # Determine the division factor based on the maximum value.
  # This logic tries to scale the data into the 0-1 range.
  divi <- 1 # Default: no division if max is already 1 or less.
  if (mx > 100) {
    divi <- 1000
  } else if (mx > 10) {
    divi <- 100
  } else if (mx > 1) {
    divi <- 10
  }

  # Apply the scaling to the specified column.
  dat <- dat %>%
    dplyr::mutate(!!col_name := !!rlang::sym(col_name) / divi)

  return(dat)
}


#' @title Extract Feature Names from Spatial Data
#'
#' @description
#' `splnr_featureNames()` extracts the names of conservation features
#' from an `sf` dataframe, excluding geometry and any specified columns.
#'
#' @details
#' This function is a utility for preparing data for `prioritizr` or other
#' conservation planning packages that require a vector of feature names.
#' It typically removes the geometry column and any columns related to cost
#' (prefixed with "Cost_") by default, allowing you to specify additional
#' columns to exclude.
#'
#' The output is a simple character vector of column names, which can be
#' directly used as feature identifiers in conservation problems.
#'
#' @param dat An `sf` dataframe representing conservation features. Each
#'   non-geometry column is assumed to be a feature.
#' @param exclude A character vector of column names (or prefixes) to exclude
#'   from the output. By default, it excludes columns starting with "Cost_".
#'   If you provide a value, it will be *appended* to the default exclusion.
#'   Set to `NULL` or `character(0)` if you want no exclusions beyond the default.
#'
#' @return A character vector containing the names of the conservation features.
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr select
#' @importFrom sf st_drop_geometry
#' @importFrom tidyselect starts_with
#'
#' @examples
#' \dontrun{
#' # Assuming 'dat_species_prob' is an existing sf object in your package.
#' # It likely has columns like 'Spp1', 'Spp2', 'Cost_SomeMeasure', etc.
#'
#' # Example 1: Get all feature names, excluding default 'Cost_' columns.
#' feature_names_default <- splnr_featureNames(dat = dat_species_prob)
#' print(feature_names_default)
#'
#' # Example 2: Get feature names, excluding 'Cost_' columns and 'Spp5'.
#' feature_names_custom_exclude <- splnr_featureNames(
#'   dat = dat_species_prob,
#'   exclude = "Spp5"
#' )
#' print(feature_names_custom_exclude)
#'
#' # Example 3: If you only want to exclude a specific column and not 'Cost_'
#' # (you'd need to manually specify exclude = "geometry" and then your column)
#' # This case is more complex and usually handled by direct dplyr::select.
#' # This function's primary use is to remove cost columns and potentially others.
#' }
splnr_featureNames <- function(dat, exclude = NA) {

  # Assertions to validate input parameters.
  assertthat::assert_that(
    inherits(dat, "sf"),
    msg = "'dat' must be an 'sf' object."
  )
  assertthat::assert_that(
    is.character(exclude) || is.na(exclude),
    msg = "'exclude' must be a character vector or NA."
  )
  assertthat::assert_that(
    !is.null(sf::st_geometry(dat)),
    msg = "'dat' must have a geometry column."
  )

  # Define columns to exclude. Always start with "Cost_" as a default.
  if (all(is.na(exclude))) { # Check if `exclude` is literally `NA` (the default)
    exclude_cols <- c("Cost_")
  } else {
    # If `exclude` is provided and not NA, append it to "Cost_".
    # Ensure it's a character vector and handle potential NULL/empty cases.
    exclude_cols <- c("Cost_", exclude[is.na(exclude) == FALSE])
  }

  # Drop geometry, then select columns that do NOT start with any of the
  # prefixes in `exclude_cols`, and finally get the column names.
  feature_names <- dat %>%
    sf::st_drop_geometry() %>%
    dplyr::select(-tidyselect::starts_with(exclude_cols)) %>%
    colnames()

  return(feature_names)
}


#' @title Arrange Features by Spatial Coordinates
#'
#' @description
#' `splnr_arrangeFeatures()` sorts the rows of an `sf` object based on the
#' longitude (X) and then latitude (Y) of its centroids. This ensures a
#' consistent ordering of planning units, which can be important for
#' reproducibility in some spatial analyses or data processing steps.
#'
#' @details
#' This function computes the centroid for each polygon (or point/multipoint)
#' in the input `sf` object. It then extracts the X and Y coordinates of these
#' centroids and uses them to sort the entire `sf` object. The primary sort key
#' is the longitude (X-coordinate), and the secondary sort key is the latitude
#' (Y-coordinate).
#'
#' Sorting can be beneficial for tasks like debugging, comparing data from
#' different runs, or ensuring deterministic behavior in algorithms that
#' process spatial units sequentially.
#'
#' @param df An `sf` object whose rows are to be sorted.
#'
#' @return A sorted `sf` object, with rows ordered primarily by longitude (X)
#'   and secondarily by latitude (Y) of their centroids.
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom sf st_centroid st_coordinates
#'
#' @examples
#' \dontrun{

#' print("Original order:")
#' print(dat_species_prob)
#'
#' # Sort the features.
#' df_arranged <- splnr_arrangeFeatures(df = dat_species_prob)
#' print("Sorted order:")
#' print(df_arranged)
#' }
splnr_arrangeFeatures <- function(df) {

  # Assertions to validate input parameters.
  assertthat::assert_that(
    inherits(df, "sf"),
    msg = "'df' must be an 'sf' object."
  )
  assertthat::assert_that(
    !is.null(sf::st_geometry(df)),
    msg = "'df' must have a geometry column."
  )
  assertthat::assert_that(
    nrow(df) > 0,
    msg = "'df' must not be empty."
  )

  # Calculate the centroids of the geometries.
  # suppressWarnings is used here because st_centroid can sometimes issue warnings
  # for complex geometries (e.g., empty geometries or geometries spanning the antimeridian)
  # which might not be relevant for simple sorting.
  suppressWarnings({
    centroids <- sf::st_centroid(df)
    # Extract the X and Y coordinates from the centroids.
    xy <- sf::st_coordinates(centroids)
  })

  # Order the input dataframe based on the X (longitude) and then Y (latitude)
  # coordinates of its centroids. This provides a deterministic sort order.
  df_sorted <- df[order(xy[, "X"], xy[, "Y"]), ]

  return(df_sorted)
}


#' @title Prepare Data to Plot Cohen's Kappa Correlation Matrix
#'
#' @description
#' `splnr_get_kappaCorrData()` calculates Cohen's Kappa correlation coefficients
#' between a list of `prioritizr` conservation solutions. The output is a
#' symmetrical matrix suitable for visualizing pairwise agreement using a heatmap.
#'
#' @details
#' This function is essential for assessing the similarity or divergence among
#' different conservation plans. It takes a list of `prioritizr` solution objects,
#' each expected to contain a binary column named `solution_1` (indicating
#' selected or unselected planning units).
#'
#' For every unique pair of solutions in the input list, it computes Cohen's Kappa
#' using the `irr::kappa2()` function. Cohen's Kappa measures the agreement
#' between two raters (in this case, two conservation solutions) for categorical
#' items, correcting for chance agreement. A Kappa value of 1 indicates perfect
#' agreement, 0 indicates agreement equivalent to chance, and negative values
#' indicate agreement worse than chance.
#'
#' The resulting matrix is symmetrical, with diagonal elements always equal to 1
#' (a solution perfectly agrees with itself). This matrix can then be passed to
#' visualization functions like `splnr_plot_corrMat()` to create a correlation heatmap.
#'
#' @param sol A `list` of `prioritizr` solution objects. Each element in the list
#'   must be an `sf` object containing a binary column named `solution_1`.
#' @param name_sol A character vector providing descriptive names for each
#'   solution in the `sol` list. The length of this vector must match the
#'   length of `sol`. These names will be used as row and column names in the
#'   output correlation matrix.
#'
#' @return A numeric `matrix` (`matrixOut`) representing the Cohen's Kappa
#'   correlation matrix between all pairs of solutions. Rows and columns are
#'   named according to `name_sol`.
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_cols select
#' @importFrom rlang .data
#' @importFrom stats setNames
#' @importFrom tibble as_tibble
#' @importFrom tidyr pivot_wider
#'
#' @examples
#' \dontrun{
#' # Assuming 'dat_species_bin' is an existing sf object in your package.
#'
#' # Create a dummy prioritizr problem and solve it for solution 1 (30% target).
#' dat_problem1 <- prioritizr::problem(
#'   dat_species_bin %>% dplyr::mutate(Cost = runif(n = dim(.)[[1]])),
#'   features = c("Spp1", "Spp2", "Spp3", "Spp4", "Spp5"),
#'   cost_column = "Cost"
#' ) %>%
#'   prioritizr::add_min_set_objective() %>%
#'   prioritizr::add_relative_targets(0.3) %>%
#'   prioritizr::add_binary_decisions() %>%
#'   prioritizr::add_default_solver(verbose = FALSE)
#'
#' dat_soln1 <- dat_problem1 %>%
#'   prioritizr::solve.ConservationProblem()
#'
#' # Create another dummy prioritizr problem and solve it for solution 2 (50% target).
#' dat_problem2 <- prioritizr::problem(
#'   dat_species_bin %>%
#'     dplyr::mutate(Cost = runif(n = dim(.)[[1]])),
#'   features = c("Spp1", "Spp2", "Spp3", "Spp4", "Spp5"),
#'   cost_column = "Cost"
#' ) %>%
#'   prioritizr::add_min_set_objective() %>%
#'   prioritizr::add_relative_targets(0.5) %>%
#'   prioritizr::add_binary_decisions() %>%
#'   prioritizr::add_default_solver(verbose = FALSE)
#'
#' dat_soln2 <- dat_problem2 %>%
#'   prioritizr::solve.ConservationProblem()
#'
#' # Calculate the Cohen's Kappa correlation matrix between the two solutions.
#' corrMat <- splnr_get_kappaCorrData(
#'   sol = list(dat_soln1, dat_soln2),
#'   name_sol = c("Solution_A_30pct", "Solution_B_50pct")
#' )
#' print(corrMat)
#'
#' # This output can then be directly passed to splnr_plot_corrMat().
#' # splnr_plot_corrMat(corrMat, AxisLabels = c("Sol A (30%)", "Sol B (50%)"))
#' }
splnr_get_kappaCorrData <- function(sol, name_sol) {

  # Assertions to validate input parameters.
  assertthat::assert_that(
    is.list(sol),
    msg = "'sol' must be a list of prioritizr solution objects."
  )
  assertthat::assert_that(
    length(sol) > 1,
    msg = "'sol' list must contain at least two solutions for correlation."
  )
  assertthat::assert_that(
    is.character(name_sol) && length(name_sol) > 0,
    msg = "'name_sol' must be a non-empty character vector."
  )
  assertthat::assert_that(
    length(name_sol) == length(sol),
    msg = "The length of 'name_sol' must match the number of solutions in 'sol'."
  )
  # Check each solution in the list for 'solution_1' column and sf class
  for (i in seq_along(sol)) {
    assertthat::assert_that(
      inherits(sol[[i]], "sf"),
      msg = paste0("Element ", i, " in 'sol' is not an 'sf' object.")
    )
    assertthat::assert_that(
      "solution_1" %in% names(sol[[i]]),
      msg = paste0("Solution ", i, " in 'sol' is missing the 'solution_1' column.")
    )
  }

  # Check if 'irr' package is installed. If not, stop with an informative error.
  if (requireNamespace("irr", quietly = TRUE) == FALSE){
    stop("To run splnr_get_kappaCorrData you will need to install the 'irr' package: install.packages('irr').")
  }

  # Prepare a list of solutions, selecting only the 'solution_1' column and renaming it
  # with the provided 'name_sol'. Each element will be a tibble with one column.
  s_list <- lapply(seq_along(sol), function(x) {
    sol[[x]] %>%
      tibble::as_tibble(.name_repair = "unique") %>% # Convert to tibble, handling duplicate names.
      dplyr::select("solution_1") %>% # Select the binary solution column.
      stats::setNames(name_sol[[x]]) # Rename the column to the provided solution name.
  })

  # Initialize a list to store pairwise kappa results.
  # 'y' is a counter for storing results in s_matrix.
  y <- 1
  s_matrix <- list()
  # Loop through all unique pairs of solutions (including self-correlation).
  for (i in 1:length(s_list)) {
    for (j in 1:length(s_list)) {
      # Combine the two solutions (as single-column tibbles) side-by-side.
      combined_solutions <- dplyr::bind_cols(s_list[[i]], s_list[[j]])
      # Calculate Cohen's Kappa between the two solutions.
      kappa_temp <- irr::kappa2(combined_solutions)
      kappa_corrvalue <- kappa_temp$value # Extract the Kappa value.
      kappa_pvalue <- kappa_temp$p.value # Extract the p-value (though not used in final output matrix).

      # Store the pair's names, kappa value, and p-value.
      s_matrix[[y]] <- cbind(colnames(s_list[[i]]), colnames(s_list[[j]]), kappa_corrvalue, kappa_pvalue)
      y <- y + 1 # Increment counter.
    }
  }

  # Combine all pairwise results into a single tibble.
  s_matrix_all <- do.call(rbind, s_matrix) %>%
    tibble::as_tibble(.name_repair = "unique")
  # Rename the first two columns to be more descriptive.
  colnames(s_matrix_all)[1:2] <- c("plan1", "plan2")

  # Transform the long-format results into a wide-format correlation matrix.
  matrix_final <- s_matrix_all %>%
    tibble::as_tibble(.name_repair = "unique") %>%
    dplyr::select(-.data$kappa_pvalue) %>% # Remove p-value column as it's not needed for the correlation matrix plot.
    tidyr::pivot_wider(names_from = "plan2", values_from = "kappa_corrvalue") %>% # Pivot to make 'plan2' names as new columns.
    as.matrix() # Convert to matrix format.

  # Note: matrix_x was part of original code but not used for the return value.
  # matrix_x <- s_matrix_all %>%
  #   tibble::as_tibble(.name_repair = "unique")

  # Set row names of the final matrix for clarity.
  rownames(matrix_final) <- matrix_final[, 1]
  # Determine the number of columns to select (number of solutions + 1 for the first column).
  # The original code's 'n <- length(s_list) + 1' implies the first column is the row names,
  # so it selects from the second column onwards up to 'n'.
  n_cols_to_select <- length(s_list) + 1
  matrixOut <- matrix_final[, 2:n_cols_to_select] # Select only the numeric correlation values.
  class(matrixOut) <- "numeric" # Ensure the matrix is of numeric class.

  return(matrixOut)
}

#' @title Prepare Data to Plot Selection Frequency of Planning Units
#'
#' @description
#' `splnr_get_selFreq()` calculates how many times each planning unit is
#' selected across an array of `prioritizr` solutions. This "selection
#' frequency" can be derived from either a list of individual solutions or
#' a `prioritizr` portfolio object.
#'
#' @details
#' Understanding selection frequency is crucial for identifying robust
#' conservation areas—those that are consistently chosen across multiple
#' planning scenarios or alternative optimal solutions.
#'
#' The function supports two types of input:
#' \itemize{
#'   \item `"portfolio"`: If `solnMany` is a single `sf` object representing a
#'         portfolio of solutions (e.g., generated by `prioritizr::add_cuts_portfolio()`).
#'         In this case, the function assumes columns starting with "solution_"
#'         represent individual solutions within the portfolio.
#'   \item `"list"`: If `solnMany` is a `list` where each element is an `sf`
#'         object representing a single `prioritizr` solution (each with a
#'         "solution_1" column).
#' }
#' For both types, the function sums the binary `solution` values (0 or 1)
#' across all solutions for each planning unit. The result is converted to a
#' factor to represent discrete frequency levels.
#'
#' The output `sf` object can then be passed to `splnr_plot_selectionFreq()`
#' for visualization as a heatmap.
#'
#' @param solnMany A `list` of `prioritizr` solutions (if `type = "list"`)
#'   or a single `sf` object representing a `prioritizr` portfolio of solutions
#'   (if `type = "portfolio"`). Each individual solution must contain a
#'   column named `solution_1`.
#' @param type A character string indicating the input type: `"portfolio"`
#'   (for a single `sf` object with multiple solution columns) or `"list"`
#'   (for a list of single-solution `sf` objects). Defaults to `"portfolio"`.
#'
#' @return An `sf` object (`selFreq`) containing a column named `selFreq`.
#'   This column is a factor representing the selection frequency (sum of
#'   selected occurrences across all solutions) for each planning unit.
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr mutate select starts_with
#' @importFrom rlang .data
#' @importFrom sf st_as_sf st_drop_geometry
#' @importFrom stringr str_c str_pad
#' @importFrom stats setNames
#' @importFrom tibble as_tibble
#'
#' @examples
#' \dontrun{
#' # Assuming 'dat_species_bin' is an existing sf object in your package.
#'
#' # Create a base prioritizr problem.
#' dat_problem <- prioritizr::problem(
#'   dat_species_bin %>% dplyr::mutate(Cost = runif(n = dim(.)[[1]])),
#'   features = c("Spp1", "Spp2", "Spp3", "Spp4", "Spp5"),
#'   cost_column = "Cost"
#' ) %>%
#'   prioritizr::add_min_set_objective() %>%
#'   prioritizr::add_relative_targets(0.3) %>%
#'   prioritizr::add_binary_decisions() %>%
#'   prioritizr::add_default_solver(verbose = FALSE)
#'
#' # --- Example 1: Using a portfolio of solutions ---
#' # Create a conservation problem that contains a portfolio of solutions (e.g., 5 solutions).
#' dat_soln_portfolio <- dat_problem %>%
#'   prioritizr::add_cuts_portfolio(number_solutions = 5) %>%
#'   prioritizr::solve.ConservationProblem()
#'
#' # Calculate selection frequency from the portfolio.
#' selFreq_portfolio <- splnr_get_selFreq(solnMany = dat_soln_portfolio, type = "portfolio")
#' print(head(selFreq_portfolio))
#' # You can then plot this: splnr_plot_selectionFreq(selFreq_portfolio)
#'
#' # --- Example 2: Using a list of individual solutions ---
#' # Solve the problem multiple times to get different solutions (e.g., by randomizing costs)
#' dat_soln_list <- list(
#'   dat_problem %>% prioritizr::solve.ConservationProblem(),
#'   dat_problem %>%
#'     dplyr::mutate(Cost = runif(n = dim(.)[[1]])) %>% # Vary cost for a different solution
#'     prioritizr::solve.ConservationProblem(),
#'   dat_problem %>%
#'     dplyr::mutate(Cost = runif(n = dim(.)[[1]])) %>% # Another different solution
#'     prioritizr::solve.ConservationProblem()
#' )
#'
#' # Calculate selection frequency from the list of solutions.
#' selFreq_list <- splnr_get_selFreq(solnMany = dat_soln_list, type = "list")
#' print(head(selFreq_list))
#' # You can then plot this: splnr_plot_selectionFreq(selFreq_list)
#' }
splnr_get_selFreq <- function(solnMany, type = "portfolio") {

  # Assertions to validate input parameters.
  assertthat::assert_that(
    is.character(type) && length(type) == 1,
    msg = "'type' must be a single character string: 'portfolio' or 'list'."
  )
  assertthat::assert_that(
    type %in% c("portfolio", "list"),
    msg = "'type' must be either 'portfolio' or 'list'."
  )

  if (type == "portfolio") {
    # If type is "portfolio", expected input is a single sf object (the portfolio).
    assertthat::assert_that(
      inherits(solnMany, "sf"),
      msg = "For 'type = \"portfolio\"', 'solnMany' must be an 'sf' object."
    )
    assertthat::assert_that(
      any(grepl("solution_", names(solnMany))),
      msg = "For 'type = \"portfolio\"', 'solnMany' must contain columns starting with 'solution_'."
    )

    # Calculate selection frequency for a portfolio (sf object with multiple solution columns).
    selFreq <- solnMany %>%
      # Convert to tibble for dplyr operations on columns, ensuring unique names.
      dplyr::mutate(selFreq = as.factor(rowSums(dplyr::select(tibble::as_tibble(.),
                                                              dplyr::starts_with("solution_")), na.rm = TRUE))) %>%
      # Convert back to sf, explicitly retaining the original geometry.
      sf::st_as_sf(geometry = sf::st_geometry(solnMany)) %>%
      # Select only the calculated selection frequency column.
      dplyr::select("selFreq")
    return(selFreq)

  } else if (type == "list") {
    # If type is "list", expected input is a list of sf objects (individual solutions).
    assertthat::assert_that(
      is.list(solnMany),
      msg = "For 'type = \"list\"', 'solnMany' must be a list of sf objects."
    )
    assertthat::assert_that(
      length(solnMany) > 0,
      msg = "For 'type = \"list\"', 'solnMany' must not be an empty list."
    )
    # Check each element in the list.
    for (i in seq_along(solnMany)) {
      assertthat::assert_that(
        inherits(solnMany[[i]], "sf"),
        msg = paste0("Element ", i, " in 'solnMany' is not an 'sf' object.")
      )
      assertthat::assert_that(
        "solution_1" %in% names(solnMany[[i]]),
        msg = paste0("Solution ", i, " in 'solnMany' is missing the 'solution_1' column.")
      )
    }

    # Generate default names for solutions in the list for column naming.
    name_sol <- stringr::str_c("soln", stringr::str_pad(1:length(solnMany), width = 1, pad = "0"))

    # Process each solution in the list: select 'solution_1' and rename.
    s_list <- lapply(seq_along(solnMany), function(x) {
      solnMany[[x]] %>%
        tibble::as_tibble() %>% # Convert to tibble.
        dplyr::select("solution_1") %>% # Select the solution column.
        stats::setNames(name_sol[[x]]) # Rename it to the generated name.
    })

    # Combine all single-column solution tibbles into one wide data frame.
    soln <- data.frame(matrix(unlist(s_list), ncol = length(s_list)))
    colnames(soln) <- name_sol

    # Calculate selection frequency by summing binary solution columns.
    selFreq <- soln %>%
      # Sum across all columns starting with "soln" (which are our individual solutions).
      dplyr::mutate(selFreq = as.factor(rowSums(dplyr::select(., dplyr::starts_with("soln")), na.rm = TRUE))) %>%
      # Convert back to sf, using the geometry from the first solution in the list.
      sf::st_as_sf(geometry = sf::st_geometry(solnMany[[1]])) %>%
      # Select only the calculated selection frequency column.
      dplyr::select("selFreq")
    return(selFreq)

  } else {
    # This block should technically not be reached due to initial assertthat.
    stop("This function requires either a prioritizr portfolio or a list of solutions. Please check your input.")
  }
}
