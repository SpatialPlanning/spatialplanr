#' @title Plot Spatial Data
#'
#' @description
#' This function provides a versatile way to plot spatial data (`sf` objects)
#' within the `spatialplanr` package. It can visualize various data types,
#' including binary presence/absence, logical values, continuous data, or simply
#' the planning unit outlines.
#'
#' @details
#' The `splnr_plot` function automatically detects the type of data specified by
#' `colNames` (binary, logical, or continuous) and adjusts the plotting
#' aesthetics accordingly. If multiple `colNames` are provided, it calculates
#' the sum of features for each planning unit and plots this sum. If `colNames`
#' is `NULL`, it will simply plot the outlines of the planning units.
#'
#' This function is designed to be a flexible replacement for several plotting
#' functions, such as `splnr_plot_cost()`, `splnr_plot_binFeature()`,
#' `splnr_plot_MPAs()`, and `splnr_plot_featureNo()`, streamlining the plotting
#' workflow within the package.
#'
#' Written by Kilian Barreiro and Jason Everett.
#' Last modified: February 2024.
#'
#' @param df The input dataframe containing the data to be plotted. This must be
#'   an `sf` object and include a geometry column.
#' @param colNames A character vector of column names from `df` to be used for
#'   coloring the plot. If `NULL` (default), only the planning unit outlines are plotted.
#'   If a single column is specified, it checks for binary, logical, or continuous data.
#'   If multiple columns are specified, it sums the values across these columns to create
#'   a "FeatureSum" for plotting.
#' @param paletteName A character string specifying the name of the `RColorBrewer`
#'   palette to use for filling continuous data. Defaults to `"YlGnBu"`.
#' @param colourVals A character vector of two color values to use for binary
#'   (0/1) or logical (FALSE/TRUE) data. The first color is for '0' or 'FALSE'
#'   (absence), and the second is for '1' or 'TRUE' (presence).
#'   Defaults to `c("#c6dbef", "#3182bd")`.
#' @param plotTitle A character string for the subtitle of the plot.
#'   Defaults to `""` (no subtitle).
#' @param legendTitle A character string for the title of the legend. If `NULL`,
#'   a default title will be used based on the data type.
#' @param legendLabels A character vector of strings to use for the legend labels,
#'   particularly useful for binary or logical data (e.g., `c("Absent", "Present")`).
#'   If `NULL`, default labels are used for binary/logical plots.
#'
#' @return A `ggplot` object representing the spatial plot.
#'
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr across as_tibble filter mutate select
#' @importFrom ggplot2 aes coord_sf geom_sf ggplot labs scale_fill_distiller
#' @importFrom ggplot2 scale_fill_manual scale_fill_viridis_c guide_colourbar guides
#' @importFrom purrr map_vec
#' @importFrom rlang .data
#' @importFrom scales squish
#' @importFrom sf st_as_sf st_bbox st_drop_geometry
#' @importFrom tidyr replace_na
#' @importFrom tidyselect all_of starts_with where
#'
#' @examples
#' \dontrun{
#' # Assuming 'dat_species_bin', 'dat_bathy', and 'dat_PUs' are existing sf objects
#' # in your package, suitable for plotting.
#'
#' # Binary plot of species distribution for "Spp1"
#' plot_spp1_binary <- splnr_plot(
#'   df = dat_species_bin,
#'   colNames = "Spp1",
#'   legendTitle = "Species Presence",
#'   legendLabels = c("Absent", "Present")
#' )
#' print(plot_spp1_binary)
#'
#' # Logical plot of species distribution for "Spp1" (converted from binary)
#' plot_spp1_logical <- splnr_plot(
#'   df = dat_species_bin %>%
#'     dplyr::mutate(dplyr::across(
#'       tidyselect::starts_with("Spp"), as.logical
#'     )),
#'   colNames = "Spp1",
#'   legendTitle = "Species Presence",
#'   legendLabels = c("Absent", "Present")
#' )
#' print(plot_spp1_logical)
#'
#' # Continuous plot of bathymetry
#' plot_bathymetry <- splnr_plot(
#'   df = dat_bathy,
#'   colNames = "bathymetry",
#'   plotTitle = "Bathymetry",
#'   legendTitle = "Bathymetry (m)"
#' )
#' print(plot_bathymetry)
#'
#' # Plot Planning Units outlines only
#' plot_planning_units <- splnr_plot(df = dat_PUs)
#' print(plot_planning_units)
#'
#' # Multi-binary features: Plotting the sum of multiple "Spp" features
#' plot_multi_spp_sum <- splnr_plot(
#'   df = dat_species_bin,
#'   colNames = colnames(dat_species_bin %>%
#'     sf::st_drop_geometry() %>%
#'     dplyr::select(tidyselect::starts_with("Spp"))),
#'   legendTitle = "Number of Features"
#' )
#' print(plot_multi_spp_sum)
#' }
splnr_plot <- function(df,
                       colNames = NULL,
                       paletteName = "YlGnBu",
                       colourVals = c("#c6dbef", "#3182bd"),
                       plotTitle = "",
                       legendTitle = NULL,
                       legendLabels = NULL) {

  # Assertions to validate input parameters.
  assertthat::assert_that(
    is.data.frame(df),
    msg = "'df' must be a data.frame."
  )
  assertthat::assert_that(
    inherits(df, "sf"),
    msg = "'df' must be an 'sf' object."
  )
  assertthat::assert_that(
    all(c("xmin", "xmax", "ymin", "ymax") %in% names(sf::st_bbox(df))),
    msg = "'df' must have a valid bounding box (sf::st_bbox)."
  )
  assertthat::assert_that(
    is.null(colNames) || is.character(colNames),
    msg = "'colNames' must be a character vector or NULL."
  )
  if (!is.null(colNames)) {
    assertthat::assert_that(
      all(colNames %in% colnames(df)),
      msg = paste0("Not all specified 'colNames' exist in the input dataframe. Missing: ",
                   paste(colNames[!colNames %in% colnames(df)], collapse = ", "))
    )
  }
  assertthat::assert_that(
    is.character(paletteName),
    msg = "'paletteName' must be a character string (e.g., a RColorBrewer palette name)."
  )
  assertthat::assert_that(
    is.null(legendTitle) || is.character(legendTitle),
    msg = "'legendTitle' must be a character string or NULL."
  )
  assertthat::assert_that(
    is.null(legendLabels) || is.character(legendLabels),
    msg = "'legendLabels' must be a character vector or NULL."
  )
  assertthat::assert_that(
    is.character(plotTitle),
    msg = "'plotTitle' must be a character string."
  )
  assertthat::assert_that(
    is.character(colourVals) && length(colourVals) == 2,
    msg = "'colourVals' must be a character vector of exactly two color strings."
  )


  # Initialize flags for data type detection.
  is_binary <- FALSE
  is_logi <- FALSE
  is_continuous <- FALSE
  showFeatureSum <- FALSE

  # Determine data type based on 'colNames' presence and content.
  if (!is.null(colNames)){ # If 'colNames' are provided.

    if (length(colNames) == 1){ # If only one column name is specified.

      if (is.logical(df[[colNames]])){ # Check if the column data is logical (TRUE/FALSE).
        is_logi <- TRUE
      } else { # If not logical, check if it's binary (0/1).
        # Create a temporary dataframe, replacing NA with 0 in the target columns for binary check.
        df0 <- df %>%
          dplyr::mutate(dplyr::across(tidyselect::all_of(colNames), ~tidyr::replace_na(., 0)))
        # Check if all values in the column are exclusively 0 or 1.
        is_binary <- all(purrr::map_vec(colNames, function(x) all(df0[[x]] %in% c(0, 1))))
      }

      ## If not binary and not logical, assume it's continuous.
      if (isFALSE(is_binary) & isFALSE(is_logi)){
        is_continuous <- TRUE # This assumption allows plotting, and issues would be visible.
      }

    } else if (length(colNames) > 1){ # If multiple column names are specified.
      showFeatureSum <- TRUE # Set flag to calculate and show the sum of features.
    }

  }

  # Initialize the base ggplot object with coordinate system and subtitle.
  gg <- ggplot2::ggplot() +
    ggplot2::coord_sf(xlim = sf::st_bbox(df)$xlim, ylim = sf::st_bbox(df)$ylim) +
    ggplot2::labs(subtitle = plotTitle)

  # Plot logic based on the determined data type.

  if (showFeatureSum) {
    # If showing feature sum, calculate it and prepare data for plotting.
    df <- df %>%
      dplyr::as_tibble() %>% # Convert to tibble to handle geometry column easily.
      dplyr::select(tidyselect::all_of(c(colNames, "geometry"))) %>% # Select only relevant columns and geometry.
      dplyr::mutate(FeatureSum = rowSums(dplyr::across(tidyselect::where(is.numeric)), na.rm = TRUE)) %>% # Calculate sum of numeric feature columns.
      sf::st_as_sf(sf_column_name = "geometry") %>% # Convert back to sf object.
      dplyr::select("FeatureSum") # Keep only FeatureSum and geometry.

    # Add geom_sf for continuous fill based on FeatureSum.
    gg <- gg +
      ggplot2::geom_sf(data = df, ggplot2::aes(fill = .data$FeatureSum), colour = NA, size = 0.1) +
      # Apply a distiller palette for continuous data and configure the legend.
      ggplot2::scale_fill_distiller(
        name = legendTitle,
        palette = paletteName,
        aesthetics = c("fill"),
        oob = scales::squish) +
      ggplot2::guides(fill = ggplot2::guide_colourbar(order = -1))

    return(gg)
  } else if (is_binary | is_logi) { # If data is binary or logical.

    # Set default legend labels if not provided.
    if (is.null(legendLabels)){
      legendLabels = c("Absence", "Presence")
    }

    # Add geom_sf for discrete fill based on the single column.
    gg <- gg +
      ggplot2::geom_sf(data = df, ggplot2::aes(fill = factor(.data[[colNames]])), colour = "grey80", size = 0.1)

    # Apply manual fill scale for binary (0/1) data.
    if (isTRUE(is_binary)) {
      gg <- gg +
        ggplot2::scale_fill_manual(values = c("0" = colourVals[1], "1" = colourVals[2]),
                                   labels = legendLabels,
                                   name = legendTitle)
    }

    # Apply manual fill scale for logical (FALSE/TRUE) data.
    if (isTRUE(is_logi)) {
      gg <- gg +
        ggplot2::scale_fill_manual(values = c("FALSE" = colourVals[1], "TRUE" = colourVals[2]),
                                   labels = legendLabels,
                                   name = legendTitle)
    }

  } else if (is_continuous) { # If data is continuous.

    # Add geom_sf for continuous fill and color based on the single column.
    gg <- gg +
      ggplot2::geom_sf(data = df, ggplot2::aes(fill = .data[[colNames]], colour = .data[[colNames]])) +
      # Apply a viridis continuous color scale for fill and color.
      ggplot2::scale_fill_viridis_c(name = legendTitle, aesthetics = c("colour", "fill")) +
      # Configure guides to show color bar for fill and hide color legend for outline.
      ggplot2::guides(fill = ggplot2::guide_colourbar(order = 1),
                      colour = "none")

  } else if (is.null(colNames)){ # If no column to plot by (only planning unit outlines).

    # Add geom_sf to display planning unit outlines without fill.
    gg <- gg +
      ggplot2::geom_sf(data = df, colour = "grey80", fill = NA, size = 0.1)
  }

  return(gg)
}
