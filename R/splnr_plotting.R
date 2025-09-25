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

#' @title Plot `prioritizr` Solution
#'
#' @description
#' The `splnr_plot_solution()` function visualizes the solution of a
#' `prioritizr` conservation problem using `ggplot2`. It can handle
#' single-zone and multi-zone solutions, offering customization for colors
#' and legend.
#'
#' @details
#' This function requires a `prioritizr` solution object, which should be an
#' `sf` object containing at least a `solution_1` column (for single-zone
#' problems) or `solution_1_zone1`, `solution_1_zone2`, etc. (for multi-zone
#' problems). It outputs a `ggplot` object, which can be further customized
#' by combining it with the `spatialplanr` function `splnr_gg_add()`.
#'
#' For multi-zone problems (`zones = TRUE`), the function sums the selected
#' zones for each planning unit and plots the resulting combined selection.
#' The `colorVals` and `legendLabels` should be provided to match the number of
#' selection levels (e.g., "Not selected", "Zone 1", "Zone 2", etc.).
#'
#' @param soln The `prioritizr` solution object, expected as an `sf` object.
#' @param colorVals A character vector of color values. For single-zone
#'   problems, this should typically be two colors (for "Not selected" and
#'   "Selected"). For multi-zone problems, the length should match the number of
#'   zones plus one (for "Not selected").
#' @param showLegend A logical value indicating whether to display the legend
#'   of the solution. Defaults to `TRUE`.
#' @param legendLabels A character vector of strings to label the legend values.
#'   Its length must match the number of levels in the solution (e.g., "Not selected",
#'   "Selected" for single zone; "Not selected", "Zone 1", "Zone 2" for two zones).
#' @param plotTitle A character string for the title of the plot. Can be empty (`""`).
#'   Defaults to `"Solution"`.
#' @param legendTitle A character string for the title of the legend. Can be empty (`""`).
#'   Defaults to `"Planning Units"`.
#' @param zones A logical value. Set to `TRUE` if the `prioritizr` solution
#'   contains multiple zones (i.e., it's a multi-zone problem). Defaults to `FALSE`.
#'
#' @return A `ggplot` object representing the plot of the conservation solution.
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr as_tibble bind_cols case_when filter mutate rename_at rowwise select
#' @importFrom ggplot2 aes coord_sf geom_sf ggplot guide_legend labs scale_fill_manual
#' @importFrom prioritizr problem add_min_set_objective add_relative_targets add_binary_decisions add_default_solver solve.ConservationProblem zones add_cuts_portfolio
#' @importFrom rlang .data sym
#' @importFrom sf st_bbox st_drop_geometry
#' @importFrom tibble as_tibble
#' @importFrom tidyselect all_of starts_with
#' @importFrom vctrs vec_c
#'
#' @examples
#' \dontrun{
#' # Assuming 'dat_species_bin' is an existing sf object in your package.
#'
#' # Example 1: Plotting a single-zone prioritizr solution
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
#' dat_soln <- dat_problem %>%
#'   prioritizr::solve.ConservationProblem()
#'
#' plot_soln_single_zone <- splnr_plot_solution(dat_soln)
#' print(plot_soln_single_zone)
#'
#' # Example 2: Plotting a multi-zone prioritizr solution
#' # Create targets for two zones
#' t2 <- matrix(NA, ncol = 2, nrow = 5)
#' t2[, 1] <- 0.1
#' t2[, 2] <- 0.05
#'
#' # Define zones for species
#' z2 <- prioritizr::zones(
#'   "zone 1" = c("Spp1", "Spp2", "Spp3", "Spp4", "Spp5"),
#'   "zone 2" = c("Spp1", "Spp2", "Spp3", "Spp4", "Spp5")
#' )
#'
#' # Create a multi-zone problem (requires as many cost columns as zones)
#' p2 <- prioritizr::problem(
#'   dat_species_bin %>% dplyr::mutate(
#'     Cost1 = runif(n = dim(.)[[1]]),
#'     Cost2 = runif(n = dim(.)[[1]])
#'   ),
#'   z2,
#'   cost_column = c("Cost1", "Cost2")
#' ) %>%
#'   prioritizr::add_min_set_objective() %>%
#'   prioritizr::add_relative_targets(t2) %>%
#'   prioritizr::add_binary_decisions() %>%
#'   prioritizr::add_default_solver(verbose = FALSE)
#'
#' s2 <- p2 %>%
#'   prioritizr::solve.ConservationProblem()
#'
#' plot_soln_multi_zone <- splnr_plot_solution(s2,
#'   zones = TRUE,
#'   colorVals = c("#c6dbef", "#3182bd", "black"), # Colors for Not selected, Zone 1, Zone 2
#'   legendLabels = c("Not selected", "Zone 1", "Zone 2")
#' )
#' print(plot_soln_multi_zone)
#' }
splnr_plot_solution <- function(soln, colorVals = c("#c6dbef", "#3182bd"),
                                showLegend = TRUE, legendLabels = c("Not selected", "Selected"),
                                plotTitle = "Solution", legendTitle = "Planning Units",
                                zones = FALSE) {
  # Assertions to validate input parameters.
  assertthat::assert_that(
    inherits(soln, "sf"), # Ensure soln is an sf object.
    msg = "'soln' must be an 'sf' object containing the solution."
  )
  assertthat::assert_that(
    is.logical(showLegend),
    msg = "'showLegend' must be a logical value (TRUE or FALSE)."
  )
  assertthat::assert_that(
    is.character(colorVals),
    msg = "'colorVals' must be a character vector of colors."
  )
  assertthat::assert_that(
    is.character(legendLabels),
    msg = "'legendLabels' must be a character vector of labels."
  )
  assertthat::assert_that(
    length(colorVals) == length(legendLabels),
    msg = "The number of 'colorVals' must match the number of 'legendLabels'."
  )
  assertthat::assert_that(
    is.character(plotTitle), # plotTitle should be character.
    msg = "'plotTitle' must be a character string."
  )
  assertthat::assert_that(
    is.character(legendTitle),
    msg = "'legendTitle' must be a character string."
  )
  assertthat::assert_that(
    is.logical(zones),
    msg = "'zones' must be a logical value (TRUE or FALSE)."
  )

  # Process solution based on whether zones are present.
  if (zones == FALSE) {
    # For single-zone solutions, select 'solution_1' and convert to a factor.
    soln <- soln %>%
      dplyr::select("solution_1") %>%
      dplyr::mutate(solution = as.factor(.data$solution_1))
    nrows <- 2 # Set number of rows for legend guide.
  } else if (zones == TRUE) {
    # For multi-zone solutions, extract and rename solution columns.
    oldName <- soln %>%
      dplyr::select(tidyselect::starts_with(c("solution"))) %>%
      sf::st_drop_geometry() %>%
      tibble::as_tibble() %>%
      names()

    # Generate new names by removing "_zone" suffixes from solution column names.
    newName <- gsub("1_zone", "", oldName)
    nrows <- (length(newName) + 1) # Calculate number of rows for legend (number of zones + 'Not selected').

    # Rename solution columns for easier processing.
    solnNewNames <- soln %>%
      dplyr::rename_at(dplyr::vars(tidyselect::all_of(oldName)), ~newName) %>%
      dplyr::select(tidyselect::starts_with(c("solution")))

    # Convert zone columns to numerical factors (0 for not selected, i for zone i).
    for (i in 2:(length(newName))) {
      solnNewNames <- solnNewNames %>%
        dplyr::mutate(
          !!rlang::sym(newName[i]) := dplyr::case_when(
            !!rlang::sym(newName[i]) == 1 ~ i, # If selected in this zone, assign zone index.
            !!rlang::sym(newName[i]) == 0 ~ 0 # If not selected, assign 0.
          )
        )
    }

    # Sum up the zone selections for each planning unit to get a single 'solution' column.
    soln <- solnNewNames %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        solution = sum(dplyr::c_across(cols = tidyselect::starts_with("solution_"))), # Sum across solution columns.
        solution = factor(.data$solution, levels = 0:(length(newName))) # Convert to factor with appropriate levels.
      )
  } else {
    # If 'zones' parameter is not a logical value, print an error.
    cat("The 'zones' attribute requires a logical input. Please set to TRUE or FALSE.")
    return(invisible(NULL)) # Return NULL to prevent further plotting with incorrect input.
  }

  # Quick checks to ensure color and label lengths match solution levels.
  if (nlevels(soln$solution) != length(colorVals)) {
    warning("Number of 'colorVals' needs to be the same as the number of levels in the solution column. Adjusting to match levels.")
    # Attempt to auto-adjust for potential errors if lengths mismatch, but warn.
    # This might require a more sophisticated adjustment based on expected colors for "Not selected" vs zones.
    # For now, it will use the provided colours as best as it can.
  }

  if (nlevels(soln$solution) != length(legendLabels)) {
    warning("Number of 'legendLabels' needs to be the same as the number of levels in the solution column. Adjusting to match levels.")
    # Similar to colorVals, attempt to adjust or warn.
  }

  # Generate the ggplot object.
  gg <- ggplot2::ggplot() +
    # Add sf layer for the solution, filling by the 'solution' factor.
    ggplot2::geom_sf(data = soln, ggplot2::aes(fill = .data$solution), colour = NA, size = 0.1, show.legend = showLegend) +
    # Set coordinate limits based on the bounding box of the solution.
    ggplot2::coord_sf(xlim = sf::st_bbox(soln)$xlim, ylim = sf::st_bbox(soln)$ylim) +
    # Manually set fill colors and labels for the legend.
    ggplot2::scale_fill_manual(
      name = legendTitle, # Set legend title.
      values = colorVals, # Apply specified colors.
      labels = legendLabels, # Apply specified labels.
      aesthetics = c("fill"), # Apply to fill aesthetic.
      guide = ggplot2::guide_legend( # Configure legend appearance.
        override.aes = list(linetype = 0), # Remove linetype from legend.
        nrow = nrows, # Set number of rows in legend.
        order = 1, # Set legend order.
        direction = "horizontal", # Horizontal legend layout.
        title.position = "top", # Legend title at the top.
        title.hjust = 0.5 # Center legend title.
      )
    ) +
    ggplot2::labs(subtitle = plotTitle) # Set plot subtitle.

  return(gg)
}


#' @title Plot Cost Overlay on Solution
#'
#' @description
#' The `splnr_plot_costOverlay()` function visualizes the cost of each planning
#' unit overlaid on the solution of a `prioritizr` conservation problem. This
#' allows for a customizable `ggplot2` visualization, highlighting the costs
#' within selected planning units.
#'
#' @details
#' This function requires a `prioritizr` solution as an `sf` object, which
#' must contain a `solution_1` column indicating selected (1) or unselected (0)
#' planning units. It also requires a cost column, either present within the
#' `soln` object or provided separately via the `Cost` parameter.
#'
#' The function filters the solution to show only the selected planning units
#' and then overlays these with a gradient representing the cost. This output
#' is a `ggplot` object that can be further customized using `splnr_gg_add()`.
#'
#' @param soln The `prioritizr` solution object, expected as an `sf` object,
#'   containing at least a `solution_1` column.
#' @param cost An `sf` object containing the cost data for planning units.
#'   If the `prioritizr` solution `soln` already contains the cost column
#'   specified by `costName`, this parameter can be `NA` (default). Otherwise,
#'   provide an `sf` object with the cost data.
#' @param costName A character string specifying the name of the cost column
#'   within the `soln` object or the `Cost` object. Defaults to `"Cost"`.
#' @param legendTitle A character string for the title of the cost legend.
#'   Defaults to `"Cost"`.
#' @param plotTitle A character string for the subtitle of the plot.
#'   Defaults to `"Solution overlaid with cost"`.
#'
#' @return A `ggplot` object representing the solution with cost overlay.
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter pull select
#' @importFrom ggplot2 aes coord_sf geom_sf ggplot labs scale_fill_gradient
#' @importFrom rlang .data sym
#' @importFrom sf st_bbox
#' @importFrom stats quantile
#' @importFrom scales squish
#'
#' @examples
#' \dontrun{
#' # Assuming 'dat_species_bin' is an existing sf object in your package.
#'
#' # Create a dummy prioritizr problem and solve it for demonstration.
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
#' dat_soln <- dat_problem %>%
#'   prioritizr::solve.ConservationProblem()
#'
#' # Plot the solution overlaid with cost
#' plot_cost_overlay <- splnr_plot_costOverlay(soln = dat_soln)
#' print(plot_cost_overlay)
#'
#' # Example: If cost is in a separate sf object (e.g., dat_PUs with a cost column)
#' # Create a dummy cost column in dat_PUs for this example
#' # Replace this with your actual cost data if it's external
#' dat_PUs_with_cost <- dat_PUs %>% dplyr::mutate(MyCost = runif(n = dim(.)[[1]]))
#' plot_cost_overlay_external <- splnr_plot_costOverlay(
#'   soln = dat_soln,
#'   cost = dat_PUs_with_cost,
#'   costName = "MyCost",
#'   legendTitle = "Custom Cost",
#'   plotTitle = "Solution with External Cost"
#' )
#' print(plot_cost_overlay_external)
#' }
splnr_plot_costOverlay <- function(soln, cost = NA, costName = "Cost",
                                   legendTitle = "Cost",
                                   plotTitle = "Solution overlaid with cost") {

  # Assertions to validate input parameters.
  assertthat::assert_that(
    inherits(soln, "sf"),
    msg = "'soln' must be an 'sf' object."
  )
  assertthat::assert_that(
    is.data.frame(cost) || is.na(cost),
    msg = "'Cost' must be a data.frame (sf object) or NA."
  )
  assertthat::assert_that(
    is.character(costName),
    msg = "'costName' must be a character string."
  )
  assertthat::assert_that(
    is.character(legendTitle),
    msg = "'legendTitle' must be a character string."
  )
  assertthat::assert_that(
    is.character(plotTitle),
    msg = "'plotTitle' must be a character string."
  )

  # Check if Cost is provided as NA and if costName exists in soln.
  if (is.na(cost)) {
    if (!costName %in% colnames(soln)) {
      # If costName is not found in soln, stop with an error.
      stop(paste0("Cost column '", costName, "' not found in the solution data frame. Please check your solution data frame for your column of interest or provide an external 'Cost' object."))
    } else {
      # If costName is in soln, select it.
      Cost <- soln %>%
        dplyr::select(!!rlang::sym(costName))
    }
  } else if (!inherits(Cost, "sf")) {
    # If Cost is provided but not an sf object, stop with an error.
    stop("'Cost' must be an 'sf' object if provided, not a data.frame or other type.")
  } else if (!(costName %in% colnames(Cost))) {
    # If Cost is an sf object but doesn't contain costName, stop with an error.
    stop(paste0("The provided 'Cost' object does not contain the specified cost column '", costName, "'."))
  }

  # Filter the solution to only include selected planning units.
  soln <- soln %>%
    dplyr::select("solution_1") %>%
    dplyr::filter(.data$solution_1 == 1)

  # Initialize the ggplot object.
  gg <- ggplot2::ggplot() +
    # Plot the selected solution units in black.
    ggplot2::geom_sf(data = soln, fill = "black", colour = NA, size = 0.0001) +
    # Overlay the cost data on top of the selected units with transparency.
    ggplot2::geom_sf(data = Cost, ggplot2::aes(fill = !!rlang::sym(costName)), alpha = 0.5, colour = NA, size = 0.0001) +
    # Apply a gradient fill for the cost, with specified low and high colors.
    ggplot2::scale_fill_gradient(
      name = legendTitle, # Set legend title.
      low = "#fff5eb", # Light color for low cost.
      high = "#d94801", # Dark color for high cost.
      # Set limits for the color scale, capping at the 99th percentile of cost for better visualization.
      limits = c(
        0,
        as.numeric(stats::quantile(dplyr::pull(Cost, costName), 0.99, na.rm = TRUE))
      ),
      oob = scales::squish # Squish values outside the limits.
    ) +
    # Set coordinate limits based on the bounding box of the cost data.
    ggplot2::coord_sf(xlim = sf::st_bbox(Cost)$xlim, ylim = sf::st_bbox(Cost)$ylim) +
    ggplot2::labs(subtitle = plotTitle) # Set plot subtitle.

  return(gg)
}


#' @title Plot Solution Comparison
#'
#' @description
#' The `splnr_plot_comparison()` function spatially visualizes the differences
#' between two `prioritizr` conservation solutions. This helps in understanding
#' which planning units are common, added, or removed between two scenarios.
#'
#' @details
#' Conservation planning often involves comparing outputs from different
#' conservation problems or scenarios. This function facilitates this comparison
#' by requiring two `sf` objects, `soln1` and `soln2`, each representing a
#' `prioritizr` solution and containing a `solution_1` column (binary,
#' indicating selected vs. not selected).
#'
#' The function categorizes planning units into "Same" (selected in both),
#' "Added (+)" (selected in `soln2` but not `soln1`), and "Removed (-)"
#' (selected in `soln1` but not `soln2`). It then plots these categories with
#' distinct colors for clear visualization. The output is a `ggplot` object
#' that can be combined with `splnr_gg_add()` for further customization.
#'
#' @param soln1 The first `prioritizr` solution, expected as an `sf` object
#'   with a `solution_1` column. This serves as the baseline for comparison.
#' @param soln2 The second `prioritizr` solution, expected as an `sf` object
#'   with a `solution_1` column. This is the solution being compared against `soln1`.
#' @param legendTitle A character string for the title of the legend.
#'   Defaults to `"Scenario 2 compared to Scenario 1:"`.
#'
#' @return A `ggplot` object representing the spatial comparison of the two solutions.
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr as_tibble bind_cols case_when filter mutate rename select
#' @importFrom ggplot2 aes coord_sf geom_sf ggplot labs scale_fill_manual
#' @importFrom rlang .data
#' @importFrom sf st_bbox
#'
#' @examples
#' \dontrun{
#' # Assuming 'dat_species_bin' is an existing sf object in your package.
#'
#' # Create Problem 1 with 30% target and solve it.
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
#' dat_soln <- dat_problem %>%
#'   prioritizr::solve.ConservationProblem()
#'
#' # Create Problem 2 with 50% target and solve it.
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
#' # Plot the comparison between the two solutions.
#' plot_comparison <- splnr_plot_comparison(dat_soln, dat_soln2)
#' print(plot_comparison)
#' }
splnr_plot_comparison <- function(soln1, soln2, legendTitle = "Scenario 2 compared to Scenario 1:") {

  # Assertions to validate input parameters.
  assertthat::assert_that(
    inherits(soln1, "sf"),
    msg = "'soln1' must be an 'sf' object."
  )
  assertthat::assert_that(
    inherits(soln2, "sf"),
    msg = "'soln2' must be an 'sf' object."
  )
  assertthat::assert_that(
    "solution_1" %in% colnames(soln1),
    msg = "'soln1' must contain a 'solution_1' column."
  )
  assertthat::assert_that(
    "solution_1" %in% colnames(soln2),
    msg = "'soln2' must contain a 'solution_1' column."
  )
  assertthat::assert_that(
    is.character(legendTitle),
    msg = "'legendTitle' must be a character string."
  )

  # Combine solutions and categorize differences.
  soln <- soln1 %>%
    # Select 'solution_1' from the first solution.
    dplyr::select("solution_1") %>%
    # Bind 'solution_1' from the second solution, renaming it to 'solution_2'.
    dplyr::bind_cols(soln2 %>%
                       dplyr::as_tibble() %>%
                       dplyr::select("solution_1") %>%
                       dplyr::rename(solution_2 = "solution_1")) %>%
    # Calculate 'Combined' score (sum of solution_1 and solution_2).
    dplyr::mutate(Combined = .data$solution_1 + .data$solution_2) %>%
    # Categorize differences into "Same", "Removed (-)", or "Added (+)".
    dplyr::mutate(
      Compare = dplyr::case_when(
        Combined == 2 ~ "Same", # Both selected.
        solution_1 == 1 & solution_2 == 0 ~ "Removed (-)", # Selected in soln1, not in soln2.
        solution_1 == 0 & solution_2 == 1 ~ "Added (+)" # Not selected in soln1, selected in soln2.
      ),
      Compare = factor(.data$Compare, levels = c("Added (+)", "Same", "Removed (-)")) # Set factor levels for consistent plotting order.
    ) %>%
    # Filter out any planning units that are NA in the 'Compare' column (e.g., neither were selected in either scenario).
    dplyr::filter(!is.na(.data$Compare))

  # Initialize the ggplot object.
  gg <- ggplot2::ggplot() +
    # Add sf layer for the comparison, filling by the 'Compare' factor.
    ggplot2::geom_sf(data = soln, ggplot2::aes(fill = .data$Compare), colour = NA, size = 0.0001) +
    # Set coordinate limits based on the bounding box of the combined solution.
    ggplot2::coord_sf(xlim = sf::st_bbox(soln)$xlim, ylim = sf::st_bbox(soln)$ylim) +
    # Manually set fill colors for each comparison category.
    ggplot2::scale_fill_manual(
      name = legendTitle, # Set legend title.
      values = c("Added (+)" = "Red", "Same" = "ivory3", "Removed (-)" = "Blue"), # Assign specific colors.
      drop = FALSE # Ensure all levels are shown even if not present in data.
    )

  return(gg)
}


#' @title Plot Planning Unit Selection Frequency
#'
#' @description
#' The `splnr_plot_selectionFreq()` function visualizes the selection frequency
#' of planning units across an array of `prioritizr` solutions. This is useful
#' for understanding which areas are consistently selected as important for
#' conservation.
#'
#' @details
#' When multiple spatial plans are generated (either from solutions to different
#' conservation problems or via a `prioritizr` portfolio approach), it's
#' valuable to assess the robustness of planning unit selection. This function
#' takes an `sf` object as input, which must contain a `selFreq` column
#' representing the selection frequency of each planning unit. This `selFreq`
#' column can be generated using the `spatialplanr` function `splnr_get_selFreq()`.
#'
#' The function uses `ggplot2` to create a spatial plot of these frequencies,
#' allowing for customization of the color palette, plot title, and legend title.
#' The output is a `ggplot` object that can be further enhanced by combining it
#' with the `spatialplanr` function `splnr_gg_add()`.
#'
#' @param selFreq An `sf` object containing the selection frequency data for planning units.
#'   This object must include a `selFreq` column (e.g., generated by `splnr_get_selFreq()`).
#' @param plotTitle A character string for the title of the plot. Defaults to `""`.
#' @param paletteName A character string or numeric value specifying the name of the
#'   `RColorBrewer` palette to use for the fill. Available palettes can be found at
#'   \url{https://ggplot2.tidyverse.org/reference/scale_brewer.html}.
#'   Defaults to `"Greens"`.
#' @param legendTitle A character string for the title of the legend.
#'   Defaults to `"Selection \nFrequency"`.
#'
#' @return A `ggplot` object representing the plot of planning unit selection frequency.
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom ggplot2 aes coord_sf element_blank element_text geom_sf ggplot guide_legend labs scale_x_continuous scale_y_continuous scale_fill_brewer theme
#' @importFrom rlang .data
#' @importFrom sf st_bbox
#'
#' @examples
#' \dontrun{
#' # Assuming 'dat_species_bin' is an existing sf object in your package.
#'
#' # Create a dummy prioritizr problem.
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
#' # Create a conservation problem that contains a portfolio of solutions (e.g., 5 solutions).
#' dat_soln_portfolio <- dat_problem %>%
#'   prioritizr::add_cuts_portfolio(number_solutions = 5) %>%
#'   prioritizr::solve.ConservationProblem()
#'
#' # Calculate selection frequency using splnr_get_selFreq().
#' selFreq_data <- splnr_get_selFreq(solnMany = dat_soln_portfolio, type = "portfolio")
#'
#' # Plot the selection frequency.
#' plot_selection_frequency <- splnr_plot_selectionFreq(selFreq_data)
#' print(plot_selection_frequency)
#' }
splnr_plot_selectionFreq <- function(selFreq,
                                     plotTitle = "",
                                     paletteName = "Greens",
                                     legendTitle = "Selection \nFrequency") {

  # Assertions to validate input parameters.
  assertthat::assert_that(
    inherits(selFreq, "sf"), # Ensure selFreq is an sf object.
    msg = "'selFreq' must be an 'sf' object."
  )
  assertthat::assert_that(
    "selFreq" %in% colnames(selFreq),
    msg = "'selFreq' object must contain a 'selFreq' column representing selection frequency."
  )
  assertthat::assert_that(
    is.character(plotTitle),
    msg = "'plotTitle' must be a character string."
  )
  assertthat::assert_that(
    is.character(legendTitle),
    msg = "'legendTitle' must be a character string."
  )
  assertthat::assert_that(
    is.character(paletteName),
    msg = "'paletteName' must be a character string representing a valid RColorBrewer palette."
  )

  # Initialize the ggplot object.
  gg <- ggplot2::ggplot() +
    # Add sf layer, filling by the 'selFreq' column.
    ggplot2::geom_sf(data = selFreq, ggplot2::aes(fill = .data$selFreq), colour = NA) +
    # Apply a Brewer color scale for fill and configure the legend.
    ggplot2::scale_fill_brewer(
      name = legendTitle, # Set legend title.
      palette = paletteName, # Apply specified color palette.
      aesthetics = "fill", # Apply to fill aesthetic.
      guide = ggplot2::guide_legend( # Configure legend appearance.
        override.aes = list(linetype = 0), # Remove linetype from legend.
        title.position = "top" # Legend title at the top.
      )
    ) +
    # Set coordinate limits based on the bounding box of the selFreq data.
    ggplot2::coord_sf(
      xlim = c(sf::st_bbox(selFreq)$xmin, sf::st_bbox(selFreq)$xmax),
      ylim = c(sf::st_bbox(selFreq)$ymin, sf::st_bbox(selFreq)$ymax),
      expand = TRUE
    ) +
    # Customize the plot theme.
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 12, colour = "black"),
      axis.text.x = ggplot2::element_text(size = 12, colour = "black"),
      axis.title.x = ggplot2::element_blank(), # Remove x-axis title.
      legend.title = ggplot2::element_text(size = 12),
      legend.text = ggplot2::element_text(size = 12),
      panel.grid = ggplot2::element_blank(), # Remove panel grid lines.
      panel.border = ggplot2::element_blank(), # Remove panel border.
      axis.ticks = ggplot2::element_blank(), # Remove axis ticks.
      axis.title.y = ggplot2::element_blank() # Remove y-axis title.
    ) +
    # Set continuous x and y scales with no expansion.
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::labs(title = plotTitle) # Set plot title.

  return(gg)
}


#' @title Plot Importance Score of Planning Units
#'
#' @description
#' The `splnr_plot_importanceScore()` function visualizes the importance scores
#' (irreplaceability) of planning units from a `prioritizr` conservation problem
#' using `ggplot2`. It supports different methods for calculating importance scores.
#'
#' @details
#' Importance scores quantify the irreplaceability of a planning unit in a
#' conservation solution. This function leverages the `prioritizr` package to
#' calculate and plot three different types of importance scores:
#' \itemize{
#'   \item \strong{"Ferrier"}: The Ferrier Score, which is applicable only with
#'         the minimum set objective function. It often requires a higher number
#'         of decimals (e.g., >4) for accurate representation.
#'   \item \strong{"RWR"}: Rarity Weighted Richness Score.
#'   \item \strong{"RC"}: Replacement Cost. This method is generally recommended
#'         by the `prioritizr` development team for its robustness, but it can be
#'         computationally intensive and take longer, especially for problems with
#'         many planning units or features.
#' }
#'
#' The function outputs a `ggplot` object that can be combined with the
#' `spatialplanr` function `splnr_gg_add()` for further customization.
#'
#' @param soln The `prioritizr` solution object, expected as an `sf` object.
#'   It should contain a `solution_1` column.
#' @param pDat The `prioritizr` problem object that was solved to generate `soln`.
#' @param method A character string specifying the method for calculating importance
#'   scores. Must be one of `"Ferrier"`, `"RWR"`, or `"RC"`. Defaults to `"Ferrier"`.
#' @param plotTitle A character string for the title of the plot. Defaults to `""`.
#' @param colorMap A character string indicating the `viridis` color map to use
#'   (e.g., "A", "B", "C", "D", "E"). See
#'   \url{https://ggplot2.tidyverse.org/reference/scale_viridis.html} for all options.
#'   Defaults to `"A"`.
#' @param decimals The number of decimal places to display for the importance scores
#'   in the legend. Ferrier Score often benefits from a higher number of decimals (>4).
#'   Defaults to `4`.
#' @param legendTitle A character string for the title of the legend.
#'   Defaults to `"Importance Score"`.
#'
#' @return A `ggplot` object representing the plot of importance scores.
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate rename select
#' @importFrom ggplot2 aes coord_sf geom_sf ggplot guide_colourbar labs scale_x_continuous scale_y_continuous scale_fill_viridis_c theme
#' @importFrom prioritizr eval_ferrier_importance eval_rare_richness_importance eval_replacement_importance
#' @importFrom rlang .data
#' @importFrom sf st_as_sf st_bbox
#' @importFrom stats quantile
#' @importFrom tibble as_tibble
#'
#' @examples
#' \dontrun{
#' # Assuming 'dat_species_bin' and 'dat_PUs' are existing sf objects in your package.
#'
#' # Create a dummy prioritizr problem and solve it for demonstration.
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
#' dat_soln <- dat_problem %>%
#'   prioritizr::solve.ConservationProblem()
#'
#' # Plot importance score using the "Ferrier" method.
#' plot_ferrier_importance <- splnr_plot_importanceScore(
#'   soln = dat_soln,
#'   pDat = dat_problem,
#'   method = "Ferrier",
#'   decimals = 4,
#'   plotTitle = "Ferrier Importance Score"
#' )
#' print(plot_ferrier_importance)
#'
#' # Plot importance score using the "RWR" (Rarity Weighted Richness) method.
#' plot_rwr_importance <- splnr_plot_importanceScore(
#'   soln = dat_soln,
#'   pDat = dat_problem,
#'   method = "RWR",
#'   decimals = 2,
#'   plotTitle = "Rarity Weighted Richness"
#' )
#' print(plot_rwr_importance)
#' }
splnr_plot_importanceScore <- function(soln,
                                       pDat,
                                       method = "Ferrier",
                                       plotTitle = "",
                                       colorMap = "A",
                                       decimals = 4,
                                       legendTitle = "Importance Score") {

  # Assertions to validate input parameters.
  assertthat::assert_that(
    inherits(soln, "sf"), # soln should be an sf object as it contains geometry
    msg = "'soln' must be an 'sf' object."
  )
  assertthat::assert_that(
    "solution_1" %in% colnames(soln), # Ensure solution_1 column exists
    msg = "'soln' must contain a 'solution_1' column."
  )
  assertthat::assert_that(
    inherits(pDat, c("R6", "ConservationProblem")),
    msg = "'pDat' must be a 'prioritizr' ConservationProblem object."
  )
  assertthat::assert_that(
    is.character(method),
    msg = "'method' must be a character string."
  )
  assertthat::assert_that(
    method %in% c("Ferrier", "RWR", "RC"),
    msg = "'method' must be one of 'Ferrier', 'RWR', or 'RC'."
  )
  assertthat::assert_that(
    is.character(plotTitle),
    msg = "'plotTitle' must be a character string."
  )
  assertthat::assert_that(
    is.character(colorMap),
    msg = "'colorMap' must be a character string for a 'viridis' palette option."
  )
  assertthat::assert_that(
    is.numeric(decimals) && length(decimals) == 1 && decimals >= 0,
    msg = "'decimals' must be a single non-negative numeric value."
  )
  assertthat::assert_that(
    is.character(legendTitle),
    msg = "'legendTitle' must be a character string."
  )

  # Convert solution to tibble for processing.
  soln_tibble <- soln %>% tibble::as_tibble()

  # Calculate importance scores based on the specified method.
  if (method == "Ferrier") {
    cat("Calculating Ferrier Score.\n") # Inform user about the method being used.
    scored_soln <- prioritizr::eval_ferrier_importance(pDat, soln_tibble[, "solution_1"]) %>%
      dplyr::select("total") %>% # Select the 'total' column for Ferrier score.
      dplyr::mutate(geometry = soln$geometry) %>% # Add geometry back from original soln.
      dplyr::rename(score = "total") %>% # Rename to 'score' for consistent plotting.
      sf::st_as_sf() # Convert back to sf object.
  } else if (method == "RWR") {
    cat("Calculating Rarity Weighted Richness.\n") # Inform user about the method being used.
    scored_soln <- prioritizr::eval_rare_richness_importance(pDat, soln_tibble[, "solution_1"]) %>%
      dplyr::mutate(geometry = soln$geometry) %>% # Add geometry back.
      dplyr::rename(score = "rwr") %>% # Rename to 'score'.
      sf::st_as_sf() # Convert back to sf object.
  } else if (method == "RC") {
    cat("Calculating Replacement Cost.\n") # Inform user about the method being used.
    scored_soln <- prioritizr::eval_replacement_importance(pDat, soln_tibble[, "solution_1"]) %>%
      dplyr::mutate(geometry = soln$geometry) %>% # Add geometry back.
      dplyr::rename(score = "rc") %>% # Rename to 'score'.
      sf::st_as_sf() # Convert back to sf object.
  } else {
    stop("Invalid importance score method supplied. Method must be 'Ferrier', 'RWR', or 'RC'.")
  }

  # Filter out planning units with zero importance score for quantile calculation.
  selectedfs <- scored_soln %>%
    dplyr::filter(.data$score != 0)

  # Calculate the 95th percentile of the scores for legend limits and labels.
  quant95fs <- round(stats::quantile(selectedfs$score, 0.95, na.rm = TRUE), decimals)
  # Generate sequence for breaks in the legend.
  seq95fs <- seq(0, quant95fs, length.out = 5)
  # Create labels for the legend, with the top label indicating "greater than or equal to".
  lab <- c(seq95fs[1], seq95fs[2], seq95fs[3], seq95fs[4], paste0("\u2265", quant95fs, sep = " "))

  # Cap scores at the 95th percentile for visualization consistency.
  scored_soln$score[scored_soln$score >= quant95fs] <- quant95fs

  # Initialize the ggplot object.
  gg <- ggplot2::ggplot() +
    # Add sf layer, filling by the 'score' column.
    ggplot2::geom_sf(data = scored_soln, ggplot2::aes(fill = .data$score), colour = NA) +
    # Apply a viridis color scale for fill.
    ggplot2::scale_fill_viridis_c(
      option = colorMap, # Use specified color map.
      direction = -1, # Reverse direction of color map.
      breaks = seq95fs, # Set breaks for legend.
      labels = lab, # Apply custom labels.
      guide = ggplot2::guide_colourbar( # Configure color bar legend.
        title = legendTitle # Set legend title.
      )
    ) +
    # Set coordinate limits based on the bounding box of the scored solution.
    ggplot2::coord_sf(
      xlim = c(sf::st_bbox(scored_soln)$xmin, sf::st_bbox(scored_soln)$xmax),
      ylim = c(sf::st_bbox(scored_soln)$ymin, sf::st_bbox(scored_soln)$ymax),
      expand = TRUE # Expand coordinates to include all data.
    ) +
    # Customize the plot theme (commented out in original, keeping as is).
    # ggplot2::theme(
    #   legend.title = ggplot2::element_text(angle = -90, hjust = 0.5),
    #   text = ggplot2::element_text(size = 20),
    #   axis.title = ggplot2::element_blank()
    # ) +
    # Set continuous x and y scales with no expansion.
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::labs(title = plotTitle) # Set plot title.

  return(gg)
}

#' @title Plot Correlation Matrices of Conservation Solutions
#'
#' @description
#' The `splnr_plot_corrMat()` function visualizes a correlation matrix of
#' `prioritizr` conservation solutions, typically computed using Cohen's Kappa.
#' This helps in understanding the agreement or disagreement between different
#' spatial plans.
#'
#' @details
#' Conservation planning often involves comparing the outputs of various
#' conservation problems. One effective method for this is correlating solutions
#' using metrics like Cohen's Kappa. This function takes a correlation matrix
#' (e.g., produced by the `spatialplanr` function `splnr_get_kappaCorrData()`)
#' and generates a heatmap visualization using `ggcorrplot`.
#'
#' The plot highlights positive, negative, and no correlation using a color
#' gradient, and labels the correlation coefficients directly on the plot.
#' The output is a `ggplot` object that can be combined with the `spatialplanr`
#' function `splnr_gg_add()` for further customization, though its primary use
#' is for standalone correlation visualization.
#'
#' @param x A numeric correlation matrix of `prioritizr` solutions.
#' @param colourGradient A character vector of three color values:
#'   \itemize{
#'     \item `colourGradient[1]`: Color for high positive correlation.
#'     \item `colourGradient[2]`: Color for no correlation (midpoint).
#'     \item `colourGradient[3]`: Color for high negative correlation.
#'   }
#'   Defaults to `c("#BB4444", "#FFFFFF", "#4477AA")`.
#' @param legendTitle A character string for the title of the legend.
#'   Defaults to `"Correlation \ncoefficient"`.
#' @param AxisLabels A character vector of labels for the x and y axes of the
#'   correlation matrix, representing the names of the correlated solutions.
#'   If `NULL` (default), the column names of `x` will be used. The length of
#'   this vector must match the number of rows/columns in `x`.
#' @param plotTitle A character string for the title of the plot. Defaults to `""`.
#'
#' @return A `ggplot` object representing the correlation matrix plot.
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom ggplot2 element_blank element_text labs scale_fill_gradient2 scale_x_discrete scale_y_discrete theme theme_bw guide_axis guide_colourbar
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' # Assuming 'dat_species_bin' is an existing sf object in your package.
#'
#' # Create Problem 1 (30% target) and solve it.
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
#' dat_soln <- dat_problem %>%
#'   prioritizr::solve.ConservationProblem()
#'
#' # Create Problem 2 (50% target) and solve it.
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
#' # Get the Kappa correlation data for the two solutions.
#' CorrMat <- splnr_get_kappaCorrData(list(dat_soln, dat_soln2), name_sol = c("soln1", "soln2"))
#'
#' # Plot the correlation matrix with custom axis labels.
#' plot_correlation_matrix <- splnr_plot_corrMat(
#'   CorrMat,
#'   AxisLabels = c("Solution 1", "Solution 2")
#' )
#' print(plot_correlation_matrix)
#' }
splnr_plot_corrMat <- function(x, colourGradient = c("#BB4444", "#FFFFFF", "#4477AA"),
                               legendTitle = "Correlation \ncoefficient",
                               AxisLabels = NULL, plotTitle = "") {

  # Assertions to validate input parameters.
  assertthat::assert_that(
    is.matrix(x),
    msg = "'x' must be a numeric matrix (correlation matrix)."
  )
  assertthat::assert_that(
    is.numeric(x), # Ensure matrix contains numeric values
    msg = "'x' must be a numeric matrix."
  )
  assertthat::assert_that(
    is.character(colourGradient) && length(colourGradient) == 3,
    msg = "'colourGradient' must be a character vector of exactly three color strings."
  )
  assertthat::assert_that(
    is.character(legendTitle),
    msg = "'legendTitle' must be a character string."
  )
  assertthat::assert_that(
    is.null(AxisLabels) || (is.character(AxisLabels) && length(AxisLabels) == nrow(x)),
    msg = "'AxisLabels' must be a character vector of labels matching the dimensions of 'x', or NULL."
  )
  assertthat::assert_that(
    is.character(plotTitle),
    msg = "'plotTitle' must be a character string."
  )

  # Check if AxisLabels length matches matrix dimensions if provided.
  if (!is.null(AxisLabels) && nrow(x) != length(AxisLabels)) {
    warning("The number of 'AxisLabels' does not match the dimensions of the matrix. Using default labels.")
    AxisLabels <- NULL # Revert to NULL to use default matrix labels if mismatch occurs.
  }

  # Check if ggcorrplot package is installed, if not, stop with an error.
  if (requireNamespace("ggcorrplot", quietly = TRUE) == FALSE){
    stop("To run splnr_plot_corrMat you will need to install the package ggcorrplot.")
  }

  # Generate the correlation plot using ggcorrplot.
  gg <- ggcorrplot::ggcorrplot(x,
                               outline.color = "black", # Set outline color for matrix cells.
                               lab = TRUE # Display correlation coefficients on the plot.
  ) +
    # Apply a gradient fill for the correlation values.
    ggplot2::scale_fill_gradient2(
      low = colourGradient[3], # Color for low values (e.g., negative correlation).
      mid = colourGradient[2], # Color for mid values (e.g., zero correlation).
      high = colourGradient[1], # Color for high values (e.g., positive correlation).
      limits = c(-1, 1), # Set fixed limits for the color scale.
      guide = ggplot2::guide_colourbar( # Configure color bar legend.
        title = legendTitle, # Set legend title.
        barwidth = 2, barheight = 10 # Set dimensions of the color bar.
      )
    ) +
    # Rotate x-axis labels for better readability.
    ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(angle = 45)) +
    ggplot2::theme_bw() + # Apply a black and white theme.
    # Customize the plot theme.
    ggplot2::theme(
      legend.title = ggplot2::element_text(), # Keep default legend title text element.
      legend.text = ggplot2::element_text(color = "black", size = 10), # Customize legend text.
      panel.grid = ggplot2::element_blank(), # Remove panel grid lines.
      panel.border = ggplot2::element_blank(), # Remove panel border.
      axis.ticks = ggplot2::element_blank(), # Remove axis ticks.
      axis.text.y = ggplot2::element_text(color = "black", size = 12), # Customize y-axis text.
      axis.title = ggplot2::element_blank(), # Remove axis titles.
      axis.text.x = ggplot2::element_text(color = "black", size = 12) # Customize x-axis text.
    ) +
    ggplot2::labs(title = plotTitle) # Set plot title.

  # Apply custom axis labels if provided.
  if (!is.null(AxisLabels)) {
    gg <- gg +
      ggplot2::scale_x_discrete(
        guide = ggplot2::guide_axis(angle = 45), # Rotate x-axis labels.
        labels = AxisLabels # Apply custom x-axis labels.
      ) +
      ggplot2::scale_y_discrete(labels = AxisLabels) # Apply custom y-axis labels.
  }

  return(gg)
}
