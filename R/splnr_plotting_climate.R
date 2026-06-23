
#' @title Plot Climate Metric Data
#'
#' @description
#' The `splnr_plot_climData()` function creates a spatial plot of climate metric
#' information from an `sf` object. It provides a customizable visualization
#' using `ggplot2` and `viridis` color palettes.
#'
#' @details
#' This function is designed to visualize spatial data that contains a specific
#' climate metric. It expects an `sf` object (`df`) with a geometry column and
#' the climate metric data in a column specified by `colInterest`. The plot uses
#' a continuous color scale (viridis) to represent the metric values across the
#' planning units.
#'
#' This function can be easily integrated into a larger plotting workflow or
#' used independently to inspect climate data distributions.
#'
#' @param df An `sf` object containing the climate metric information. It must
#'   have a geometry column.
#' @param colInterest A character string specifying the name of the column in `df`
#'   that contains the climate metric data to be plotted.
#' @param colorMap A character string indicating the `viridis` color map to use
#'   (e.g., "A", "B", "C", "D", "E"). See
#'   \url{https://ggplot2.tidyverse.org/reference/scale_viridis.html} for all options.
#'   Defaults to `"C"`.
#' @param plotTitle A character string for the subtitle of the plot.
#'   Defaults to `" "` (a single space, effectively no subtitle).
#' @param legendTitle A character string for the title of the legend.
#'   Defaults to `"Climate metric"`.
#'
#' @return A `ggplot` object representing the spatial plot of the climate metric.
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr select
#' @importFrom ggplot2 aes coord_sf geom_sf ggplot labs scale_fill_viridis_c
#' @importFrom rlang sym
#' @importFrom sf st_as_sf st_bbox
#'
#' @examples
#' \dontrun{
#' # Assuming 'dat_clim' is an existing sf object in your package
#' # with a column named "metric" or another relevant climate metric.
#'
#' # Example: Plot climate data using "metric" column
#' plot_climate_metric <- splnr_plot_climData(
#'   df = dat_clim,
#'   colInterest = "metric",
#'   plotTitle = "Annual Climate Warming",
#'   legendTitle = "Warming (°C/year)"
#' )
#' print(plot_climate_metric)
#'
#' # Example with a different color map
#' plot_climate_alt_cmap <- splnr_plot_climData(
#'   df = dat_clim,
#'   colInterest = "metric",
#'   colorMap = "D", # Using 'D' for a different viridis palette
#'   plotTitle = "Climate Metric (Alternative Colors)"
#' )
#' print(plot_climate_alt_cmap)
#' }
splnr_plot_climData <- function(df,
                                colInterest,
                                colorMap = "C",
                                plotTitle = " ",
                                legendTitle = "Climate metric") {

  # Assertions to validate input parameters.
  assertthat::assert_that(
    inherits(df, "sf"),
    msg = "'df' must be an 'sf' object."
  )
  assertthat::assert_that(
    is.character(colInterest),
    msg = "'colInterest' must be a character string."
  )
  assertthat::assert_that(
    colInterest %in% names(df),
    msg = paste0("The column '", colInterest, "' does not exist in the input dataframe 'df'.")
  )
  assertthat::assert_that(
    is.character(colorMap),
    msg = "'colorMap' must be a character string for a 'viridis' palette option."
  )
  assertthat::assert_that(
    is.character(plotTitle),
    msg = "'plotTitle' must be a character string."
  )
  assertthat::assert_that(
    is.character(legendTitle),
    msg = "'legendTitle' must be a character string."
  )

  # Initialize the ggplot object.
  gg <- ggplot2::ggplot() +
    # Add sf layer, filling by the specified climate metric column.
    ggplot2::geom_sf(data = df %>% sf::st_as_sf(), ggplot2::aes(fill = !!rlang::sym(colInterest)), colour = NA) +
    # Apply a viridis continuous color scale for fill.
    ggplot2::scale_fill_viridis_c(
      name = legendTitle, # Set legend title.
      option = colorMap # Apply specified color map.
    ) +
    # Set coordinate limits based on the bounding box of the dataframe.
    ggplot2::coord_sf(xlim = sf::st_bbox(df)$xlim, ylim = sf::st_bbox(df)$ylim) +
    ggplot2::labs(subtitle = plotTitle) # Set plot subtitle.

  return(gg)
}


#' @title Basic Kernel Density Plots for Climate-Smart Spatial Plans
#'
#' @description
#' `splnr_plot_climKernelDensity_Basic()` generates a basic kernel density plot
#' to visualize the distribution of a climate metric within selected and
#' unselected planning units of a `prioritizr` solution.
#'
#' @details
#' This internal function is used by `splnr_plot_climKernelDensity()` when
#' `type = "Basic"`. It creates a ridge plot using `ggridges` to show the
#' density of a climate metric for planning units that were "Selected" versus
#' "Not Selected" in a conservation solution. The "Selected" distribution is
#' typically darker and more opaque, while "Not Selected" is lighter and
#' transparent.
#'
#' The x-axis labels are customized to indicate "more climate-resilient" and
#' "less climate-resilient" based on the minimum and maximum metric values.
#'
#' @param soln The `prioritizr` solution, expected as a data frame (can be an
#'   `sf` object that will be treated as a data frame). It must contain a
#'   `metric` column (numeric) with climate metric information and a `solution_1`
#'   column (numeric, 0 or 1) indicating selected planning units.
#'
#' @return A `ggplot` object representing the kernel density plot.
#' @keywords internal
#' @noRd
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate
#' @importFrom ggplot2 aes element_blank element_line element_text ggplot labs scale_fill_manual scale_x_continuous scale_y_discrete theme theme_bw guide_legend
#' @importFrom rlang .data :=
#'
splnr_plot_climKernelDensity_Basic <- function(soln) {

  # Assertions to validate input parameters.
  assertthat::assert_that(
    inherits(soln, "data.frame"),
    msg = "'soln' must be a data.frame (can be an sf object)."
  )
  assertthat::assert_that(
    "metric" %in% names(soln),
    msg = "'soln' must contain a 'metric' column."
  )
  assertthat::assert_that(
    "solution_1" %in% names(soln),
    msg = "'soln' must contain a 'solution_1' column."
  )
  assertthat::assert_that(
    is.numeric(soln$metric),
    msg = "The 'metric' column in 'soln' must be numeric."
  )
  assertthat::assert_that(
    is.numeric(soln$solution_1) || is.logical(soln$solution_1), # Allow logical too, as it converts to numeric 0/1
    msg = "The 'solution_1' column in 'soln' must be numeric (0/1) or logical (TRUE/FALSE)."
  )

  # Check if ggridges package is installed, if not, stop with an error.
  if (requireNamespace("ggridges", quietly = TRUE) == FALSE){
    stop("To run splnr_plot_climKernelDensity you will need to install the package ggridges.")
  }

  # Add a dummy variable "approach" for the ridge plot's Y-axis.
  soln$approach <- "Ridge"

  # Initialize ggplot object.
  ggRidge <- ggplot2::ggplot() +
    # Add density ridges for selected planning units.
    ggridges::stat_density_ridges(
      data = soln %>% dplyr::filter(.data$solution_1 == 1) %>% dplyr::mutate(solution_1 = "Selected"), # Filter for selected and label.
      ggplot2::aes(x = .data$metric, y = .data$approach, fill = .data$solution_1),
      color = "#194361", quantile_lines = TRUE, quantiles = 2, # Darker color and quantile lines.
      show.legend = TRUE
    ) +
    # Add density ridges for not selected planning units.
    ggridges::stat_density_ridges(
      data = soln %>% dplyr::filter(.data$solution_1 == 0) %>% dplyr::mutate(solution_1 = "Not Selected"), # Filter for not selected and label.
      ggplot2::aes(x = .data$metric, y = .data$approach, fill = .data$solution_1),
      color = "#3182bd", quantile_lines = TRUE, quantiles = 2, # Lighter color and quantile lines.
      alpha = 0.5, # Make semi-transparent.
      show.legend = TRUE
    ) +
    # Customize x-axis to show "more climate-resilient" and "less climate-resilient" at min/max.
    ggplot2::scale_x_continuous(
      name = "Climate resilience metric",
      breaks = c(min(soln$metric, na.rm = TRUE), max(soln$metric, na.rm = TRUE)), # Use actual min/max.
      labels = c("more climate-resilient", "less climate-resilient")
    ) +
    # Customize y-axis with no expansion.
    ggplot2::scale_y_discrete(expand = c(0, 0)) +
    # Set plot labels.
    ggplot2::labs(
      x = "Climate resilience metric",
      y = "Proportion of planning units"
    ) +
    ggplot2::theme_bw() + # Apply black and white theme.
    # Customize theme elements.
    ggplot2::theme(
      axis.ticks = ggplot2::element_line(color = "black", linewidth = 1),
      text = ggplot2::element_text(size = 20),
      axis.line = ggplot2::element_line(colour = "black", linewidth = 1),
      axis.text.y = ggplot2::element_blank(), # Hide y-axis text.
      axis.text.x = ggplot2::element_text(size = 20),
      axis.title = ggplot2::element_text(size = 20),
      legend.title = ggplot2::element_text(color = "black", angle = 90, hjust = 0.5), # Rotate legend title.
      legend.position = "bottom",
      legend.text = ggplot2::element_text(size = 20)
    ) +
    # Manually set fill colors for "Not Selected" and "Selected" in the legend.
    ggplot2::scale_fill_manual(
      name = "", # Empty legend title.
      values = c("Not Selected" = "#c6dbef", "Selected" = "#3182bd"),
      aesthetics = "fill",
      guide = ggplot2::guide_legend(
        override.aes = list(linetype = 0), # Remove linetype from legend.
        nrow = 1 # Single row for legend.
      )
    )
}

#' @title Fancy Kernel Density Plot for a Single Climate-Smart Spatial Plan
#'
#' @description
#' `splnr_plot_climKernelDensity_Fancy()` generates a kernel density plot for
#' a single conservation solution, showing the distribution of a climate metric
#' within selected and unselected planning units.
#'
#' @details
#' This internal function is used by `splnr_plot_climKernelDensity()` when
#' `type = "Normal"`. It accepts a single `prioritizr` solution and produces a
#' ridge plot with:
#' \itemize{
#'   \item A viridis gradient-filled ridge for selected planning units.
#'   \item A grey dotted-outline ridge for unselected planning units.
#'   \item Vertical lines at the median of each group (solid black for selected,
#'     dotted black for unselected).
#'   \item A categorical legend showing the visual style of each group.
#' }
#'
#' To compare two solutions side-by-side, call this function twice and combine
#' the results with \code{patchwork::wrap_plots()}.
#'
#' @param soln A single `prioritizr` solution object (`sf` or `data.frame`)
#'   containing a solution column (numeric 0/1 or logical) and a climate metric
#'   column (numeric).
#' @param solution_name A scalar character string naming the solution column in
#'   `soln`. Defaults to `"solution_1"`.
#' @param climate_name A scalar character string naming the climate metric column
#'   in `soln`. Defaults to `"metric"`.
#' @param colorMap A character string indicating the `viridis` color map to use
#'   for filling the selected areas (e.g., "A", "B", "C", "D", "E"). See
#'   \url{https://ggplot2.tidyverse.org/reference/scale_viridis.html} for all options.
#'   Defaults to `"C"`.
#' @param legendTitle A character string or `expression` for the title of the
#'   viridis colour bar legend. Defaults to
#'   `expression(" \u00B0C y"^"-1" * "")`, representing "°C year⁻¹".
#' @param xAxisLab A character string or `expression` for the x-axis label.
#'   Defaults to `expression("Climate warming ( \u00B0C y"^"-1" * ")")`.
#'
#' @return A `ggplot` object representing the kernel density plot.
#' @keywords internal
#' @noRd
splnr_plot_climKernelDensity_Fancy <- function(soln,
                                               solution_name = "solution_1",
                                               climate_name = "metric",
                                               colorMap = "C",
                                               legendTitle = expression(" \u00B0C y"^"-1" * ""),
                                               xAxisLab = expression("Climate warming ( \u00B0C y"^"-1" * ")")) {

  # --- Input validation -------------------------------------------------------

  assertthat::assert_that(
    inherits(soln, "data.frame"),
    msg = "'soln' must be a data.frame or sf object."
  )
  assertthat::assert_that(
    is.character(solution_name) && length(solution_name) == 1L,
    msg = "'solution_name' must be a single character string."
  )
  assertthat::assert_that(
    is.character(climate_name) && length(climate_name) == 1L,
    msg = "'climate_name' must be a single character string."
  )
  assertthat::assert_that(
    solution_name %in% names(soln),
    msg = paste0("'soln' is missing the solution column '", solution_name, "'.")
  )
  assertthat::assert_that(
    climate_name %in% names(soln),
    msg = paste0("'soln' is missing the climate column '", climate_name, "'.")
  )
  assertthat::assert_that(
    is.numeric(soln[[solution_name]]) || is.logical(soln[[solution_name]]),
    msg = paste0("Column '", solution_name, "' must be numeric (0/1) or logical.")
  )
  assertthat::assert_that(
    is.numeric(soln[[climate_name]]),
    msg = paste0("Climate column '", climate_name, "' must be numeric.")
  )
  assertthat::assert_that(
    is.character(colorMap),
    msg = "'colorMap' must be a character string for a 'viridis' palette option."
  )
  assertthat::assert_that(
    is.vector(legendTitle) || is.expression(legendTitle),
    msg = "'legendTitle' must be a character string or an expression."
  )
  assertthat::assert_that(
    is.vector(xAxisLab) || is.expression(xAxisLab),
    msg = "'xAxisLab' must be a character string or an expression."
  )

  # Check that the optional 'ggridges' package is available before attempting
  # to use it. 'ggridges' is listed under Suggests (not Imports). A missing
  # package produces a cryptic "could not find function" error without this guard.
  if (!requireNamespace("ggridges", quietly = TRUE)) {
    stop(
      "Package 'ggridges' is required for splnr_plot_climKernelDensity_Fancy(). ",
      "Install it with: install.packages('ggridges')",
      call. = FALSE
    )
  }

  # --- Data preparation -------------------------------------------------------

  # Rename the solution and climate columns to standard names so the rest of
  # the function can reference them without tidy-eval gymnastics.
  df <- soln %>%
    tibble::as_tibble() %>%
    dplyr::select(tidyselect::all_of(c(solution_name, climate_name))) %>%
    dplyr::rename(solution_1 = tidyselect::all_of(solution_name),
                  metric     = tidyselect::all_of(climate_name)) %>%
    # A single-solution plot still needs a y-axis grouping variable for ggridges.
    # We use the climate column name as the label so the y-axis is informative
    # when the user inspects the raw plot object.
    dplyr::mutate(approach = climate_name)

  # Compute medians for the vertical reference lines.
  # These are computed from the data rather than passed in so the function
  # remains self-contained and the lines always reflect the actual distribution.
  medians <- df %>%
    dplyr::group_by(.data$solution_1) %>%
    dplyr::summarise(med = stats::median(.data$metric, na.rm = TRUE),
                     .groups = "drop")

  med_selected   <- medians$med[medians$solution_1 == 1]
  med_unselected <- medians$med[medians$solution_1 == 0]

  # Middle colour of the viridis palette — used as the representative fill
  # colour for the "Selected PUs" key in the categorical legend.
  mid_colour <- scales::viridis_pal(option = colorMap)(3)[2]

  # --- Plot construction ------------------------------------------------------

  ggRidge <- ggplot2::ggplot() +
    # Gradient-filled ridge for selected planning units.
    ggridges::geom_density_ridges_gradient(
      data = df %>% dplyr::filter(.data$solution_1 == 1),
      ggplot2::aes(
        x    = .data$metric,
        y    = .data$approach,
        fill = ggplot2::after_stat(.data$x)
      ),
      scale = 1
    ) +
    # Viridis colour scale for the gradient fill (continuous legend).
    ggplot2::scale_fill_viridis_c(
      name   = legendTitle,
      option = colorMap,
      guide  = ggplot2::guide_colorbar(
        barheight = ggplot2::unit(10, "lines"),
        barwidth  = ggplot2::unit(3, "lines")
      )
    ) +
    # Grey dotted ridge for unselected planning units.
    ggridges::geom_density_ridges(
      data = df %>% dplyr::filter(.data$solution_1 == 0),
      ggplot2::aes(x = .data$metric, y = .data$approach),
      alpha = 0.25, linetype = "dotted", scale = 1
    ) +
    # Median line for selected PUs — solid black.
    ggplot2::geom_vline(
      xintercept = med_selected,
      colour     = "black",
      linetype   = "solid",
      linewidth  = 0.8
    ) +
    # Median line for unselected PUs — dotted black.
    ggplot2::geom_vline(
      xintercept = med_unselected,
      colour     = "black",
      linetype   = "dotted",
      linewidth  = 0.8
    ) +
    # Invisible points used solely to generate the categorical legend.
    # shape = 22 (filled square) with override.aes gives legend keys that
    # visually match the ridge appearance without affecting the actual plot.
    ggplot2::geom_point(
      data = data.frame(x = NA_real_, group = "Selected PUs"),
      ggplot2::aes(x = .data$x, y = 1, colour = .data$group),
      alpha = 0, na.rm = TRUE
    ) +
    ggplot2::geom_point(
      data = data.frame(x = NA_real_, group = "Unselected PUs"),
      ggplot2::aes(x = .data$x, y = 1, colour = .data$group),
      alpha = 0, na.rm = TRUE
    ) +
    ggplot2::scale_colour_manual(
      name   = NULL,
      values = c("Selected PUs" = mid_colour, "Unselected PUs" = "grey70"),
      guide  = ggplot2::guide_legend(
        override.aes = list(
          fill     = c(mid_colour, "grey70"),
          colour   = c("black",    "black"),
          linetype = c("solid",    "dotted"),
          shape    = 22,
          size     = 8,
          alpha    = 1
        )
      )
    ) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_discrete(expand = ggplot2::expansion(mult = c(0.01, 0))) +
    ggplot2::labs(x = xAxisLab) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.ticks      = ggplot2::element_line(color = "black", linewidth = 1),
      axis.line       = ggplot2::element_line(colour = "black", linewidth = 1),
      axis.text       = ggplot2::element_text(color = "black", size = 14),
      axis.title.x    = ggplot2::element_text(size = 14),
      axis.title.y    = ggplot2::element_blank(),
      axis.text.y     = ggplot2::element_blank(),
      legend.text     = ggplot2::element_text(size = 15, color = "black"),
      legend.title    = ggplot2::element_text(size = 15, color = "black"),
      legend.title.position = "right"
    )

  return(ggRidge)
}


#' @title Kernel Density Plots for Climate-Smart Spatial Plans
#'
#' @description
#' `splnr_plot_climKernelDensity()` generates kernel density plots for a single
#' climate-smart spatial plan, offering two distinct plotting styles:
#' "Normal" (for publication-quality visualisation) and "Basic" (for simplified
#' visualisation for stakeholders).
#'
#' @details
#' This wrapper function dispatches to either
#' `splnr_plot_climKernelDensity_Fancy()` (for `type = "Normal"`) or
#' `splnr_plot_climKernelDensity_Basic()` (for `type = "Basic"`) based on the
#' `type` parameter.
#'
#' The "Normal" style produces a ridge plot with a viridis gradient fill for
#' selected planning units, a grey dotted ridge for unselected units, vertical
#' median lines, and a categorical legend. The "Basic" style is streamlined for
#' clarity and quick interpretation.
#'
#' To compare two solutions side-by-side, call this function once per solution
#' and combine the results with \code{patchwork::wrap_plots()}.
#'
#' @param soln A single `prioritizr` solution object (`sf` or `data.frame`).
#'   For `type = "Normal"`: must contain the columns named by `solution_name`
#'   and `climate_name`.
#'   For `type = "Basic"`: must be an `sf` object with `solution_1` and
#'   `metric` columns.
#' @param solution_name A scalar character string naming the solution column
#'   (0/1 or logical) in `soln`. Used only for `type = "Normal"`.
#'   Defaults to `"solution_1"`.
#' @param climate_name A scalar character string naming the climate metric
#'   column (numeric) in `soln`. Used only for `type = "Normal"`.
#'   Defaults to `"metric"`.
#' @param type A character string specifying the plotting style. Must be either
#'   `"Normal"` or `"Basic"`. Defaults to `"Normal"`.
#' @param colorMap A character string indicating the `viridis` color map to use
#'   (e.g., "A", "B", "C", "D", "E"). See
#'   \url{https://ggplot2.tidyverse.org/reference/scale_viridis.html} for all options.
#'   Defaults to `"C"`.
#' @param legendTitle A character string or `expression` for the title of the
#'   viridis colour bar legend. Defaults to
#'   `expression(" \u00B0C y"^"-1" * "")`, representing "°C year⁻¹".
#' @param xAxisLab A character string or `expression` for the x-axis label.
#'   Defaults to `expression("Climate warming ( \u00B0C y"^"-1" * ")")`.
#'
#' @return A `ggplot` object representing the kernel density plot.
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom prioritizr problem add_min_set_objective add_relative_targets add_binary_decisions add_default_solver solve.ConservationProblem
#' @importFrom dplyr mutate select left_join row_number
#' @importFrom sf st_drop_geometry
#' @importFrom tidyselect starts_with
#'
#' @examples
#' \dontrun{
#' # Assuming 'dat_species_bin' and 'dat_clim' are existing sf objects
#' # in your package.
#'
#' # Prepare data for a climate-priority area approach (CPA)
#' target <- dat_species_bin %>%
#'   sf::st_drop_geometry() %>%
#'   colnames() %>%
#'   data.frame() %>%
#'   setNames(c("feature")) %>%
#'   dplyr::mutate(target = 0.3)
#'
#' CPA <- splnr_climate_priorityAreaApproach(
#'   features = dat_species_bin,
#'   metric = dat_clim,
#'   targets = target,
#'   direction = -1,
#'   refugiaTarget = 1
#' )
#'
#' out_sf <- CPA$Features %>%
#'   dplyr::mutate(Cost_None = 1, .row_id = dplyr::row_number()) %>%
#'   dplyr::left_join(
#'     dat_clim %>%
#'       sf::st_drop_geometry() %>%
#'       dplyr::mutate(.row_id = dplyr::row_number()),
#'     by = ".row_id"
#'   ) %>%
#'   dplyr::select(-".row_id")
#'
#' # Define features for the prioritizr problem
#' usedFeatures <- out_sf %>%
#'   sf::st_drop_geometry() %>%
#'   dplyr::select(-tidyselect::starts_with("Cost_"), -"metric") %>%
#'   names()
#'
#' # Create and solve a prioritizr problem
#' p1 <- prioritizr::problem(out_sf, usedFeatures, "Cost_None") %>%
#'   prioritizr::add_min_set_objective() %>%
#'   prioritizr::add_relative_targets(CPA$Targets$target) %>%
#'   prioritizr::add_binary_decisions() %>%
#'   prioritizr::add_default_solver(verbose = FALSE)
#'
#' dat_solnClim <- prioritizr::solve.ConservationProblem(p1)
#'
#' # Example 1: Basic kernel density plot
#' plot_basic_kde <- splnr_plot_climKernelDensity(soln = dat_solnClim, type = "Basic")
#' print(plot_basic_kde)
#'
#' # Example 2: Normal (Fancy) kernel density plot
#' plot_normal_kde <- splnr_plot_climKernelDensity(
#'   soln = dat_solnClim,
#'   type = "Normal"
#' )
#' print(plot_normal_kde)
#'
#' # Example 3: Compare two solutions side-by-side using patchwork
#' dat_solnClim_2 <- dat_solnClim %>%
#'   dplyr::mutate(solution_1 = sample(c(0L, 1L), dplyr::n(), replace = TRUE))
#'
#' plot_compare <- patchwork::wrap_plots(
#'   splnr_plot_climKernelDensity(soln = dat_solnClim,   type = "Normal",
#'                                legendTitle = "Scenario 1", xAxisLab = "Climate metric"),
#'   splnr_plot_climKernelDensity(soln = dat_solnClim_2, type = "Normal",
#'                                legendTitle = "Scenario 2", xAxisLab = "Climate metric"),
#'   ncol = 1
#' )
#' print(plot_compare)
#'
#' # Example 4: Custom colour map and labels
#' plot_custom <- splnr_plot_climKernelDensity(
#'   soln = dat_solnClim,
#'   type = "Normal",
#'   colorMap = "plasma",
#'   legendTitle = "Climate Value",
#'   xAxisLab = "Climate Metric (units)"
#' )
#' print(plot_custom)
#' }
splnr_plot_climKernelDensity <- function(soln,
                                         solution_name = "solution_1",
                                         climate_name = "metric",
                                         type = "Normal",
                                         colorMap = "C",
                                         legendTitle = expression(" \u00B0C y"^"-1" * ""),
                                         xAxisLab = expression("Climate warming ( \u00B0C y"^"-1" * ")")) {

  # Assertions to validate input parameters.
  assertthat::assert_that(
    is.character(type),
    msg = "'type' must be a character string ('Normal' or 'Basic')."
  )
  assertthat::assert_that(
    type %in% c("Normal", "Basic"),
    msg = "'type' must be either 'Normal' or 'Basic'."
  )
  assertthat::assert_that(
    is.character(solution_name) && length(solution_name) == 1L,
    msg = "'solution_name' must be a single character string."
  )
  assertthat::assert_that(
    is.character(climate_name) && length(climate_name) == 1L,
    msg = "'climate_name' must be a single character string."
  )
  assertthat::assert_that(
    is.character(colorMap),
    msg = "'colorMap' must be a character string for a 'viridis' palette option."
  )
  assertthat::assert_that(
    is.vector(legendTitle) || is.expression(legendTitle),
    msg = "'legendTitle' must be a character string or an expression."
  )
  assertthat::assert_that(
    is.vector(xAxisLab) || is.expression(xAxisLab),
    msg = "'xAxisLab' must be a character string or an expression."
  )

  # Conditional logic to call either Basic or Fancy plotting function.
  if (type == "Normal") {
    # For type = "Normal", soln must be a single sf or data.frame.
    # To compare two solutions, call this function twice and combine with
    # patchwork::wrap_plots().
    if (!inherits(soln, "data.frame")) {
      stop("For 'type = \"Normal\"', 'soln' must be a single sf or data.frame object.")
    }
    ggclimDens <- splnr_plot_climKernelDensity_Fancy(
      soln          = soln,
      solution_name = solution_name,
      climate_name  = climate_name,
      colorMap      = colorMap,
      legendTitle   = legendTitle,
      xAxisLab      = xAxisLab
    )
  } else if (type == "Basic") {
    # If type is "Basic", expect a single sf object.
    if (!inherits(soln, "sf")) {
      stop("For 'type = \"Basic\"', 'soln' must be a single sf object.")
    }
    ggclimDens <- splnr_plot_climKernelDensity_Basic(soln = soln)
  } else {
    # This case should ideally be caught by initial assertthat, but kept as a fallback.
    stop("Invalid 'type' specified. Must be 'Normal' or 'Basic'.")
  }

  return(ggclimDens)
}
