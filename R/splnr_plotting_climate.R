
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
splnr_plot_climData <- function(df, colInterest, colorMap = "C",
                                plotTitle = " ", legendTitle = "Climate metric") {

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
      legend.title = ggplot2::element_text(color = "black", angle = 270, hjust = 0.5), # Rotate legend title.
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

#' @title Fancy Kernel Density Plots for Climate-Smart Spatial Plans
#'
#' @description
#' `splnr_plot_climKernelDensity_Fancy()` generates a more elaborate kernel
#' density plot suitable for comparing distributions of a climate metric across
#' multiple conservation solutions.
#'
#' @details
#' This internal function is used by `splnr_plot_climKernelDensity()` when
#' `type = "Normal"`. It accepts a list of `prioritizr` solutions and their
#' corresponding names, allowing for a comparative visualization of climate
#' metric distributions between different scenarios.
#'
#' The function pivots the data to a long format, enabling `ggridges` to plot
#' overlapping density ridges for each solution, with selected areas filled
#' by a gradient based on the metric value and unselected areas shown as dotted
#' outlines. This provides a detailed visual comparison of how climate metrics
#' vary across different spatial plans.
#'
#' @param solution_list A `list` of `prioritizr` solution objects. Each solution
#'   (e.g., `s1`, `s2`) in the list must be an `sf` or `data.frame` object
#'   containing a `metric` column (numeric) and a `solution_1` column (numeric, 0 or 1).
#' @param names A character vector of names corresponding to each solution in
#'   `solution_list`. The length of this vector must match the length of `solution_list`.
#' @param colorMap A character string indicating the `viridis` color map to use
#'   for filling the selected areas (e.g., "A", "B", "C", "D", "E"). See
#'   \url{https://ggplot2.tidyverse.org/reference/scale_viridis.html} for all options.
#'   Defaults to `"C"`.
#' @param legendTitle A character string or `expression` for the title of the legend.
#'   Defaults to `expression(" \u00B0C y"^"-1" * "")`, representing "°C year⁻¹".
#' @param xAxisLab A character string or `expression` for the x-axis label,
#'   depending on the climate metric input. Defaults to
#'   `expression("Climate warming ( \u00B0C y"^"-1" * ")")`.
#'
#' @return A `ggplot` object representing the fancy kernel density plot.
#' @keywords internal
#' @noRd
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate rename select
#' @importFrom forcats fct_relevel
#' @importFrom ggplot2 aes after_stat element_blank element_line element_text expansion ggplot labs scale_fill_viridis_c scale_x_continuous scale_y_discrete theme theme_bw
#' @importFrom rlang .data sym
#' @importFrom tibble as_tibble
#' @importFrom tidyr pivot_longer
#'
splnr_plot_climKernelDensity_Fancy <- function(solution_list,
                                               names,
                                               colorMap = "C",
                                               legendTitle = expression(" \u00B0C y"^"-1" * ""),
                                               xAxisLab = expression("Climate warming ( \u00B0C y"^"-1" * ")")) {

  # Assertions to validate input parameters.
  assertthat::assert_that(
    is.list(solution_list),
    msg = "'solution_list' must be a list of prioritizr solutions."
  )
  assertthat::assert_that(
    length(solution_list) > 0,
    msg = "'solution_list' must contain at least one solution."
  )
  assertthat::assert_that(
    is.character(names),
    msg = "'names' must be a character vector of solution names."
  )
  assertthat::assert_that(
    length(names) == length(solution_list),
    msg = "The length of 'names' must match the length of 'solution_list'."
  )
  assertthat::assert_that(
    is.character(colorMap),
    msg = "'colorMap' must be a character string for a 'viridis' palette option."
  )
  assertthat::assert_that(
    is.vector(legendTitle) || is.expression(legendTitle), # Allow vector (character) or expression.
    msg = "'legendTitle' must be a character string or an expression."
  )
  assertthat::assert_that(
    is.vector(xAxisLab) || is.expression(xAxisLab), # Allow vector (character) or expression.
    msg = "'xAxisLab' must be a character string or an expression."
  )

  # Check that each solution in the list has 'metric' and 'solution_1' columns
  for (i in seq_along(solution_list)) {
    assertthat::assert_that(
      "metric" %in% names(solution_list[[i]]),
      msg = paste0("Solution ", i, " in 'solution_list' is missing the 'metric' column.")
    )
    assertthat::assert_that(
      "solution_1" %in% names(solution_list[[i]]),
      msg = paste0("Solution ", i, " in 'solution_list' is missing the 'solution_1' column.")
    )
    assertthat::assert_that(
      is.numeric(solution_list[[i]]$metric),
      msg = paste0("The 'metric' column in solution ", i, " must be numeric.")
    )
    assertthat::assert_that(
      is.numeric(solution_list[[i]]$solution_1) || is.logical(solution_list[[i]]$solution_1),
      msg = paste0("The 'solution_1' column in solution ", i, " must be numeric (0/1) or logical.")
    )
  }


  #TODO Write check for ggridges


  list_sol <- list()
  group_name <- "approach" # Define a column name for grouping different solutions.

  # Loop through each solution in the list to prepare data for plotting.
  for (i in 1:length(names)) {
    list_sol[[i]] <- solution_list[[i]] %>%
      tibble::as_tibble() %>% # Convert to tibble to ensure consistent data frame behavior.
      dplyr::select("solution_1", "metric") %>% # Select only solution status and metric.
      dplyr::rename(!!rlang::sym(names[i]) := "metric") %>% # Rename 'metric' column to the solution's name.
      # Pivot data longer to enable plotting multiple solutions on one plot.
      tidyr::pivot_longer(!!rlang::sym(names[i]), names_to = group_name, values_to = "metric")
  }

  # Combine all processed data frames into a single data frame.
  df <- do.call(rbind, list_sol) %>%
    # Relevel the 'approach' factor to control the order of ridges in the plot.
    dplyr::mutate(approach = forcats::fct_relevel(.data$approach, rev))

  # Initialize ggplot object.
  ggRidge <- ggplot2::ggplot() +
    # Add density ridges with gradient fill for selected planning units.
    ggridges::geom_density_ridges_gradient(
      data = df %>% dplyr::filter(.data$solution_1 == 1), # Filter for selected units.
      ggplot2::aes(
        x = .data$metric,
        y = .data$approach,
        fill = ggplot2::after_stat(.data$x), # Fill based on the x-value (metric).
      ), scale = 1 # Set ridge scale.
    ) +
    # Apply a viridis color scale for the gradient fill.
    ggplot2::scale_fill_viridis_c(name = legendTitle, option = colorMap) +
    # Add density ridges for not selected planning units with dotted lines and transparency.
    ggridges::geom_density_ridges(
      data = df %>% dplyr::filter(.data$solution_1 == 0), # Filter for not selected units.
      ggplot2::aes(x = .data$metric, y = .data$approach),
      alpha = 0.25, linetype = "dotted", scale = 1 # Set transparency, linetype, and scale.
    ) +
    # (Commented out in original: Optional vertical line for mean climate warming)
    # geom_vline(xintercept = climate$mean_climate_warming,
    #            linetype = "dashed", color = "tan1", size = 0.5) +
    # Set x-axis limits with no expansion.
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    # Set y-axis limits with expansion.
    ggplot2::scale_y_discrete(expand = ggplot2::expansion(mult = c(0.01, 0))) +
    ggplot2::labs(x = xAxisLab) + # Set x-axis label.
    ggplot2::theme_bw() + # Apply black and white theme.
    # Customize theme elements.
    ggplot2::theme(
      axis.ticks = ggplot2::element_line(color = "black", linewidth = 1),
      axis.line = ggplot2::element_line(colour = "black", linewidth = 1),
      axis.text = ggplot2::element_text(color = "black", size = 14),
      axis.title.x = ggplot2::element_text(size = 14),
      axis.title.y = ggplot2::element_blank(), # Hide y-axis title.
      axis.text.y = ggplot2::element_blank(), # Hide y-axis text.
      legend.text = ggplot2::element_text(size = 15, color = "black"),
      legend.title = ggplot2::element_text(size = 15, color = "black")
    )
}


#' @title Kernel Density Plots for Climate-Smart Spatial Plans
#'
#' @description
#' `splnr_plot_climKernelDensity()` generates kernel density plots for
#' climate-smart spatial plans, offering two distinct plotting styles:
#' "Normal" (for publication-quality comparison of multiple solutions) and
#' "Basic" (for simplified visualization for stakeholders).
#'
#' @details
#' This wrapper function intelligently dispatches to either
#' `splnr_plot_climKernelDensity_Fancy()` (for `type = "Normal"`) or
#' `splnr_plot_climKernelDensity_Basic()` (for `type = "Basic"`) based on the
#' `type` parameter.
#'
#' The "Normal" (Fancy) style is suitable for detailed comparisons,
#' accommodating a list of solutions and custom axis labels, while the "Basic"
#' style is streamlined for clarity and quick interpretation, ideal for
#' stakeholder engagement.
#'
#' Both underlying functions require a `prioritizr` solution containing a
#' `metric` column with climate metric information and a `solution_1` column
#' indicating selected planning units.
#'
#' @param soln For `type = "Normal"`: A `list` of `prioritizr` solution objects
#'   (e.g., `list(s1, s2)`). Each solution must contain a `metric` column and
#'   a `solution_1` column.
#'   For `type = "Basic"`: A single `prioritizr` solution `sf` object.
#' @param names A character vector of names corresponding to each solution in
#'   `soln` when `type = "Normal"`. Not used for `type = "Basic"`.
#'   Defaults to `NA`.
#' @param type A character string specifying the plotting style. Must be either
#'   `"Normal"` or `"Basic"`. Defaults to `"Normal"`.
#' @param colorMap A character string indicating the `viridis` color map to use
#'   (e.g., "A", "B", "C", "D", "E"). See
#'   \url{https://ggplot2.tidyverse.org/reference/scale_viridis.html} for all options.
#'   Defaults to `"C"`.
#' @param legendTitle A character string or `expression` for the title of the legend.
#'   Defaults to `expression(" \u00B0C y"^"-1" * "")`, representing "°C year⁻¹".
#' @param xAxisLab A character string or `expression` for the x-axis label,
#'   depending on the climate metric input. Defaults to
#'   `expression("Climate warming ( \u00B0C y"^"-1" * ")")`.
#'
#' @return A `ggplot` object representing the kernel density plot.
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom prioritizr problem add_min_set_objective add_relative_targets add_binary_decisions add_default_solver solve.ConservationProblem
#' @importFrom dplyr mutate select
#' @importFrom sf st_drop_geometry st_join
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
#' # Join climate metric to features for the problem
#' out_sf <- CPA$Features %>%
#'   dplyr::mutate(Cost_None = rep(1, dim(.)[[1]])) %>% # Ensure enough costs for PUs
#'   sf::st_join(dat_clim, join = sf::st_equals)
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
#' # Example 2: Normal (Fancy) kernel density plot for a single solution
#' plot_normal_kde_single <- splnr_plot_climKernelDensity(
#'   soln = list(dat_solnClim),
#'   names = c("Solution 1"),
#'   type = "Normal"
#' )
#' print(plot_normal_kde_single)
#'
#' # Example 3: Normal (Fancy) plot comparing two solutions (create a dummy second solution)
#' # For demonstration, let's create another dummy solution
#' dat_solnClim_2 <- dat_solnClim %>%
#'   dplyr::mutate(solution_1 = sample(c(0, 1), n(), replace = TRUE)) # Randomize selection
#'
#' plot_normal_kde_multi <- splnr_plot_climKernelDensity(
#'   soln = list(dat_solnClim, dat_solnClim_2),
#'   names = c("Solution A", "Solution B"),
#'   type = "Normal",
#'   colorMap = "plasma",
#'   legendTitle = "Climate Value",
#'   xAxisLab = "Climate Metric (units)"
#' )
#' print(plot_normal_kde_multi)
#' }
splnr_plot_climKernelDensity <- function(soln,
                                         names = NA,
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
    is.character(names) || is.na(names),
    msg = "'names' must be a character vector or NA."
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
    # If type is "Normal", expect a list of solutions.
    if (inherits(soln, "list") == FALSE) {
      stop("For 'type = \"Normal\"', 'soln' must be a list of prioritizr solutions.")
    } else if (inherits(soln, "list")) {
      # Ensure 'names' matches the number of solutions if 'type' is "Normal" and 'names' is provided.
      if (!is.na(names[1]) && length(names) != length(soln)) {
        stop("When 'type = \"Normal\"' and 'names' are provided, the length of 'names' must match the number of solutions in 'soln'.")
      } else if (is.na(names[1])) {
        # If names are not provided, create default names.
        names <- paste0("Solution ", seq_along(soln))
      }
      # Call the fancy kernel density plotting function.
      ggclimDens <- splnr_plot_climKernelDensity_Fancy(
        solution_list = soln, names = names, colorMap = colorMap,
        legendTitle = legendTitle, xAxisLab = xAxisLab
      )
    }
  } else if (type == "Basic") {
    # If type is "Basic", expect a single sf object.
    if (inherits(soln, "sf") == FALSE) {
      stop("For 'type = \"Basic\"', 'soln' must be a single sf object.")
    } else if (inherits(soln, "sf")) {
      # Call the basic kernel density plotting function.
      ggclimDens <- splnr_plot_climKernelDensity_Basic(soln = soln)
    }
  } else {
    # This case should ideally be caught by initial assertthat, but kept as a fallback.
    stop("Invalid 'type' specified. Must be 'Normal' or 'Basic'.")
  }

  return(ggclimDens)
}
