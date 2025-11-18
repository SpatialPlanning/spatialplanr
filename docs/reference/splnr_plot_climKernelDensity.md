# Kernel Density Plots for Climate-Smart Spatial Plans

`splnr_plot_climKernelDensity()` generates kernel density plots for
climate-smart spatial plans, offering two distinct plotting styles:
"Normal" (for publication-quality comparison of multiple solutions) and
"Basic" (for simplified visualization for stakeholders).

## Usage

``` r
splnr_plot_climKernelDensity(
  soln,
  solution_names = "solution_1",
  climate_names = "metric",
  type = "Normal",
  colorMap = "C",
  legendTitle = expression(" °C y"^"-1" * ""),
  xAxisLab = expression("Climate warming ( °C y"^"-1" * ")")
)
```

## Arguments

- soln:

  For `type = "Normal"`: A `list` of `prioritizr` solution objects
  (e.g., `list(s1, s2)`). Each solution must contain a `metric` column
  and a `solution_1` column. For `type = "Basic"`: A single `prioritizr`
  solution `sf` object.

- solution_names:

  A character vector of names corresponding to each solution in `soln`
  when `type = "Normal"`. Not used for `type = "Basic"`. Defaults to
  `NA`.

- climate_names:

  A character string of the name of the climate

- type:

  A character string specifying the plotting style. Must be either
  `"Normal"` or `"Basic"`. Defaults to `"Normal"`.

- colorMap:

  A character string indicating the `viridis` color map to use (e.g.,
  "A", "B", "C", "D", "E"). See
  <https://ggplot2.tidyverse.org/reference/scale_viridis.html> for all
  options. Defaults to `"C"`.

- legendTitle:

  A character string or `expression` for the title of the legend.
  Defaults to `expression(" \u00B0C y"^"-1" * "")`, representing "°C
  year⁻¹".

- xAxisLab:

  A character string or `expression` for the x-axis label, depending on
  the climate metric input. Defaults to
  `expression("Climate warming ( \u00B0C y"^"-1" * ")")`.

## Value

A `ggplot` object representing the kernel density plot.

## Details

This wrapper function intelligently dispatches to either
`splnr_plot_climKernelDensity_Fancy()` (for `type = "Normal"`) or
`splnr_plot_climKernelDensity_Basic()` (for `type = "Basic"`) based on
the `type` parameter.

The "Normal" (Fancy) style is suitable for detailed comparisons,
accommodating a list of solutions and custom axis labels, while the
"Basic" style is streamlined for clarity and quick interpretation, ideal
for stakeholder engagement.

Both underlying functions require a `prioritizr` solution containing a
climate metric column with climate metric information and a prioritizr
solution column indicating selected planning units.

## Examples

``` r
if (FALSE) { # \dontrun{
# Assuming 'dat_species_bin' and 'dat_clim' are existing sf objects
# in your package.

# Prepare data for a climate-priority area approach (CPA)
target <- dat_species_bin %>%
  sf::st_drop_geometry() %>%
  colnames() %>%
  data.frame() %>%
  setNames(c("feature")) %>%
  dplyr::mutate(target = 0.3)

CPA <- splnr_climate_priorityAreaApproach(
  features = dat_species_bin,
  metric = dat_clim,
  targets = target,
  direction = -1,
  refugiaTarget = 1
)

# Join climate metric to features for the problem
out_sf <- CPA$Features %>%
  dplyr::mutate(Cost_None = rep(1, dim(.)[[1]])) %>% # Ensure enough costs for PUs
  sf::st_join(dat_clim, join = sf::st_equals)

# Define features for the prioritizr problem
usedFeatures <- out_sf %>%
  sf::st_drop_geometry() %>%
  dplyr::select(-tidyselect::starts_with("Cost_"), -"metric") %>%
  names()

# Create and solve a prioritizr problem
p1 <- prioritizr::problem(out_sf, usedFeatures, "Cost_None") %>%
  prioritizr::add_min_set_objective() %>%
  prioritizr::add_relative_targets(CPA$Targets$target) %>%
  prioritizr::add_binary_decisions() %>%
  prioritizr::add_default_solver(verbose = FALSE)

dat_solnClim <- prioritizr::solve.ConservationProblem(p1)

# Example 1: Basic kernel density plot
plot_basic_kde <- splnr_plot_climKernelDensity(soln = dat_solnClim, type = "Basic")
print(plot_basic_kde)

# Example 2: Normal (Fancy) kernel density plot for a single solution
plot_normal_kde_single <- splnr_plot_climKernelDensity(
  soln = list(dat_solnClim),
  solution_names = c("Solution 1"),
  type = "Normal"
)
print(plot_normal_kde_single)

# Example 3: Normal (Fancy) plot comparing two solutions (create a dummy second solution)
# For demonstration, let's create another dummy solution
dat_solnClim_2 <- dat_solnClim %>%
  dplyr::mutate(solution_1 = sample(c(0, 1), n(), replace = TRUE)) # Randomize selection

plot_normal_kde_multi <- splnr_plot_climKernelDensity(
  soln = list(dat_solnClim, dat_solnClim_2),
  solution_names = c("Solution A", "Solution B"),
  climate_names = "metric",
  type = "Normal",
  colorMap = "plasma",
  legendTitle = "Climate Value",
  xAxisLab = "Climate Metric (units)"
)
print(plot_normal_kde_multi)
} # }
```
