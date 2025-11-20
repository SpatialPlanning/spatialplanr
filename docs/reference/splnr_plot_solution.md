# Plot `prioritizr` Solution

The `splnr_plot_solution()` function visualizes the solution of a
`prioritizr` conservation problem using `ggplot2`. It can handle
single-zone and multi-zone solutions, offering customization for colors
and legend.

## Usage

``` r
splnr_plot_solution(
  soln,
  colorVals = c("#c6dbef", "#3182bd"),
  showLegend = TRUE,
  legendLabels = c("Not selected", "Selected"),
  plotTitle = "Solution",
  legendTitle = "Planning Units",
  zones = FALSE
)
```

## Arguments

- soln:

  The `prioritizr` solution object, expected as an `sf` object.

- colorVals:

  A character vector of color values. For single-zone problems, this
  should typically be two colors (for "Not selected" and "Selected").
  For multi-zone problems, the length should match the number of zones
  plus one (for "Not selected").

- showLegend:

  A logical value indicating whether to display the legend of the
  solution. Defaults to `TRUE`.

- legendLabels:

  A character vector of strings to label the legend values. Its length
  must match the number of levels in the solution (e.g., "Not selected",
  "Selected" for single zone; "Not selected", "Zone 1", "Zone 2" for two
  zones).

- plotTitle:

  A character string for the title of the plot. Can be empty (`""`).
  Defaults to `"Solution"`.

- legendTitle:

  A character string for the title of the legend. Can be empty (`""`).
  Defaults to `"Planning Units"`.

- zones:

  A logical value. Set to `TRUE` if the `prioritizr` solution contains
  multiple zones (i.e., it's a multi-zone problem). Defaults to `FALSE`.

## Value

A `ggplot` object representing the plot of the conservation solution.

## Details

This function requires a `prioritizr` solution object, which should be
an `sf` object containing at least a `solution_1` column (for
single-zone problems) or `solution_1_zone1`, `solution_1_zone2`, etc.
(for multi-zone problems). It outputs a `ggplot` object, which can be
further customized by combining it with the `spatialplanr` function
[`splnr_gg_add()`](https://mathmarecol.github.io/spatialplanr/reference/splnr_gg_add.md).

For multi-zone problems (`zones = TRUE`), the function sums the selected
zones for each Planning Unit and plots the resulting combined selection.
The `colorVals` and `legendLabels` should be provided to match the
number of selection levels (e.g., "Not selected", "Zone 1", "Zone 2",
etc.).

## Examples

``` r
if (FALSE) { # \dontrun{
# Assuming 'dat_species_bin' is an existing sf object in your package.

# Example 1: Plotting a single-zone prioritizr solution
dat_problem <- prioritizr::problem(
  dat_species_bin %>% dplyr::mutate(Cost = runif(n = dim(.)[[1]])),
  features = c("Spp1", "Spp2", "Spp3", "Spp4", "Spp5"),
  cost_column = "Cost"
) %>%
  prioritizr::add_min_set_objective() %>%
  prioritizr::add_relative_targets(0.3) %>%
  prioritizr::add_binary_decisions() %>%
  prioritizr::add_default_solver(verbose = FALSE)

dat_soln <- dat_problem %>%
  prioritizr::solve.ConservationProblem()

plot_soln_single_zone <- splnr_plot_solution(dat_soln)
print(plot_soln_single_zone)

# Example 2: Plotting a multi-zone prioritizr solution
# Create targets for two zones
t2 <- matrix(NA, ncol = 2, nrow = 5)
t2[, 1] <- 0.1
t2[, 2] <- 0.05

# Define zones for species
z2 <- prioritizr::zones(
  "zone 1" = c("Spp1", "Spp2", "Spp3", "Spp4", "Spp5"),
  "zone 2" = c("Spp1", "Spp2", "Spp3", "Spp4", "Spp5")
)

# Create a multi-zone problem (requires as many cost columns as zones)
p2 <- prioritizr::problem(
  dat_species_bin %>% dplyr::mutate(
    Cost1 = runif(n = dim(.)[[1]]),
    Cost2 = runif(n = dim(.)[[1]])
  ),
  z2,
  cost_column = c("Cost1", "Cost2")
) %>%
  prioritizr::add_min_set_objective() %>%
  prioritizr::add_relative_targets(t2) %>%
  prioritizr::add_binary_decisions() %>%
  prioritizr::add_default_solver(verbose = FALSE)

s2 <- p2 %>%
  prioritizr::solve.ConservationProblem()

plot_soln_multi_zone <- splnr_plot_solution(s2,
  zones = TRUE,
  colorVals = c("#c6dbef", "#3182bd", "black"), # Colors for Not selected, Zone 1, Zone 2
  legendLabels = c("Not selected", "Zone 1", "Zone 2")
)
print(plot_soln_multi_zone)
} # }
```
