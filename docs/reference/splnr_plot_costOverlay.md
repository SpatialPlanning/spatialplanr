# Plot Cost Overlay on Solution

The `splnr_plot_costOverlay()` function visualizes the cost of each
Planning Unit overlaid on the solution of a `prioritizr` conservation
problem. This allows for a customizable `ggplot2` visualization,
highlighting the costs within selected Planning Units.

## Usage

``` r
splnr_plot_costOverlay(
  soln,
  cost = NA,
  costName = "Cost",
  legendTitle = "Cost",
  plotTitle = "Solution overlaid with cost"
)
```

## Arguments

- soln:

  The `prioritizr` solution object, expected as an `sf` object,
  containing at least a `solution_1` column.

- cost:

  An `sf` object containing the cost data for Planning Units. If the
  `prioritizr` solution `soln` already contains the cost column
  specified by `costName`, this parameter can be `NA` (default).
  Otherwise, provide an `sf` object with the cost data.

- costName:

  A character string specifying the name of the cost column within the
  `soln` object or the `Cost` object. Defaults to `"Cost"`.

- legendTitle:

  A character string for the title of the cost legend. Defaults to
  `"Cost"`.

- plotTitle:

  A character string for the subtitle of the plot. Defaults to
  `"Solution overlaid with cost"`.

## Value

A `ggplot` object representing the solution with cost overlay.

## Details

This function requires a `prioritizr` solution as an `sf` object, which
must contain a `solution_1` column indicating selected (1) or unselected
(0) Planning Units. It also requires a cost column, either present
within the `soln` object or provided separately via the `Cost`
parameter.

The function filters the solution to show only the selected Planning
Units and then overlays these with a gradient representing the cost.
This output is a `ggplot` object that can be further customized using
[`splnr_gg_add()`](https://mathmarecol.github.io/spatialplanr/reference/splnr_gg_add.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# Assuming 'dat_species_bin' is an existing sf object in your package.

# Create a dummy prioritizr problem and solve it for demonstration.
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

# Plot the solution overlaid with cost
plot_cost_overlay <- splnr_plot_costOverlay(soln = dat_soln)
print(plot_cost_overlay)

# Example: If cost is in a separate sf object (e.g., dat_PUs with a cost column)
# Create a dummy cost column in dat_PUs for this example
# Replace this with your actual cost data if it's external
dat_PUs_with_cost <- dat_PUs %>% dplyr::mutate(MyCost = runif(n = dim(.)[[1]]))
plot_cost_overlay_external <- splnr_plot_costOverlay(
  soln = dat_soln,
  cost = dat_PUs_with_cost,
  costName = "MyCost",
  legendTitle = "Custom Cost",
  plotTitle = "Solution with External Cost"
)
print(plot_cost_overlay_external)
} # }
```
