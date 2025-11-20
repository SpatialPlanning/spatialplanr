# Plot Importance Score of Planning Units

The `splnr_plot_importanceScore()` function visualizes the importance
scores (irreplaceability) of Planning Units from a `prioritizr`
conservation problem using `ggplot2`. It supports different methods for
calculating importance scores.

## Usage

``` r
splnr_plot_importanceScore(
  soln,
  pDat,
  method = "Ferrier",
  plotTitle = "",
  colorMap = "A",
  decimals = 4,
  legendTitle = "Importance Score"
)
```

## Arguments

- soln:

  The `prioritizr` solution object, expected as an `sf` object. It
  should contain a `solution_1` column.

- pDat:

  The `prioritizr` problem object that was solved to generate `soln`.

- method:

  A character string specifying the method for calculating importance
  scores. Must be one of `"Ferrier"`, `"RWR"`, or `"RC"`. Defaults to
  `"Ferrier"`.

- plotTitle:

  A character string for the title of the plot. Defaults to `""`.

- colorMap:

  A character string indicating the `viridis` color map to use (e.g.,
  "A", "B", "C", "D", "E"). See
  <https://ggplot2.tidyverse.org/reference/scale_viridis.html> for all
  options. Defaults to `"A"`.

- decimals:

  The number of decimal places to display for the importance scores in
  the legend. Ferrier Score often benefits from a higher number of
  decimals (\>4). Defaults to `4`.

- legendTitle:

  A character string for the title of the legend. Defaults to
  `"Importance Score"`.

## Value

A `ggplot` object representing the plot of importance scores.

## Details

Importance scores quantify the irreplaceability of a Planning Unit in a
conservation solution. This function leverages the `prioritizr` package
to calculate and plot three different types of importance scores:

- **"Ferrier"**: The Ferrier Score, which is applicable only with the
  minimum set objective function. It often requires a higher number of
  decimals (e.g., \>4) for accurate representation.

- **"RWR"**: Rarity Weighted Richness Score.

- **"RC"**: Replacement Cost. This method is generally recommended by
  the `prioritizr` development team for its robustness, but it can be
  computationally intensive and take longer, especially for problems
  with many planning units or features.

The function outputs a `ggplot` object that can be combined with the
`spatialplanr` function
[`splnr_gg_add()`](https://mathmarecol.github.io/spatialplanr/reference/splnr_gg_add.md)
for further customization.

## Examples

``` r
if (FALSE) { # \dontrun{
# Assuming 'dat_species_bin' and 'dat_PUs' are existing sf objects in your package.

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

# Plot importance score using the "Ferrier" method.
plot_ferrier_importance <- splnr_plot_importanceScore(
  soln = dat_soln,
  pDat = dat_problem,
  method = "Ferrier",
  decimals = 4,
  plotTitle = "Ferrier Importance Score"
)
print(plot_ferrier_importance)

# Plot importance score using the "RWR" (Rarity Weighted Richness) method.
plot_rwr_importance <- splnr_plot_importanceScore(
  soln = dat_soln,
  pDat = dat_problem,
  method = "RWR",
  decimals = 2,
  plotTitle = "Rarity Weighted Richness"
)
print(plot_rwr_importance)
} # }
```
