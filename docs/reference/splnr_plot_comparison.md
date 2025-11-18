# Plot Solution Comparison

The `splnr_plot_comparison()` function spatially visualizes the
differences between two `prioritizr` conservation solutions. This helps
in understanding which Planning Units are common, added, or removed
between two scenarios.

## Usage

``` r
splnr_plot_comparison(
  soln1,
  soln2,
  legendTitle = "Scenario 2 compared to Scenario 1:"
)
```

## Arguments

- soln1:

  The first `prioritizr` solution, expected as an `sf` object with a
  `solution_1` column. This serves as the baseline for comparison.

- soln2:

  The second `prioritizr` solution, expected as an `sf` object with a
  `solution_1` column. This is the solution being compared against
  `soln1`.

- legendTitle:

  A character string for the title of the legend. Defaults to
  `"Scenario 2 compared to Scenario 1:"`.

## Value

A `ggplot` object representing the spatial comparison of the two
solutions.

## Details

Conservation planning often involves comparing outputs from different
conservation problems or scenarios. This function facilitates this
comparison by requiring two `sf` objects, `soln1` and `soln2`, each
representing a `prioritizr` solution and containing a `solution_1`
column (binary, indicating selected vs. not selected).

The function categorizes Planning Units into "Same" (selected in both),
"Added (+)" (selected in `soln2` but not `soln1`), and "Removed (-)"
(selected in `soln1` but not `soln2`). It then plots these categories
with distinct colors for clear visualization. The output is a `ggplot`
object that can be combined with
[`splnr_gg_add()`](https://mathmarecol.github.io/spatialplanr/reference/splnr_gg_add.md)
for further customization.

## Examples

``` r
if (FALSE) { # \dontrun{
# Assuming 'dat_species_bin' is an existing sf object in your package.

# Create Problem 1 with 30% target and solve it.
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

# Create Problem 2 with 50% target and solve it.
dat_problem2 <- prioritizr::problem(
  dat_species_bin %>%
    dplyr::mutate(Cost = runif(n = dim(.)[[1]])),
  features = c("Spp1", "Spp2", "Spp3", "Spp4", "Spp5"),
  cost_column = "Cost"
) %>%
  prioritizr::add_min_set_objective() %>%
  prioritizr::add_relative_targets(0.5) %>%
  prioritizr::add_binary_decisions() %>%
  prioritizr::add_default_solver(verbose = FALSE)

dat_soln2 <- dat_problem2 %>%
  prioritizr::solve.ConservationProblem()

# Plot the comparison between the two solutions.
plot_comparison <- splnr_plot_comparison(dat_soln, dat_soln2)
print(plot_comparison)
} # }
```
