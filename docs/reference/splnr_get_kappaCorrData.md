# Prepare Data to Plot Cohen's Kappa Correlation Matrix

`splnr_get_kappaCorrData()` calculates Cohen's Kappa correlation
coefficients between a list of `prioritizr` conservation solutions. The
output is a symmetrical matrix suitable for visualizing pairwise
agreement using a heatmap.

## Usage

``` r
splnr_get_kappaCorrData(sol, name_sol)
```

## Arguments

- sol:

  A `list` of `prioritizr` solution objects. Each element in the list
  must be an `sf` object containing a binary column named `solution_1`.

- name_sol:

  A character vector providing descriptive names for each solution in
  the `sol` list. The length of this vector must match the length of
  `sol`. These names will be used as row and column names in the output
  correlation matrix.

## Value

A numeric `matrix` (`matrixOut`) representing the Cohen's Kappa
correlation matrix between all pairs of solutions. Rows and columns are
named according to `name_sol`.

## Details

This function is essential for assessing the similarity or divergence
among different conservation plans. It takes a list of `prioritizr`
solution objects, each expected to contain a binary column named
`solution_1` (indicating selected or unselected planning units).

For every unique pair of solutions in the input list, it computes
Cohen's Kappa using the
[`irr::kappa2()`](https://rdrr.io/pkg/irr/man/kappa2.html) function.
Cohen's Kappa measures the agreement between two raters (in this case,
two conservation solutions) for categorical items, correcting for chance
agreement. A Kappa value of 1 indicates perfect agreement, 0 indicates
agreement equivalent to chance, and negative values indicate agreement
worse than chance.

The resulting matrix is symmetrical, with diagonal elements always equal
to 1 (a solution perfectly agrees with itself). This matrix can then be
passed to visualization functions like
[`splnr_plot_corrMat()`](https://mathmarecol.github.io/spatialplanr/reference/splnr_plot_corrMat.md)
to create a correlation heatmap.

## Examples

``` r
if (FALSE) { # \dontrun{
# Assuming 'dat_species_bin' is an existing sf object in your package.

# Create a dummy prioritizr problem and solve it for solution 1 (30% target).
dat_problem1 <- prioritizr::problem(
  dat_species_bin %>% dplyr::mutate(Cost = runif(n = dim(.)[[1]])),
  features = c("Spp1", "Spp2", "Spp3", "Spp4", "Spp5"),
  cost_column = "Cost"
) %>%
  prioritizr::add_min_set_objective() %>%
  prioritizr::add_relative_targets(0.3) %>%
  prioritizr::add_binary_decisions() %>%
  prioritizr::add_default_solver(verbose = FALSE)

dat_soln1 <- dat_problem1 %>%
  prioritizr::solve.ConservationProblem()

# Create another dummy prioritizr problem and solve it for solution 2 (50% target).
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

# Calculate the Cohen's Kappa correlation matrix between the two solutions.
corrMat <- splnr_get_kappaCorrData(
  sol = list(dat_soln1, dat_soln2),
  name_sol = c("Solution_A_30pct", "Solution_B_50pct")
)
print(corrMat)

# This output can then be directly passed to splnr_plot_corrMat().
# splnr_plot_corrMat(corrMat, AxisLabels = c("Sol A (30%)", "Sol B (50%)"))
} # }
```
