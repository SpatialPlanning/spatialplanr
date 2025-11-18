# Plot Correlation Matrices of Conservation Solutions

The `splnr_plot_corrMat()` function visualizes a correlation matrix of
`prioritizr` conservation solutions, typically computed using Cohen's
Kappa. This helps in understanding the agreement or disagreement between
different spatial plans.

## Usage

``` r
splnr_plot_corrMat(
  x,
  colourGradient = c("#BB4444", "#FFFFFF", "#4477AA"),
  legendTitle = "Correlation \ncoefficient",
  AxisLabels = NULL,
  plotTitle = ""
)
```

## Arguments

- x:

  A numeric correlation matrix of `prioritizr` solutions.

- colourGradient:

  A character vector of three color values:

  - `colourGradient[1]`: Color for high positive correlation.

  - `colourGradient[2]`: Color for no correlation (midpoint).

  - `colourGradient[3]`: Color for high negative correlation.

  Defaults to `c("#BB4444", "#FFFFFF", "#4477AA")`.

- legendTitle:

  A character string for the title of the legend. Defaults to
  `"Correlation \ncoefficient"`.

- AxisLabels:

  A character vector of labels for the x and y axes of the correlation
  matrix, representing the names of the correlated solutions. If `NULL`
  (default), the column names of `x` will be used. The length of this
  vector must match the number of rows/columns in `x`.

- plotTitle:

  A character string for the title of the plot. Defaults to `""`.

## Value

A `ggplot` object representing the correlation matrix plot.

## Details

Conservation planning often involves comparing the outputs of various
conservation problems. One effective method for this is correlating
solutions using metrics like Cohen's Kappa. This function takes a
correlation matrix (e.g., produced by the `spatialplanr` function
[`splnr_get_kappaCorrData()`](https://mathmarecol.github.io/spatialplanr/reference/splnr_get_kappaCorrData.md))
and generates a heatmap visualization using `ggcorrplot`.

The plot highlights positive, negative, and no correlation using a color
gradient, and labels the correlation coefficients directly on the plot.
The output is a `ggplot` object that can be combined with the
`spatialplanr` function
[`splnr_gg_add()`](https://mathmarecol.github.io/spatialplanr/reference/splnr_gg_add.md)
for further customization, though its primary use is for standalone
correlation visualization.

## Examples

``` r
if (FALSE) { # \dontrun{
# Assuming 'dat_species_bin' is an existing sf object in your package.

# Create Problem 1 (30% target) and solve it.
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

# Create Problem 2 (50% target) and solve it.
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

# Get the Kappa correlation data for the two solutions.
CorrMat <- splnr_get_kappaCorrData(list(dat_soln, dat_soln2), name_sol = c("soln1", "soln2"))

# Plot the correlation matrix with custom axis labels.
plot_correlation_matrix <- splnr_plot_corrMat(
  CorrMat,
  AxisLabels = c("Solution 1", "Solution 2")
)
print(plot_correlation_matrix)
} # }
```
