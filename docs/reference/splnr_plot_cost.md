# Plot cost

`splnr_plot_cost()` allows to plot cost within each planning units of a
planning region in a customisable way using `ggplot2`. This function
requires an `sf` object with a cost column and outputs a `ggobject`. It
can be combined with the `spatialplanr` function
[`splnr_gg_add()`](https://mathmarecol.github.io/spatialplanr/reference/splnr_gg_add.md).

## Usage

``` r
splnr_plot_cost(
  cost,
  costName = "Cost",
  legendTitle = "Cost",
  paletteName = "YlGnBu",
  plotTitle = ""
)
```

## Arguments

- cost:

  An `sf` object of cost for `prioritizr`

- costName:

  Name of the cost column

- legendTitle:

  A character value for the title of the legend. Can be empty ("").

- paletteName:

  A string (or number) for the color palette to use. Available palettes
  can be found at
  https://ggplot2.tidyverse.org/reference/scale_brewer.html.

- plotTitle:

  A character value for the title of the plot. Can be empty ("").

## Value

A ggplot object of the plot

## Details

**\[deprecated\]**

## Examples

``` r
if (FALSE) { # \dontrun{
dat_problem <- prioritizr::problem(dat_species_bin %>% dplyr::mutate(cost = runif(n = dim(.)[[1]])),
  features = c("Spp1", "Spp2", "Spp3", "Spp4", "Spp5"),
  cost_column = "Cost"
) %>%
  prioritizr::add_min_set_objective() %>%
  prioritizr::add_relative_targets(0.3) %>%
  prioritizr::add_binary_decisions() %>%
  prioritizr::add_default_solver(verbose = FALSE)

dat_soln <- dat_problem %>%
  prioritizr::solve.ConservationProblem()

dat_cost <- dat_soln %>%
  dplyr::mutate(Cost = runif(n = dim(.)[[1]]))

(splnr_plot_cost(dat_cost))
} # }
```
