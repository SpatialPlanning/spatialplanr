# Plot Circular Barplot for Feature Representation

`splnr_plot_circBplot()` creates a circular bar plot to visualize
feature representation, categorized by groups. It's particularly useful
for displaying how different categories of features meet certain targets
in a radial layout.

## Usage

``` r
splnr_plot_circBplot(
  df,
  legend_color,
  legend_list,
  indicateTargets = TRUE,
  impTarget = NA,
  repTarget = NA,
  colTarget = "red"
)
```

## Arguments

- df:

  A [data.frame](https://rdrr.io/r/base/data.frame.html) or
  [tibble](https://tibble.tidyverse.org/reference/tibble.html) that
  **must** contain the following columns:

  - `feature`: [character](https://rdrr.io/r/base/character.html) or
    [factor](https://rdrr.io/r/base/factor.html) unique identifier for
    each individual bar (e.g., species names).

  - `value`: [numeric](https://rdrr.io/r/base/numeric.html) the value to
    be plotted on the y-axis (bar height, typically percentage
    representation).

  - `group`: [character](https://rdrr.io/r/base/character.html) or
    [factor](https://rdrr.io/r/base/factor.html) for grouping factors
    (e.g., "important", "representative").

- legend_color:

  A [named vector](https://rdrr.io/r/base/vector.html) of colors. Names
  must correspond to the unique values in the `group` column of `df`,
  and values are the corresponding colors. For example:
  `c("group_name1" = "red", "group_name2" = "blue")`.

- legend_list:

  A [character vector](https://rdrr.io/r/base/character.html) of labels
  for the legend. This should match the names used in `legend_color` or
  the levels of `group`.

- indicateTargets:

  A [logical](https://rdrr.io/r/base/logical.html) value. If `TRUE`,
  horizontal lines indicating `impTarget` and `repTarget` will be drawn
  on the plot.

- impTarget:

  A [numeric](https://rdrr.io/r/base/numeric.html) value representing
  the target percentage for 'important' features. Required if
  `indicateTargets` is `TRUE`.

- repTarget:

  A [numeric](https://rdrr.io/r/base/numeric.html) value representing
  the target percentage for 'representative' features. Required if
  `indicateTargets` is `TRUE`.

- colTarget:

  A [character](https://rdrr.io/r/base/character.html) string specifying
  the color for the target indicator lines.

## Value

A [ggplot2::ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html)
object of the circular bar plot.

## Examples

``` r
# DISCLAIMER: THIS SOLUTION IS NOT ACTUALLY RUN WITH THESE TARGETS YET

if (FALSE) { # \dontrun{

dat_problem <- prioritizr::problem(dat_species_bin %>% dplyr::mutate(Cost = runif(n = dim(.)[[1]])),
  features = c("Spp1", "Spp2", "Spp3", "Spp4", "Spp5"),
  cost_column = "Cost"
) %>%
  prioritizr::add_min_set_objective() %>%
  prioritizr::add_relative_targets(0.3) %>%
  prioritizr::add_binary_decisions() %>%
  prioritizr::add_default_solver(verbose = FALSE)

dat_soln <- dat_problem %>%
  prioritizr::solve.ConservationProblem()

s1 <- dat_soln %>%
  tibble::as_tibble()

p1 <- dat_problem

# Assuming eval_feature_representation_summary is from prioritizr
df_rep_imp <- prioritizr::eval_feature_representation_summary(
  p1,
  s1[, "solution_1"]
) %>%
  dplyr::select(feature, relative_held) %>%
  dplyr::mutate(relative_held = relative_held * 100)

imp_layers <- c("Spp1", "Spp3")

target <- data.frame(feature = c("Spp1", "Spp2", "Spp3", "Spp4", "Spp5")) %>%
  dplyr::mutate(class = dplyr::if_else(.data$feature %in% imp_layers,
    "important", "representative"
  )) %>%
  dplyr::mutate(target = dplyr::if_else(class == "important",
    50 / 100, 30 / 100
  ))

df <- merge(df_rep_imp, target) %>%
  dplyr::select(-target) %>%
  stats::na.omit() %>% # Use stats::na.omit
  dplyr::rename(value = relative_held) %>%
  dplyr::rename(group = class)

colors <- c(
  "important" = "darkgreen",
  "representative" = "darkred"
)
legends <- c("Important", "Representative")

(splnr_plot_circBplot(df,
  legend_list = legends,
  legend_color = colors,
  impTarget = 50, repTarget = 30
))
} # }
```
