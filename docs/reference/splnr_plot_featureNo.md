# Plot number of features

`splnr_plot_featureNo()` allows you to use `ggplot2` to visually inspect
the number of features per planning unit that are used as inputs in the
conservation problem. When all features are species, this map can be
seen as a visualisation of species richness in the planning region.

## Usage

``` r
splnr_plot_featureNo(
  df,
  showLegend = TRUE,
  paletteName = "YlGnBu",
  plotTitle = "Number of Features",
  legendTitle = "Features"
)
```

## Arguments

- df:

  An `sf` object of features

- showLegend:

  A logical command on whether to show the legend of the solution
  (Default: TRUE).

- paletteName:

  A string (or number) for the color palette to use. Available palettes
  can be found at
  https://ggplot2.tidyverse.org/reference/scale_brewer.html.

- plotTitle:

  A character value for the title of the plot. Can be empty ("").

- legendTitle:

  A character value for the title of the legend. Can be empty ("").

## Value

A ggplot object of the plot

## Details

**\[deprecated\]**

This function requires an `sf` object with binary information of all
features you want to include in the richness plot (`0` for absences and
`1` for presences, for example created from continuous data with the
`spatialplanr` function
[`splnr_apply_cutoffs()`](https://mathmarecol.github.io/spatialplanr/reference/splnr_apply_cutoffs.md)).
It outputs a `ggobject` and can be combined with the `spatialplanr`
function
[`splnr_gg_add()`](https://mathmarecol.github.io/spatialplanr/reference/splnr_gg_add.md).

## Examples

``` r
if (FALSE) { # \dontrun{
(splnr_plot_featureNo(dat_species_bin))
} # }
```
