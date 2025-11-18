# Plot binary feature

`splnr_plot_binFeature()` allows to plot presences and absences of a
feature in the planning region in a customisable way using `ggplot2`.
This function requires an `sf` object with binary information of a
feature(`0` for absences and `1` for presences, for example created from
continuous data with the `spatialplanr` function
[`splnr_apply_cutoffs()`](https://mathmarecol.github.io/spatialplanr/reference/splnr_apply_cutoffs.md)).
It outputs a `ggobject` and can be combined with the `spatialplanr`
function
[`splnr_gg_add()`](https://mathmarecol.github.io/spatialplanr/reference/splnr_gg_add.md).

## Usage

``` r
splnr_plot_binFeature(
  df,
  colInterest,
  colorVals = c(Suitable = "#3182bd", `Not Suitable` = "#c6dbef"),
  showLegend = TRUE,
  plotTitle = " ",
  legendTitle = "Habitat"
)
```

## Arguments

- df:

  A `data frame` with binary feature information

- colInterest:

  column of data frame that contains binary information of feature to
  plot

- colorVals:

  A `list` object of named vectors that will match the color value with
  the according name. "TRUE" stands for selected planning units.

- showLegend:

  A logical command on whether to show the legend of the solution
  (Default: TRUE).

- plotTitle:

  A character value for the title of the plot. Can be empty ("").

- legendTitle:

  A character value for the title of the legend. Can be empty ("").

## Value

A ggplot object of the plot

## Details

**\[deprecated\]**

## Examples

``` r
if (FALSE) { # \dontrun{
splnr_plot_binFeature(dat_species_bin, dat_species_bin$Spp1)
} # }
```
