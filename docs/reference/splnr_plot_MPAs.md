# Plot MPAs

`splnr_plot_MPAs()` allows to plot either the outline or the area of
MPAs existing in the planning region (for example extracted with the
`spatialplanr`function
[`splnr_get_MPAs()`](https://mathmarecol.github.io/spatialplanr/reference/splnr_get_MPAs.md))
in a customisable way using `ggplot2`. This function requires an `sf`
object containing the information whether a planning unit in the
planning region lies within an MPA or not in a column called `wdpa` and
outputs a `ggobject`. It can be combined with the `spatialplanr`
function
[`splnr_gg_add()`](https://mathmarecol.github.io/spatialplanr/reference/splnr_gg_add.md).

## Usage

``` r
splnr_plot_MPAs(
  df,
  colorVals = c(`TRUE` = "blue", `FALSE` = "white"),
  showLegend = TRUE,
  plotTitle = "Locked In Areas",
  legendTitle = ""
)
```

## Arguments

- df:

  An `sf` object of marine protected areas

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
splnr_plot_MPAs(dat_mpas)
} # }
```
