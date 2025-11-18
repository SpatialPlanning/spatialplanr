# Plot Climate Metric Data

The `splnr_plot_climData()` function creates a spatial plot of climate
metric information from an `sf` object. It provides a customizable
visualization using `ggplot2` and `viridis` color palettes.

## Usage

``` r
splnr_plot_climData(
  df,
  colInterest,
  colorMap = "C",
  plotTitle = " ",
  legendTitle = "Climate metric"
)
```

## Arguments

- df:

  An `sf` object containing the climate metric information. It must have
  a geometry column.

- colInterest:

  A character string specifying the name of the column in `df` that
  contains the climate metric data to be plotted.

- colorMap:

  A character string indicating the `viridis` color map to use (e.g.,
  "A", "B", "C", "D", "E"). See
  <https://ggplot2.tidyverse.org/reference/scale_viridis.html> for all
  options. Defaults to `"C"`.

- plotTitle:

  A character string for the subtitle of the plot. Defaults to `" "` (a
  single space, effectively no subtitle).

- legendTitle:

  A character string for the title of the legend. Defaults to
  `"Climate metric"`.

## Value

A `ggplot` object representing the spatial plot of the climate metric.

## Details

This function is designed to visualize spatial data that contains a
specific climate metric. It expects an `sf` object (`df`) with a
geometry column and the climate metric data in a column specified by
`colInterest`. The plot uses a continuous color scale (viridis) to
represent the metric values across the planning units.

This function can be easily integrated into a larger plotting workflow
or used independently to inspect climate data distributions.

## Examples

``` r
if (FALSE) { # \dontrun{
# Assuming 'dat_clim' is an existing sf object in your package
# with a column named "metric" or another relevant climate metric.

# Example: Plot climate data using "metric" column
plot_climate_metric <- splnr_plot_climData(
  df = dat_clim,
  colInterest = "metric",
  plotTitle = "Annual Climate Warming",
  legendTitle = "Warming (°C/year)"
)
print(plot_climate_metric)

# Example with a different color map
plot_climate_alt_cmap <- splnr_plot_climData(
  df = dat_clim,
  colInterest = "metric",
  colorMap = "D", # Using 'D' for a different viridis palette
  plotTitle = "Climate Metric (Alternative Colors)"
)
print(plot_climate_alt_cmap)
} # }
```
