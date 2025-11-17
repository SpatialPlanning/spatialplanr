# Plot Spatial Data

This function provides a versatile way to plot spatial data (`sf`
objects) within the `spatialplanr` package. It can visualize various
data types, including binary presence/absence, logical values,
continuous data, or simply the planning unit outlines.

This function provides a versatile way to plot spatial data (`sf`
objects) within the `spatialplanr` package. It can visualize various
data types, including binary presence/absence, logical values,
continuous data, or simply the Planning Unit outlines.

## Usage

``` r
splnr_plot(
  df,
  colNames = NULL,
  paletteName = "YlGnBu",
  colourVals = c("#c6dbef", "#3182bd"),
  plotTitle = "",
  legendTitle = NULL,
  legendLabels = NULL
)

splnr_plot(
  df,
  colNames = NULL,
  paletteName = "YlGnBu",
  colourVals = c("#c6dbef", "#3182bd"),
  plotTitle = "",
  legendTitle = NULL,
  legendLabels = NULL
)
```

## Arguments

- df:

  The input dataframe containing the data to be plotted. This must be an
  `sf` object and include a geometry column.

- colNames:

  A character vector of column names from `df` to be used for coloring
  the plot. If `NULL` (default), only the Planning Unit outlines are
  plotted. If a single column is specified, it checks for binary,
  logical, or continuous data. If multiple columns are specified, it
  sums the values across these columns to create a "FeatureSum" for
  plotting.

- paletteName:

  A character string specifying the name of the `RColorBrewer` palette
  to use for filling continuous data. Defaults to `"YlGnBu"`.

- colourVals:

  A character vector of two color values to use for binary (0/1) or
  logical (FALSE/TRUE) data. The first color is for '0' or 'FALSE'
  (absence), and the second is for '1' or 'TRUE' (presence). Defaults to
  `c("#c6dbef", "#3182bd")`.

- plotTitle:

  A character string for the subtitle of the plot. Defaults to `""` (no
  subtitle).

- legendTitle:

  A character string for the title of the legend. If `NULL`, a default
  title will be used based on the data type.

- legendLabels:

  A character vector of strings to use for the legend labels,
  particularly useful for binary or logical data (e.g.,
  `c("Absent", "Present")`). If `NULL`, default labels are used for
  binary/logical plots.

## Value

A `ggplot` object representing the spatial plot.

A `ggplot` object representing the spatial plot.

## Details

The `splnr_plot` function automatically detects the type of data
specified by `colNames` (binary, logical, or continuous) and adjusts the
plotting aesthetics accordingly. If multiple `colNames` are provided, it
calculates the sum of features for each planning unit and plots this
sum. If `colNames` is `NULL`, it will simply plot the outlines of the
planning units.

This function is designed to be a flexible replacement for several
plotting functions, such as
[`splnr_plot_cost()`](https://mathmarecol.github.io/spatialplanr/reference/splnr_plot_cost.md),
[`splnr_plot_binFeature()`](https://mathmarecol.github.io/spatialplanr/reference/splnr_plot_binFeature.md),
[`splnr_plot_MPAs()`](https://mathmarecol.github.io/spatialplanr/reference/splnr_plot_MPAs.md),
and
[`splnr_plot_featureNo()`](https://mathmarecol.github.io/spatialplanr/reference/splnr_plot_featureNo.md),
streamlining the plotting workflow within the package.

Written by Kilian Barreiro and Jason Everett. Last modified: February
2024.

The `splnr_plot()` function automatically detects the type of data
specified by `colNames` (binary, logical, or continuous) and adjusts the
plotting aesthetics accordingly. If multiple `colNames` are provided, it
calculates the sum of features for each Planning Unit and plots this
sum. If `colNames` is `NULL`, it will simply plot the outlines of the
Planning Units.

This function is designed to be a flexible replacement for several
plotting functions, such as
[`splnr_plot_cost()`](https://mathmarecol.github.io/spatialplanr/reference/splnr_plot_cost.md),
[`splnr_plot_binFeature()`](https://mathmarecol.github.io/spatialplanr/reference/splnr_plot_binFeature.md),
[`splnr_plot_MPAs()`](https://mathmarecol.github.io/spatialplanr/reference/splnr_plot_MPAs.md),
and
[`splnr_plot_featureNo()`](https://mathmarecol.github.io/spatialplanr/reference/splnr_plot_featureNo.md),
streamlining the plotting workflow within the package.

Written by Kilian Barreiro and Jason Everett. Last modified: February
2024.

## Examples

``` r
if (FALSE) { # \dontrun{
# Assuming 'dat_species_bin', 'dat_bathy', and 'dat_PUs' are existing sf objects
# in your package, suitable for plotting.

# Binary plot of species distribution for "Spp1"
plot_spp1_binary <- splnr_plot(
  df = dat_species_bin,
  colNames = "Spp1",
  legendTitle = "Species Presence",
  legendLabels = c("Absent", "Present")
)
print(plot_spp1_binary)

# Logical plot of species distribution for "Spp1" (converted from binary)
plot_spp1_logical <- splnr_plot(
  df = dat_species_bin %>%
    dplyr::mutate(dplyr::across(
      tidyselect::starts_with("Spp"), as.logical
    )),
  colNames = "Spp1",
  legendTitle = "Species Presence",
  legendLabels = c("Absent", "Present")
)
print(plot_spp1_logical)

# Continuous plot of bathymetry
plot_bathymetry <- splnr_plot(
  df = dat_bathy,
  colNames = "bathymetry",
  plotTitle = "Bathymetry",
  legendTitle = "Bathymetry (m)"
)
print(plot_bathymetry)

# Plot Planning Units outlines only
plot_planning_units <- splnr_plot(df = dat_PUs)
print(plot_planning_units)

# Multi-binary features: Plotting the sum of multiple "Spp" features
plot_multi_spp_sum <- splnr_plot(
  df = dat_species_bin,
  colNames = colnames(dat_species_bin %>%
    sf::st_drop_geometry() %>%
    dplyr::select(tidyselect::starts_with("Spp"))),
  legendTitle = "Number of Features"
)
print(plot_multi_spp_sum)
} # }
if (FALSE) { # \dontrun{
# Assuming 'dat_species_bin', 'dat_bathy', and 'dat_PUs' are existing sf objects
# in your package, suitable for plotting.

# Binary plot of species distribution for "Spp1"
plot_spp1_binary <- splnr_plot(
  df = dat_species_bin,
  colNames = "Spp1",
  legendTitle = "Species Presence",
  legendLabels = c("Absent", "Present")
)
print(plot_spp1_binary)

# Logical plot of species distribution for "Spp1" (converted from binary)
plot_spp1_logical <- splnr_plot(
  df = dat_species_bin %>%
    dplyr::mutate(dplyr::across(
      tidyselect::starts_with("Spp"), as.logical
    )),
  colNames = "Spp1",
  legendTitle = "Species Presence",
  legendLabels = c("Absent", "Present")
)
print(plot_spp1_logical)

# Continuous plot of bathymetry
plot_bathymetry <- splnr_plot(
  df = dat_bathy,
  colNames = "bathymetry",
  plotTitle = "Bathymetry",
  legendTitle = "Bathymetry (m)"
)
print(plot_bathymetry)

# Plot Planning Units outlines only
plot_planning_units <- splnr_plot(df = dat_PUs)
print(plot_planning_units)

# Multi-binary features: Plotting the sum of multiple "Spp" features
plot_multi_spp_sum <- splnr_plot(
  df = dat_species_bin,
  colNames = colnames(dat_species_bin %>%
    sf::st_drop_geometry() %>%
    dplyr::select(tidyselect::starts_with("Spp"))),
  legendTitle = "Number of Features"
)
print(plot_multi_spp_sum)
} # }
```
