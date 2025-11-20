# Add-ons for Plotting `spatialplanr` Solution Maps

This function allows users to customize existing `ggplot2` solution maps
produced by `spatialplanr` spatial plotting functions (e.g.,
[`splnr_plot_solution()`](https://mathmarecol.github.io/spatialplanr/reference/splnr_plot_solution.md)).
It provides options to add various spatial layers and apply consistent
theming in a simple and reproducible manner.

## Usage

``` r
splnr_gg_add(
  PUs = NULL,
  colorPUs = "grey80",
  Bndry = NULL,
  colorBndry = "black",
  overlay = NULL,
  colorOverlay = "grey20",
  overlay2 = NULL,
  colorOverlay2 = "grey30",
  overlay3 = NULL,
  colorOverlay3 = "grey40",
  contours = NULL,
  colorConts = "black",
  cropOverlay = NULL,
  lockIn = NULL,
  typeLockIn = "Full",
  nameLockIn = NULL,
  alphaLockIn = 1,
  colorLockIn = "black",
  legendLockIn = "",
  labelLockIn = "MPAs",
  lockOut = NULL,
  typeLockOut = "Full",
  nameLockOut = NULL,
  alphaLockOut = 1,
  colorLockOut = "black",
  legendLockOut = "",
  labelLockOut = "",
  ggtheme = "Default"
)
```

## Arguments

- PUs:

  An `sf` object representing Planning Units. If provided, their
  outlines will be drawn. Defaults to `NULL`.

- colorPUs:

  A character string specifying the color for the outlines of the
  Planning Units. Defaults to `"grey80"`.

- Bndry:

  An `sf` object representing the main planning region boundaries. If
  provided, its outline will be drawn. Defaults to `NULL`.

- colorBndry:

  A character string specifying the color for the outline of the `Bndry`
  object. Defaults to `"black"`.

- overlay:

  An `sf` object to be plotted as a general overlay. Defaults to `NULL`.

- colorOverlay:

  A character string specifying the color for `overlay`. Defaults to
  `"grey20"`.

- overlay2:

  An `sf` object for a second general overlay. Defaults to `NULL`.

- colorOverlay2:

  A character string specifying the color for `overlay2`. Defaults to
  `"grey30"`.

- overlay3:

  An `sf` object for a third general overlay. Defaults to `NULL`.

- colorOverlay3:

  A character string specifying the color for `overlay3`. Defaults to
  `"grey40"`.

- contours:

  An `sf` object containing contour lines (e.g., bathymetry or seamount
  outlines). It is expected to have a `Category` column for
  differentiating lines. Up to 6 categories are supported. Defaults to
  `NULL`.

- colorConts:

  A character string specifying the color for the contour lines.
  Defaults to `"black"`.

- cropOverlay:

  An `sf` object. Its bounding box will be used to set the `xlim` and
  `ylim` of the
  [`ggplot2::coord_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html)
  layer, effectively cropping the view. Defaults to `NULL`.

- lockIn:

  An `sf` object representing locked-in areas (e.g., existing Marine
  Protected Areas (MPAs)) that are fixed in a conservation
  prioritization. Defaults to `NULL`.

- typeLockIn:

  A character string specifying how `lockIn` areas should be plotted.
  Can be `"Full"` (fills the areas with `colorLockIn`) or `"Contours"`
  (draws only the outlines of the areas). Defaults to `"Full"`.

- nameLockIn:

  A character string specifying the column name in the `lockIn` data
  frame that contains binary (0/1 or TRUE/FALSE) information indicating
  locked-in status. Required if `lockIn` is not `NULL`.

- alphaLockIn:

  A numeric value (0 to 1) for the opacity of the `lockIn` areas when
  `typeLockIn` is `"Full"`. Defaults to `1`.

- colorLockIn:

  A character string specifying the color for the `lockIn` areas.
  Defaults to `"black"`.

- legendLockIn:

  A character string for the title of the `lockIn` legend. Can be an
  empty string `""` to suppress the title. Defaults to `""`.

- labelLockIn:

  A character string for the legend label of the `lockIn` areas (e.g.,
  "MPAs"). Defaults to `"MPAs"`.

- lockOut:

  An `sf` object representing locked-out areas (e.g., shipping lanes,
  oil and gas leases, or other excluded zones) that must not be selected
  in a conservation prioritization. Defaults to `NULL`.

- typeLockOut:

  A character string specifying how `lockOut` areas should be plotted.
  Can be `"Full"` (fills the areas with `colorLockOut`) or `"Contours"`
  (draws only the outlines of the areas). Defaults to `"Full"`.

- nameLockOut:

  A character string specifying the column name in the `lockOut` data
  frame that contains binary (0/1 or TRUE/FALSE) information indicating
  locked-out status. Required if `lockOut` is not `NULL`.

- alphaLockOut:

  A numeric value (0 to 1) for the opacity of the `lockOut` areas when
  `typeLockOut` is `"Full"`. Defaults to `1`.

- colorLockOut:

  A character string specifying the color for the `lockOut` areas.
  Defaults to `"black"`.

- legendLockOut:

  A character string for the title of the `lockOut` legend. Can be an
  empty string `""` to suppress the title. Defaults to `""`.

- labelLockOut:

  A character string for the legend label of the `lockOut` areas (e.g.,
  "Shipping Lanes"). Defaults to `""`.

- ggtheme:

  The `ggplot2` theme to apply. Can be:

  - `NA` or `FALSE`: No theme is applied, using `ggplot2` defaults.

  - `"Default"`: Applies a `spatialplanr` default theme (`theme_bw()`
    with custom text/axis settings).

  - A `list` of
    [`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html)
    properties for custom styling.

  Defaults to `"Default"`.

## Value

A `list` of `ggplot2` layers and theme elements that can be added to an
existing `ggplot` object using `+`.

## Details

The `splnr_gg_add()` function enhances `ggplot2` objects by layering
additional spatial data such as Planning Unit outlines, study area
boundaries, general overlays, geographical contours, locked-in areas
(e.g., existing Marine Protected Areas (MPAs) that must be included in a
conservation prioritization), and locked-out areas (e.g., areas that
must be excluded from selection such as shipping lanes or oil and gas
leases). It offers fine-grained control over colors, opacities, and
legend appearance for each added layer.

When using `contours`, the input `sf` object is expected to have a
column named `Category` that defines the different contour lines to be
plotted. The function currently supports up to 6 distinct contour
categories for plotting.

The `ggtheme` parameter offers flexibility in plot styling. `"Default"`
applies a standard `spatialplanr` theme (`theme_bw()` with custom text
and axis settings). A `list` of
[`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html)
elements can be provided for full customization, or `NA` (logical
`FALSE`) to apply no default theme, allowing the user to manage all
theme elements manually.

## Examples

``` r
if (FALSE) { # \dontrun{
# Assuming 'dat_species_bin' and 'dat_PUs' are existing sf objects
# in your package, suitable for prioritization problems and plotting.

# Create a dummy prioritizr problem and solve it for demonstration.
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

# Basic plot of the solution with default Planning Unit outlines and theme.
plot_basic <- splnr_plot_solution(dat_soln) +
  splnr_gg_add(PUs = dat_PUs, ggtheme = "Default")
print(plot_basic)

# Example with boundary, a custom overlay, and locked-in areas shown as contours.
# For this example, let's create dummy `bndry_sf` and `locked_in_sf` based on `dat_PUs`.
# In a real scenario, these would be loaded from your package or data.
bndry_sf <- sf::st_union(dat_PUs) %>% sf::st_as_sf()
locked_in_sf <- dat_PUs[1:100, ] %>% dplyr::mutate(is_mpa = 1)

plot_custom <- splnr_plot_solution(dat_soln) +
  splnr_gg_add(
    PUs = dat_PUs,
    Bndry = bndry_sf,
    colorBndry = "darkblue",
    overlay = bndry_sf, # Using boundary as an example overlay
    colorOverlay = "lightblue",
    lockIn = locked_in_sf,
    typeLockIn = "Contours",
    nameLockIn = "is_mpa",
    colorLockIn = "darkred",
    labelLockIn = "Existing MPAs",
    ggtheme = "Default"
  )
print(plot_custom)

# Example with custom ggplot2 theme settings (as a list)
custom_theme_list <- list(
  ggplot2::theme_classic(),
  ggplot2::theme(
    plot.background = ggplot2::element_rect(fill = "lightyellow"),
    legend.position = "top"
  )
)
plot_with_custom_theme <- splnr_plot_solution(dat_soln) +
  splnr_gg_add(PUs = dat_PUs, ggtheme = custom_theme_list)
print(plot_with_custom_theme)
} # }
```
