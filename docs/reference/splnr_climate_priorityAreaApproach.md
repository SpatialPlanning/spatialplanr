# Run the Climate Priority Area (CPA) Approach

`splnr_climate_priorityAreaApproach()` implements the Climate Priority
Area approach by splitting conservation features into climate-smart (CS)
and non-climate-smart (NCS) components and adjusting their targets
accordingly.

## Usage

``` r
splnr_climate_priorityAreaApproach(
  features,
  metric,
  targets,
  direction,
  percentile = 5,
  refugiaTarget = 1
)
```

## Arguments

- features:

  An `sf` object of conservation features (binary presence/absence).

- metric:

  An `sf` object with a column named `metric`.

- targets:

  A `data.frame` with columns `feature` and `target`.

- direction:

  `1` (higher = more climate-smart) or `-1` (lower = more
  climate-smart).

- percentile:

  Numeric (0-100). Defaults to `5`.

- refugiaTarget:

  Numeric (0-1). Defaults to `1`.

## Value

A `list` with:

- `Features`: `sf` object with `_CS` and `_NCS` columns per feature.

- `Targets`: `data.frame` with adjusted targets.

## Details

This function orchestrates two steps:

1.  **Preprocessing** via `splnr_climate_priorityArea_preprocess()`.

2.  **Target Assignment** via
    `splnr_climate_priorityArea_assignTargets()`.

## Examples

``` r
if (FALSE) { # \dontrun{
initial_targets <- dat_species_bin %>%
  sf::st_drop_geometry() %>%
  colnames() %>%
  data.frame() %>%
  setNames(c("feature")) %>%
  dplyr::mutate(target = 0.3)

CPA_result <- splnr_climate_priorityAreaApproach(
  features      = dat_species_bin,
  metric        = dat_clim,
  targets       = initial_targets,
  direction     = -1,
  percentile    = 5,
  refugiaTarget = 1
)
out_sf_cpa    <- CPA_result$Features
targets_cpa   <- CPA_result$Targets
} # }
```
