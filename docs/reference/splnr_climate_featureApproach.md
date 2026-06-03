# Run the Feature Climate-Smart Approach

`splnr_climate_featureApproach()` implements the Feature Approach to
climate-smart conservation planning by defining a global climate-smart
layer and adjusting targets to ensure a specified proportion of that
layer is captured.

## Usage

``` r
splnr_climate_featureApproach(
  features,
  metric,
  targets,
  direction,
  percentile = 35,
  refugiaTarget = 0.3
)
```

## Arguments

- features:

  An `sf` object of conservation features.

- metric:

  An `sf` object with a column named `metric`.

- targets:

  A `data.frame` with columns `feature` and `target`.

- direction:

  `1` or `-1`.

- percentile:

  Numeric (0-100). Defaults to `35`.

- refugiaTarget:

  Numeric (0-1). Defaults to `0.3`.

## Value

A `list` with:

- `Features`: `sf` object with original features plus `climate_layer`.

- `Targets`: `data.frame` with adjusted targets including
  `climate_layer`.

## Details

This function orchestrates two steps:

1.  **Preprocessing** via `splnr_climate_feature_preprocess()`.

2.  **Target Assignment** via `splnr_climate_feature_assignTargets()`.

## Examples

``` r
if (FALSE) { # \dontrun{
initial_targets <- dat_species_bin %>%
  sf::st_drop_geometry() %>%
  colnames() %>%
  data.frame() %>%
  setNames(c("feature")) %>%
  dplyr::mutate(target = 0.3)

Feature_result <- splnr_climate_featureApproach(
  features      = dat_species_bin,
  metric        = dat_clim,
  targets       = initial_targets,
  direction     = 1,
  percentile    = 35,
  refugiaTarget = 0.3
)
out_sf_feature  <- Feature_result$Features
targets_feature <- Feature_result$Targets
} # }
```
