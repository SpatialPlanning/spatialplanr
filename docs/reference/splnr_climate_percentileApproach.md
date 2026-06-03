# Run the Percentile Climate-Smart Approach

`splnr_climate_percentileApproach()` implements the Percentile Approach
to climate-smart conservation planning by filtering features to their
most climate-resilient areas and adjusting targets accordingly.

## Usage

``` r
splnr_climate_percentileApproach(
  features,
  metric,
  targets,
  direction,
  percentile = 35
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

## Value

A `list` with:

- `Features`: `sf` object filtered to climate-smart occurrences.

- `Targets`: `data.frame` with adjusted targets.

## Details

This function orchestrates two steps:

1.  **Preprocessing** via `splnr_climate_percentile_preprocess()`.

2.  **Target Assignment** via
    `splnr_climate_percentile_assignTargets()`.

## Examples

``` r
if (FALSE) { # \dontrun{
initial_targets <- dat_species_bin %>%
  sf::st_drop_geometry() %>%
  colnames() %>%
  data.frame() %>%
  setNames(c("feature")) %>%
  dplyr::mutate(target = 0.3)

Percentile_result <- splnr_climate_percentileApproach(
  features   = dat_species_bin,
  metric     = dat_clim,
  targets    = initial_targets,
  direction  = 1,
  percentile = 35
)
out_sf_percentile  <- Percentile_result$Features
targets_percentile <- Percentile_result$Targets
} # }
```
