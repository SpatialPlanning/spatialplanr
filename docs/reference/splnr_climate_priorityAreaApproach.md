# Run the Climate Priority Area (CPA) Approach

`splnr_climate_priorityAreaApproach()` implements the Climate Priority
Area approach by splitting conservation features into climate-smart (CS)
and non-climate-smart (NCS) components and adjusting their targets
accordingly. This allows conservation planning to prioritize areas with
higher climate resilience.

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

  An `sf` object representing conservation features (e.g., species
  distribution data). Each column (excluding geometry) should typically
  be a binary representation of a feature's presence (1) or absence (0)
  in each Planning Unit.

- metric:

  An `sf` object containing climate metric information. It must have a
  column named 'metric' with the climate metric values per Planning
  Unit.

- targets:

  A `data.frame` with two columns: `feature` (character, listing the
  original feature names) and `target` (numeric, the initial
  conservation target for each feature as a proportion, e.g., 0.3).

- direction:

  An integer specifying the direction of climate-smartness:

  - `1`: Higher metric values mean more climate-smart areas.

  - `-1`: Lower metric values mean more climate-smart areas.

- percentile:

  A numeric value (0-100) representing the cutoff threshold for
  determining climate-smart areas. For example, `percentile = 5` means
  the most climate-smart 5% of areas (based on `direction`) are
  considered. This value represents the lower limit of the threshold.
  Defaults to `5`.

- refugiaTarget:

  A numeric value (0-1) representing the target proportion assigned
  specifically to climate-smart areas (refugia). Defaults to `1` (100%).

## Value

A `list` with two components:

- `Features`: An `sf` object containing the binary information per
  Planning Unit for each feature, now split into `_CS` (climate-smart)
  and `_NCS` (non-climate-smart) components. This is ready to be passed
  to `prioritizr` when creating a conservation problem.

- `Targets`: A `data.frame` with the adjusted targets for the
  climate-split features. This is also ready for `prioritizr`.

## Details

This function orchestrates the steps required for the CPA approach:

1.  **Preprocessing:** It calls
    `splnr_climate_priorityArea_preprocess()` to categorize each
    feature's occurrences into CS and NCS areas based on a climate
    metric and a specified `percentile` cutoff.

2.  **Target Assignment:** It then calls
    `splnr_climate_priorityArea_assignTargets()` to calculate and assign
    new targets for these CS and NCS feature components. This ensures
    that conservation goals reflect the desired emphasis on
    climate-smart areas (e.g., aiming for 100% representation of
    features in highly resilient areas).

The output of this function is a list containing the modified features
(now split into CS/NCS components) and their corresponding adjusted
targets, ready to be used in a `prioritizr` conservation problem.

## Examples

``` r
if (FALSE) { # \dontrun{
# Assuming 'dat_species_bin' and 'dat_clim' are existing sf objects
# in your package.

# Define initial targets for species features.
initial_targets <- dat_species_bin %>%
  sf::st_drop_geometry() %>%
  colnames() %>%
  data.frame() %>%
  setNames(c("feature")) %>%
  dplyr::mutate(target = 0.3)

# Run the Climate Priority Area Approach where lower climate metric
# values mean more climate-smart areas.
CPA_Approach_result <- splnr_climate_priorityAreaApproach(
  features = dat_species_bin,
  metric = dat_clim,
  targets = initial_targets,
  direction = -1, # Example: lower metric values are more climate-smart
  percentile = 5,
  refugiaTarget = 1
)

# Access the processed features and targets:
out_sf_cpa <- CPA_Approach_result$Features
targets_cpa <- CPA_Approach_result$Targets

print(head(out_sf_cpa))
print(head(targets_cpa))
} # }
```
