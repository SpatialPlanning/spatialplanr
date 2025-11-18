# Run the Percentile Climate-Smart Approach

`splnr_climate_percentileApproach()` implements the Percentile Approach
to climate-smart conservation planning. This involves filtering features
to their most climate-resilient areas and adjusting their conservation
targets to account for this reduced feature distribution.

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

  An `sf` object representing conservation features (e.g., species
  distribution data).

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
  determining whether an area is a climate priority area or not. This is
  applied *per feature* to its distribution. Defaults to `35`.

## Value

A `list` with two components:

- `Features`: An `sf` object containing the binary information per
  Planning Unit for each feature, now filtered to include only its
  climate-smart occurrences. This is ready to be passed to `prioritizr`.

- `Targets`: A `data.frame` with the adjusted targets for the filtered
  features. This is also ready for `prioritizr`.

## Details

This function orchestrates the steps for the Percentile Approach:

1.  **Preprocessing:** It calls `splnr_climate_percentile_preprocess()`
    to identify, for each feature, its occurrences within the most
    climate-resilient `percentile` of its distribution based on a
    climate metric. This effectively "filters" the feature data to only
    include its climate-smart components.

2.  **Target Assignment:** It then calls
    `splnr_climate_percentile_assignTargets()` to calculate and assign
    new targets for these filtered features. The targets are scaled up
    to ensure that the original conservation goals are still met, but
    specifically by selecting areas from the climate-smart portions of
    the features' distributions.

The output is a list containing the modified features (filtered to their
climate-smart occurrences) and their corresponding adjusted targets,
ready to be used in a `prioritizr` conservation problem.

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

# Run the Percentile Approach where higher climate metric values mean
# more climate-smart areas.
Percentile_Approach_result <- splnr_climate_percentileApproach(
  features = dat_species_bin,
  metric = dat_clim,
  targets = initial_targets,
  direction = 1, # Example: higher metric values are more climate-smart
  percentile = 35
)

# Access the processed features and targets:
out_sf_percentile <- Percentile_Approach_result$Features
targets_percentile <- Percentile_Approach_result$Targets

print(head(out_sf_percentile))
print(head(targets_percentile))
} # }
```
