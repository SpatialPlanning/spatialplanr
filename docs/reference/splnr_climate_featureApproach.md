# Run the Feature Climate-Smart Approach

`splnr_climate_featureApproach()` implements the Feature Approach to
climate-smart conservation planning. This involves defining a global
"climate-smart" layer and adjusting conservation targets to ensure that
a specified proportion of this layer is captured in the solution.

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
  applied globally to the `metric` data. Defaults to `35`.

- refugiaTarget:

  A numeric value (0-1) representing the target proportion assigned to
  the overall climate-smart layer. Defaults to `0.3` (30%).

## Value

A `list` with two components:

- `Features`: An `sf` object containing the binary information per
  Planning Unit for each original feature, plus the new `climate_layer`
  feature. This is ready to be passed to `prioritizr`.

- `Targets`: A `data.frame` with the adjusted targets for all features,
  including the `climate_layer`. This is also ready for `prioritizr`.

## Details

This function orchestrates the steps for the Feature Approach:

1.  **Preprocessing:** It calls `splnr_climate_feature_preprocess()` to
    identify a region-wide climate-smart layer based on a percentile
    cutoff of the climate metric. This layer is then added as a new
    binary feature to your conservation data.

2.  **Target Assignment:** It then calls
    `splnr_climate_feature_assignTargets()` to calculate and assign new
    targets. Crucially, a specific `refugiaTarget` is set for the newly
    created `climate_layer` feature, ensuring that a certain proportion
    of the most climate-resilient areas are included in the final
    conservation plan.

The output is a list containing the modified features (now including the
`climate_layer`) and their corresponding adjusted targets, ready to be
used in a `prioritizr` conservation problem.

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

# Run the Feature Approach where higher climate metric values mean
# more climate-smart areas.
Feature_Approach_result <- splnr_climate_featureApproach(
  features = dat_species_bin,
  metric = dat_clim,
  targets = initial_targets,
  direction = 1, # Example: higher metric values are more climate-smart
  percentile = 35,
  refugiaTarget = 0.3
)

# Access the processed features and targets:
out_sf_feature <- Feature_Approach_result$Features
targets_feature <- Feature_Approach_result$Targets

print(head(out_sf_feature))
print(head(targets_feature))
} # }
```
