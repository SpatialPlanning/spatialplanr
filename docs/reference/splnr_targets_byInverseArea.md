# Assign Targets by Inverse Area

This function calculates inverse area targets for each conservation
feature within an `sf` dataframe, based on their areal coverage. The
target is set to be inversely proportional to the feature's area,
ranging between a specified minimum (`target_min`) and maximum
(`target_max`).

## Usage

``` r
splnr_targets_byInverseArea(df, target_min, target_max)
```

## Arguments

- df:

  An `sf` dataframe containing the features (e.g., species distribution
  data) for which to calculate inverse area targets. Each column
  (excluding geometry) should represent a feature, and each row a
  Planning Unit.

- target_min:

  A numeric value between 0 and 1 (inclusive) specifying the minimum
  target percentage. This will be the target for the most widespread
  feature.

- target_max:

  A numeric value between 0 and 1 (inclusive) specifying the maximum
  target percentage. This will be the target for the rarest feature.

## Value

A `tibble` (data frame) with two columns: `Species` (or feature name)
and `target` (the calculated inverse area target for each feature).

## Details

The inverse area target approach aims to assign higher conservation
targets to features that have a smaller overall distribution or areal
coverage within the study region. This can be particularly useful for
prioritizing rare or range-restricted features.

The calculation proceeds as follows:

1.  The area of a single Planning Unit is determined.

2.  The total area of the study region is estimated by multiplying the
    number of Planning Units by the individual Planning Unit area.

3.  For each feature (species), its total area across all Planning Units
    is calculated.

4.  The target for each feature is then scaled between `target_min` and
    `target_max` such that features with smaller areas receive targets
    closer to `target_max`, and features with larger areas receive
    targets closer to `target_min`.

The input `df` is expected to be an `sf` object where columns (excluding
geometry) represent different features (e.g., species presence/absence)
and rows represent Planning Units.

## Examples

``` r
if (FALSE) { # \dontrun{
# Assuming 'dat_species_prob' is an existing sf object in your package,
# representing species distribution in planning units.

# Calculate inverse area targets with a range from 30% to 80%.
targets_inverse_area <- dat_species_prob %>%
  splnr_targets_byInverseArea(target_min = 0.3, target_max = 0.8)
print(targets_inverse_area)

# Example with a different target range (e.g., 20% to 70%)
targets_custom_range <- dat_species_prob %>%
  splnr_targets_byInverseArea(target_min = 0.2, target_max = 0.7)
print(targets_custom_range)
} # }
```
