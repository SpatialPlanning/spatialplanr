# Extract Feature Names from Spatial Data

`splnr_featureNames()` extracts the names of conservation features from
an `sf` dataframe, excluding geometry and any specified columns.

## Usage

``` r
splnr_featureNames(dat, exclude = NA)
```

## Arguments

- dat:

  An `sf` dataframe representing conservation features. Each
  non-geometry column is assumed to be a feature.

- exclude:

  A character vector of column names (or prefixes) to exclude from the
  output. By default, it excludes columns starting with "Cost\_". If you
  provide a value, it will be *appended* to the default exclusion. Set
  to `NULL` or `character(0)` if you want no exclusions beyond the
  default.

## Value

A character vector containing the names of the conservation features.

## Details

This function is a utility for preparing data for `prioritizr` or other
conservation planning packages that require a vector of feature names.
It typically removes the geometry column and any columns related to cost
(prefixed with "Cost\_") by default, allowing you to specify additional
columns to exclude.

The output is a simple character vector of column names, which can be
directly used as feature identifiers in conservation problems.

## Examples

``` r
if (FALSE) { # \dontrun{
# Assuming 'dat_species_prob' is an existing sf object in your package.
# It likely has columns like 'Spp1', 'Spp2', 'Cost_SomeMeasure', etc.

# Example 1: Get all feature names, excluding default 'Cost_' columns.
feature_names_default <- splnr_featureNames(dat = dat_species_prob)
print(feature_names_default)

# Example 2: Get feature names, excluding 'Cost_' columns and 'Spp5'.
feature_names_custom_exclude <- splnr_featureNames(
  dat = dat_species_prob,
  exclude = "Spp5"
)
print(feature_names_custom_exclude)

# Example 3: If you only want to exclude a specific column and not 'Cost_'
# (you'd need to manually specify exclude = "geometry" and then your column)
# This case is more complex and usually handled by direct dplyr::select.
# This function's primary use is to remove cost columns and potentially others.
} # }
```
