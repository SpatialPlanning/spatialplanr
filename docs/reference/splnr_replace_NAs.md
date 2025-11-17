# Remove NAs from Spatial Data Using Nearest Neighbour

`splnr_replace_NAs()` replaces missing (NA) values in a specified column
of an `sf` dataframe with the value from the nearest spatial neighbor.

## Usage

``` r
splnr_replace_NAs(df, vari)
```

## Arguments

- df:

  An `sf` dataframe. This dataframe must contain a geometry column and
  the `vari` column with potential NA values.

- vari:

  A character string specifying the name of the column in `df` from
  which NA values are to be removed and replaced. This column must exist
  in `df`.

## Value

An `sf` object identical to the input `df`, but with NA values in the
`vari` column replaced by values from their nearest non-NA neighbors. If
no NAs are found, the original `df` is returned unchanged.

## Details

This function is useful for imputing missing data in spatial contexts.
It identifies all Planning Units with `NA` values in the `vari` column.
For each of these, it finds the geographically closest Planning Unit
that *does not* have an `NA` value in `vari`, and then copies that
non-missing value. This approach leverages the spatial autocorrelation
often present in environmental and species data.

The `st_nearest_feature()` function from the `sf` package is used for
determining the closest neighbor.

## Examples

``` r
if (FALSE) { # \dontrun{
# Assuming 'dat_species_prob' is an existing sf object in your package.
# For demonstration, let's artificially introduce some NAs in 'Spp2'.
df_with_na <- dat_species_prob %>%
  dplyr::mutate(Spp2 = ifelse(runif(n()) < 0.2, NA, Spp2))

# Replace NAs in 'Spp2' using nearest neighbor imputation.
df_no_na <- splnr_replace_NAs(df = df_with_na, vari = "Spp2")
print(sum(is.na(df_no_na$Spp2))) # Should be 0 if successful
} # }
```
