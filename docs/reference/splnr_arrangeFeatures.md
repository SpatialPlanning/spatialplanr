# Arrange Features by Spatial Coordinates

`splnr_arrangeFeatures()` sorts the rows of an `sf` object based on the
longitude (X) and then latitude (Y) of its centroids. This ensures a
consistent ordering of planning units, which can be important for
reproducibility in some spatial analyses or data processing steps.

## Usage

``` r
splnr_arrangeFeatures(df)
```

## Arguments

- df:

  An `sf` object whose rows are to be sorted.

## Value

A sorted `sf` object, with rows ordered primarily by longitude (X) and
secondarily by latitude (Y) of their centroids.

## Details

This function computes the centroid for each polygon (or
point/multipoint) in the input `sf` object. It then extracts the X and Y
coordinates of these centroids and uses them to sort the entire `sf`
object. The primary sort key is the longitude (X-coordinate), and the
secondary sort key is the latitude (Y-coordinate).

Sorting can be beneficial for tasks like debugging, comparing data from
different runs, or ensuring deterministic behavior in algorithms that
process spatial units sequentially.

## Examples

``` r
if (FALSE) { # \dontrun{
print("Original order:")
print(dat_species_prob)

# Sort the features.
df_arranged <- splnr_arrangeFeatures(df = dat_species_prob)
print("Sorted order:")
print(df_arranged)
} # }
```
