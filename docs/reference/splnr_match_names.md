# Substitute Numbers for Names in Regionalizations

`splnr_match_names()` replaces numeric or integer values in a spatial
(sf) dataframe's column with corresponding character names, typically
used for regionalization data.

## Usage

``` r
splnr_match_names(dat, nam)
```

## Arguments

- dat:

  An `sf` data frame with a single non-geometry column containing
  numeric or integer values that correspond to the names in `nam`.

- nam:

  A named character vector. The *names* of this vector should be the
  numeric/integer values found in `dat`'s column, and the *values* of
  this vector should be the desired character names for substitution.

## Value

An `sf` dataframe where the numeric/integer values in the relevant
column have been substituted with the corresponding character names from
`nam`.

## Details

This function is designed for scenarios where spatial data contains
numeric identifiers for regions, and you have a mapping (a named
character vector) to convert these IDs into more descriptive names. It
assumes that the `sf` dataframe (`dat`) has only one non-geometry column
that needs recoding.

The function directly applies the mapping from the `nam` vector to the
specified column. The names of the `nam` vector should correspond to the
numeric/integer values in the `dat` column, and the values of `nam` will
be the new character names.

## Examples

``` r
# Define the named character vector for mapping.
region_names <- c("Region1" = "SE Aust", "Region2" = "Tas", "Region3" = "NE Aust")

# Apply the function to substitute numeric codes with names.
df_named_regions <- splnr_match_names(dat = dat_region, nam = region_names)
print(df_named_regions)
#> Simple feature collection with 780 features and 1 field
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 100 ymin: -50 xmax: 160 ymax: 2
#> Geodetic CRS:  WGS 84
#> First 10 features:
#>                          geometry  Region
#> 1  POLYGON ((100 -50, 102 -50,... SE Aust
#> 2  POLYGON ((102 -50, 104 -50,... SE Aust
#> 3  POLYGON ((104 -50, 106 -50,... SE Aust
#> 4  POLYGON ((106 -50, 108 -50,... SE Aust
#> 5  POLYGON ((108 -50, 110 -50,... SE Aust
#> 6  POLYGON ((110 -50, 112 -50,... SE Aust
#> 7  POLYGON ((112 -50, 114 -50,... SE Aust
#> 8  POLYGON ((114 -50, 116 -50,... SE Aust
#> 9  POLYGON ((116 -50, 118 -50,... SE Aust
#> 10 POLYGON ((118 -50, 120 -50,... SE Aust
```
