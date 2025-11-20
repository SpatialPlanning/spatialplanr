# Function to interpolate regionalisation data onto Planning Units

**\[deprecated\]**

## Usage

``` r
splnr_convert_regionalisation(dat, PUs, cat_name = NA, col_name = NA)
```

## Arguments

- dat:

  Dataset in raster or sf format.

- PUs:

  `sf` object of Planning Units

- cat_name:

  A character string of all categories in the regionalisation

- col_name:

  The name of the layer

## Value

`sf` object containing the Planning Units and the feature.

## Details

This is a wrapper for `splnr_Convert2PUs()` but deals with need to
processes each layer seperately

The dataset needs to be raster or vector format. If the input contains
continuous data, the output is an area-averaged mean for each planning
unit. If the input is binary, the output is the proportion of the
planning unit covered.

## Examples

``` r
if (FALSE) { # \dontrun{
df <- splnr_convert_regionalisation(dat, PUs)
} # }
```
