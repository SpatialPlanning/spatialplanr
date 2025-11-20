# Function to interpolate data onto Planning Units

**\[deprecated\]**

## Usage

``` r
splnr_convert_toPUs(dat, PlanUnits)
```

## Arguments

- dat:

  Dataset or filename of dataset

- PlanUnits:

  `sf` object of Planning Units

## Value

`sf` object containing the Planning Units and the feature.

## Details

The dataset needs to be raster or vector format. If the input contains
continuous data, the output is an area-averaged mean for each planning
unit. If the input is binary, the output is the proportion of the
planning unit covered.

## Examples

``` r
if (FALSE) { # \dontrun{
df <- splnr_convert_toPUs(dat, PlanUnits)
} # }
```
