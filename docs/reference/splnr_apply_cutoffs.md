# Apply Cutoffs to Feature Data

`splnr_apply_cutoffs()` transforms numeric feature data in an `sf`
dataframe into binary (0 or 1) presence/absence values based on
specified cutoffs. It provides flexibility to either keep values above a
cutoff as 1 (default) or invert this logic to keep values below a cutoff
as 1.

## Usage

``` r
splnr_apply_cutoffs(features, Cutoffs, inverse = FALSE)
```

## Arguments

- features:

  An `sf` dataframe. It must contain a `geometry` column and at least
  one numeric column to which cutoffs will be applied.

- Cutoffs:

  A numeric value or a named numeric vector of cutoffs.

  - If a single unnamed numeric value, it's applied to all numeric
    columns.

  - If a named numeric vector, names must correspond to numeric column
    names in `features`.

  All cutoff values must be between `0` and `1`.

- inverse:

  A logical value (`TRUE` or `FALSE`). If `TRUE`, values below the
  `Cutoffs` are converted to `1` (and others to `0`). If `FALSE`
  (default), values at or above the `Cutoffs` are converted to `1`.

## Value

A modified `sf` dataframe with the same structure and geometry as
`features`, but with all targeted numeric columns transformed into
binary (0 or 1) values based on the specified cutoffs and `inverse`
setting.

## Details

This function is crucial for standardizing feature data, such as species
probability distributions or habitat suitability scores, into a binary
format often required for conservation planning and spatial analysis
(e.g., in `prioritizr`).

The function operates in two primary modes based on the `Cutoffs`
parameter:

- **Single Cutoff:** If `Cutoffs` is a single numeric value (e.g.,
  `0.5`), this value is applied uniformly to **all numeric columns** in
  the `features` dataframe, excluding the `geometry` column. For each
  numeric cell: - If `value >= Cutoffs`, it becomes `1`. - If
  `value < Cutoffs`, it becomes `0`. - `NA` values are always converted
  to `0`.

- **Named Vector of Cutoffs:** If `Cutoffs` is a named numeric vector
  (e.g., `c("feature1" = 0.5, "feature2" = 0.3)`), each specified cutoff
  is applied individually to its corresponding named column in
  `features`. This allows for different thresholds for different
  features. The same transformation rules as above apply to each
  specified column.

The `inverse` parameter provides additional control over the
binarization:

- `inverse = FALSE` (default): Values **at or above** the cutoff become
  `1`.

- `inverse = TRUE`: Values **below** the cutoff become `1`. After
  initial binarization (where values \>= cutoff are 1), the binary
  results are flipped (0s become 1s, and 1s become 0s) to achieve the
  inverse effect.

All `NA` values in the numeric columns are consistently converted to `0`
during the binarization process, regardless of the `inverse` setting.

## Examples

``` r

# Example 1: Single cutoff (0.5) applied to all numeric feature columns
# (Spp1_Prob, Spp2_Prob, and Cost will be binarized based on 0.5)
df_single_cutoff <- splnr_apply_cutoffs(dat_species_prob, Cutoffs = 0.5)
#> Applying single cutoff of 0.5 to all numeric feature columns.
print(df_single_cutoff)
#> Simple feature collection with 780 features and 5 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 100 ymin: -50 xmax: 160 ymax: 2
#> Geodetic CRS:  WGS 84
#> # A tibble: 780 × 6
#>                                           geometry  Spp1  Spp2  Spp3  Spp4  Spp5
#>                                      <POLYGON [°]> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1 ((100 -50, 102 -50, 102 -48, 100 -48, 100 -50))     1     0     0     0     0
#>  2 ((102 -50, 104 -50, 104 -48, 102 -48, 102 -50))     1     0     1     1     0
#>  3 ((104 -50, 106 -50, 106 -48, 104 -48, 104 -50))     0     1     0     1     1
#>  4 ((106 -50, 108 -50, 108 -48, 106 -48, 106 -50))     1     1     1     1     1
#>  5 ((108 -50, 110 -50, 110 -48, 108 -48, 108 -50))     0     1     0     1     1
#>  6 ((110 -50, 112 -50, 112 -48, 110 -48, 110 -50))     1     0     1     0     0
#>  7 ((112 -50, 114 -50, 114 -48, 112 -48, 112 -50))     0     1     0     0     0
#>  8 ((114 -50, 116 -50, 116 -48, 114 -48, 114 -50))     0     0     0     0     1
#>  9 ((116 -50, 118 -50, 118 -48, 116 -48, 116 -50))     0     1     1     0     1
#> 10 ((118 -50, 120 -50, 120 -48, 118 -48, 118 -50))     1     0     1     0     1
#> # ℹ 770 more rows

# Example 2: Named cutoffs for specific columns
# Spp1_Prob >= 0.6 becomes 1, Spp2_Prob >= 0.4 becomes 1
df_named_cutoffs <- splnr_apply_cutoffs(
  dat_species_prob,
  Cutoffs = c("Spp1" = 0.6, "Spp2" = 0.4)
)
#> Applying named cutoffs to specific feature columns.
#>   Applying cutoff 0.6 to column 'Spp1'.
#>   Applying cutoff 0.4 to column 'Spp2'.
print(df_named_cutoffs)
#> Simple feature collection with 780 features and 5 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 100 ymin: -50 xmax: 160 ymax: 2
#> Geodetic CRS:  WGS 84
#> # A tibble: 780 × 6
#>                                      geometry  Spp1  Spp2    Spp3    Spp4   Spp5
#>                                 <POLYGON [°]> <dbl> <dbl>   <dbl>   <dbl>  <dbl>
#>  1 ((100 -50, 102 -50, 102 -48, 100 -48, 100…     1     0 0.0969  0.435   0.0418
#>  2 ((102 -50, 104 -50, 104 -48, 102 -48, 102…     0     1 0.504   0.503   0.360 
#>  3 ((104 -50, 106 -50, 106 -48, 104 -48, 104…     0     1 0.285   0.755   0.653 
#>  4 ((106 -50, 108 -50, 108 -48, 106 -48, 106…     0     1 0.564   0.503   0.529 
#>  5 ((108 -50, 110 -50, 110 -48, 108 -48, 108…     0     1 0.150   0.863   0.753 
#>  6 ((110 -50, 112 -50, 112 -48, 110 -48, 110…     1     1 0.807   0.458   0.374 
#>  7 ((112 -50, 114 -50, 114 -48, 112 -48, 112…     0     1 0.00963 0.102   0.114 
#>  8 ((114 -50, 116 -50, 116 -48, 114 -48, 114…     0     0 0.481   0.231   0.764 
#>  9 ((116 -50, 118 -50, 118 -48, 116 -48, 116…     0     1 0.552   0.00978 0.552 
#> 10 ((118 -50, 120 -50, 120 -48, 118 -48, 118…     1     1 0.695   0.00687 0.815 
#> # ℹ 770 more rows

# Example 3: Single cutoff (0.5) with inverse logic
# Values BELOW 0.5 become 1.
df_inverse_cutoff <- splnr_apply_cutoffs(dat_species_prob, Cutoffs = 0.5, inverse = TRUE)
#> Applying single cutoff of 0.5 to all numeric feature columns.
#> Inverse logic applied: values below cutoff will be 1.
print(df_inverse_cutoff)
#> Simple feature collection with 780 features and 5 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 100 ymin: -50 xmax: 160 ymax: 2
#> Geodetic CRS:  WGS 84
#> # A tibble: 780 × 6
#>                                           geometry  Spp1  Spp2  Spp3  Spp4  Spp5
#>                                      <POLYGON [°]> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1 ((100 -50, 102 -50, 102 -48, 100 -48, 100 -50))     0     1     1     1     1
#>  2 ((102 -50, 104 -50, 104 -48, 102 -48, 102 -50))     0     1     0     0     1
#>  3 ((104 -50, 106 -50, 106 -48, 104 -48, 104 -50))     1     0     1     0     0
#>  4 ((106 -50, 108 -50, 108 -48, 106 -48, 106 -50))     0     0     0     0     0
#>  5 ((108 -50, 110 -50, 110 -48, 108 -48, 108 -50))     1     0     1     0     0
#>  6 ((110 -50, 112 -50, 112 -48, 110 -48, 110 -50))     0     1     0     1     1
#>  7 ((112 -50, 114 -50, 114 -48, 112 -48, 112 -50))     1     0     1     1     1
#>  8 ((114 -50, 116 -50, 116 -48, 114 -48, 114 -50))     1     1     1     1     0
#>  9 ((116 -50, 118 -50, 118 -48, 116 -48, 116 -50))     1     0     0     1     0
#> 10 ((118 -50, 120 -50, 120 -48, 118 -48, 118 -50))     0     1     0     1     0
#> # ℹ 770 more rows

# Example 4: Named cutoffs with inverse logic
df_named_inverse <- splnr_apply_cutoffs(
  dat_species_prob,
  Cutoffs = c("Spp1" = 0.7, "Spp2" = 0.3),
  inverse = TRUE
)
#> Applying named cutoffs to specific feature columns.
#>   Applying cutoff 0.7 to column 'Spp1'.
#>   Inverse logic applied for column 'Spp1': values below cutoff will be 1.
#>   Applying cutoff 0.3 to column 'Spp2'.
#>   Inverse logic applied for column 'Spp2': values below cutoff will be 1.
print(df_named_inverse)
#> Simple feature collection with 780 features and 5 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 100 ymin: -50 xmax: 160 ymax: 2
#> Geodetic CRS:  WGS 84
#> # A tibble: 780 × 6
#>                                      geometry  Spp1  Spp2    Spp3    Spp4   Spp5
#>                                 <POLYGON [°]> <dbl> <dbl>   <dbl>   <dbl>  <dbl>
#>  1 ((100 -50, 102 -50, 102 -48, 100 -48, 100…     1     1 0.0969  0.435   0.0418
#>  2 ((102 -50, 104 -50, 104 -48, 102 -48, 102…     1     0 0.504   0.503   0.360 
#>  3 ((104 -50, 106 -50, 106 -48, 104 -48, 104…     1     0 0.285   0.755   0.653 
#>  4 ((106 -50, 108 -50, 108 -48, 106 -48, 106…     1     0 0.564   0.503   0.529 
#>  5 ((108 -50, 110 -50, 110 -48, 108 -48, 108…     1     0 0.150   0.863   0.753 
#>  6 ((110 -50, 112 -50, 112 -48, 110 -48, 110…     0     0 0.807   0.458   0.374 
#>  7 ((112 -50, 114 -50, 114 -48, 112 -48, 112…     1     0 0.00963 0.102   0.114 
#>  8 ((114 -50, 116 -50, 116 -48, 114 -48, 114…     1     1 0.481   0.231   0.764 
#>  9 ((116 -50, 118 -50, 118 -48, 116 -48, 116…     1     0 0.552   0.00978 0.552 
#> 10 ((118 -50, 120 -50, 120 -48, 118 -48, 118…     1     0 0.695   0.00687 0.815 
#> # ℹ 770 more rows
```
