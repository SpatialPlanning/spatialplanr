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

  One of:

  - A single unnamed numeric value in `[0, 1]` — applied to all numeric
    columns.

  - A single unnamed function that accepts a numeric vector and returns
    a single numeric in `[0, 1]` — called independently per column with
    the non-`NA` values of that column.

  - A named numeric vector — names must match numeric column names in
    `features`; each value is applied to its named column only.

  - A named list of numerics and/or functions — names must match numeric
    column names in `features`; each entry is applied to its named
    column only.

- inverse:

  A logical value (`TRUE` or `FALSE`). If `TRUE`, values below the
  threshold are converted to `1` (and others to `0`). If `FALSE`
  (default), values at or above the threshold are converted to `1`.

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

The function operates in four modes based on the `Cutoffs` parameter:

- **Single numeric scalar:** A single unnamed numeric value (e.g.,
  `0.5`) is applied uniformly to **all numeric columns** in `features`,
  excluding the `geometry` column.

- **Single function:** A single unnamed function (e.g.,
  `\(x) quantile(x, 0.99)`) is called independently for each numeric
  column, with `x` being the non-`NA` values of that column. The
  returned scalar is then used as the threshold for that column. Because
  the function is evaluated per-column, different columns may receive
  different thresholds even though the same function is supplied.

- **Named numeric vector:** A named numeric vector (e.g.,
  `c("feature1" = 0.5, "feature2" = 0.3)`) applies each value to its
  corresponding named column only.

- **Named list of numerics and/or functions:** A named list (e.g.,
  `list("feature1" = 0.5, "feature2" = \(x) quantile(x, 0.99))`) applies
  each entry to its corresponding named column. Numeric entries are used
  directly; function entries are called with the non-`NA` values of that
  column and must return a single numeric in `[0, 1]`.

For all modes, the binarisation rules are:

- If `value >= threshold`, it becomes `1`.

- If `value < threshold`, it becomes `0`.

- `NA` values are always converted to `0`.

The `inverse` parameter flips the result after binarisation:

- `inverse = FALSE` (default): values **at or above** the threshold
  become `1`.

- `inverse = TRUE`: values **below** the threshold become `1`.

All resolved threshold values (whether supplied directly or returned by
a function) must lie in `[0, 1]`. `NA` values are stripped from the
column vector before it is passed to a function-based cutoff.

## Examples

``` r

# Example 1: Single numeric cutoff applied to all numeric feature columns
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

# Example 2: Single function cutoff applied to all numeric feature columns
# Each column independently receives the 99th-percentile of its own values
# as its threshold.
df_fn_cutoff <- splnr_apply_cutoffs(
  dat_species_prob,
  Cutoffs = \(x) quantile(x, 0.99)
)
#> Applying function-based cutoff independently to each numeric feature column.
#>   Column 'Spp1': resolved threshold = 0.985033159928862
#>   Column 'Spp2': resolved threshold = 0.993628154252656
#>   Column 'Spp3': resolved threshold = 0.992002531981561
#>   Column 'Spp4': resolved threshold = 0.987106560785323
#>   Column 'Spp5': resolved threshold = 0.986841972519178
print(df_fn_cutoff)
#> Simple feature collection with 780 features and 5 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 100 ymin: -50 xmax: 160 ymax: 2
#> Geodetic CRS:  WGS 84
#> # A tibble: 780 × 6
#>                                           geometry  Spp1  Spp2  Spp3  Spp4  Spp5
#>                                      <POLYGON [°]> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1 ((100 -50, 102 -50, 102 -48, 100 -48, 100 -50))     0     0     0     0     0
#>  2 ((102 -50, 104 -50, 104 -48, 102 -48, 102 -50))     0     0     0     0     0
#>  3 ((104 -50, 106 -50, 106 -48, 104 -48, 104 -50))     0     0     0     0     0
#>  4 ((106 -50, 108 -50, 108 -48, 106 -48, 106 -50))     0     0     0     0     0
#>  5 ((108 -50, 110 -50, 110 -48, 108 -48, 108 -50))     0     0     0     0     0
#>  6 ((110 -50, 112 -50, 112 -48, 110 -48, 110 -50))     0     0     0     0     0
#>  7 ((112 -50, 114 -50, 114 -48, 112 -48, 112 -50))     0     0     0     0     0
#>  8 ((114 -50, 116 -50, 116 -48, 114 -48, 114 -50))     0     0     0     0     0
#>  9 ((116 -50, 118 -50, 118 -48, 116 -48, 116 -50))     0     0     0     0     0
#> 10 ((118 -50, 120 -50, 120 -48, 118 -48, 118 -50))     0     0     0     0     0
#> # ℹ 770 more rows

# Example 3: Named numeric cutoffs for specific columns
df_named_cutoffs <- splnr_apply_cutoffs(
  dat_species_prob,
  Cutoffs = c("Spp1" = 0.6, "Spp2" = 0.4)
)
#> Applying named cutoffs to specific feature columns.
#>   Column 'Spp1': resolved threshold = 0.6
#>   Column 'Spp2': resolved threshold = 0.4
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

# Example 4: Named list mixing fixed and function-based cutoffs
df_mixed_cutoffs <- splnr_apply_cutoffs(
  dat_species_prob,
  Cutoffs = list(
    "Spp1" = 0.5,
    "Spp2" = \(x) quantile(x, 0.99)
  )
)
#> Applying named cutoffs to specific feature columns.
#>   Column 'Spp1': resolved threshold = 0.5
#>   Column 'Spp2': resolved threshold = 0.993628154252656
print(df_mixed_cutoffs)
#> Simple feature collection with 780 features and 5 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 100 ymin: -50 xmax: 160 ymax: 2
#> Geodetic CRS:  WGS 84
#> # A tibble: 780 × 6
#>                                      geometry  Spp1  Spp2    Spp3    Spp4   Spp5
#>                                 <POLYGON [°]> <dbl> <dbl>   <dbl>   <dbl>  <dbl>
#>  1 ((100 -50, 102 -50, 102 -48, 100 -48, 100…     1     0 0.0969  0.435   0.0418
#>  2 ((102 -50, 104 -50, 104 -48, 102 -48, 102…     1     0 0.504   0.503   0.360 
#>  3 ((104 -50, 106 -50, 106 -48, 104 -48, 104…     0     0 0.285   0.755   0.653 
#>  4 ((106 -50, 108 -50, 108 -48, 106 -48, 106…     1     0 0.564   0.503   0.529 
#>  5 ((108 -50, 110 -50, 110 -48, 108 -48, 108…     0     0 0.150   0.863   0.753 
#>  6 ((110 -50, 112 -50, 112 -48, 110 -48, 110…     1     0 0.807   0.458   0.374 
#>  7 ((112 -50, 114 -50, 114 -48, 112 -48, 112…     0     0 0.00963 0.102   0.114 
#>  8 ((114 -50, 116 -50, 116 -48, 114 -48, 114…     0     0 0.481   0.231   0.764 
#>  9 ((116 -50, 118 -50, 118 -48, 116 -48, 116…     0     0 0.552   0.00978 0.552 
#> 10 ((118 -50, 120 -50, 120 -48, 118 -48, 118…     1     0 0.695   0.00687 0.815 
#> # ℹ 770 more rows

# Example 5: Single numeric cutoff with inverse logic
df_inverse_cutoff <- splnr_apply_cutoffs(dat_species_prob, Cutoffs = 0.5, inverse = TRUE)
#> Applying single cutoff of 0.5 to all numeric feature columns.
#> Inverse logic applied: values below threshold will be 1.
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
```
