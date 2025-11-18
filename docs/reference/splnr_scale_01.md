# Scale Spatial Layers to Between 0 and 1

`splnr_scale_01()` re-scales the numeric values in a specified column of
an `sf` dataframe to a range between 0 and 1. This is particularly
useful for normalizing data like probabilities or costs.

## Usage

``` r
splnr_scale_01(dat, col_name)
```

## Arguments

- dat:

  An `sf` dataframe containing the column to be scaled.

- col_name:

  A character string specifying the name of the numeric column in `dat`
  that needs to be scaled.

## Value

An `sf` dataframe identical to the input `dat`, but with the values in
the `col_name` column re-scaled to be between 0 and 1.

## Details

This function inspects the maximum value (`mx`) in the `col_name`
column. It then divides all values in that column by a `divi` factor to
bring them into the 0-1 range. The `divi` factor is determined
heuristically:

- If `mx > 100`, `divi` is `1000`.

- If `mx > 10`, `divi` is `100`.

- If `mx > 1`, `divi` is `10`.

- If `mx <= 1`, no division is performed (`divi` is `1`), as the data is
  already within the desired range.

This approach ensures that the data is scaled appropriately without
hardcoding a fixed division factor.

## Examples

``` r
if (FALSE) { # \dontrun{
# Scale the 'Spp1' column.
df_scaled_spp1 <- splnr_scale_01(dat = dat_species_prob, col_name = "Spp1")
print(df_scaled_spp1)

# Example where max is already <= 1
df_already_scaled <- dat_species_prob %>% dplyr::mutate(Spp1 = Spp1 / 100)
df_no_change <- splnr_scale_01(dat = df_already_scaled, col_name = "Spp1")
print(df_no_change) # Spp1 values should remain unchanged
} # }
```
