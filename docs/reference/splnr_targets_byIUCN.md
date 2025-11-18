# Assign Targets by IUCN Red List Categories

The `splnr_targets_byIUCN()` function assigns conservation targets for
species based on their IUCN Red List categories. This allows for
prioritizing species at higher risk of extinction with more stringent
conservation goals.

## Usage

``` r
splnr_targets_byIUCN(dat, IUCN_target, IUCN_col = "IUCN_Category")
```

## Arguments

- dat:

  A dataframe or `sf` object containing species information, including a
  column with IUCN categories.

- IUCN_target:

  Either:

  - A single numeric value (e.g., `0.3`) to apply this target to all
    threatened IUCN categories ("EX", "EW", "CR", "EN", "VU").

  - A named numeric vector (e.g., `c("EX" = 0.8, "CR" = 0.6)`) to apply
    specific targets to particular IUCN categories.

- IUCN_col:

  A character string specifying the name of the column in `dat` that
  contains the IUCN category information. Defaults to `"IUCN_Category"`.

## Value

A dataframe or `sf` object identical to the input `dat`, but with an
updated or newly added `target` column reflecting the assigned
conservation goals.

## Details

This function is crucial for integrating species' extinction risk into
conservation planning. It allows you to specify targets either as a
single numeric value (applied to all 'threatened' IUCN categories) or as
a named numeric vector for specific categories.

Species can be extracted based on IUCN categories using the
`spatialplanr` function
[`splnr_get_IUCNRedList()`](https://mathmarecol.github.io/spatialplanr/reference/splnr_get_IUCNRedList.md).

**Important:** To access the IUCN database (e.g., via
[`splnr_get_IUCNRedList()`](https://mathmarecol.github.io/spatialplanr/reference/splnr_get_IUCNRedList.md)),
you need an API login token. This token, obtained from
[`rredlist::rl_use_iucn()`](https://docs.ropensci.org/rredlist/reference/rl_use_iucn.html),
must be set as an environment variable named `IUCN_REDLIST_KEY` (e.g.,
`Sys.setenv(IUCN_REDLIST_KEY = "[Your Token]")`).

The function checks if a 'target' column already exists in `dat`. If
not, it creates one. If it exists, new targets are coalesced with
existing ones, allowing for sequential application or refinement of
targets.

The "threatened" IUCN categories considered for target assignment (when
a single `IUCN_target` is provided) are: "EX" (Extinct), "EW" (Extinct
in the Wild), "CR" (Critically Endangered), "EN" (Endangered), and "VU"
(Vulnerable).

## Examples

``` r
if (FALSE) { # \dontrun{
# Example 1: Assigning specific targets to categories
# Create a dummy dataframe resembling output from splnr_get_IUCNRedList
df_species_iucn <- data.frame(
  Species = c("Diomedea exulans", "Hippocampus kuda",
              "Squatina squatina", "Common Dolphin"),
  IUCN_Category = c("VU", "EN", "CR", "LC")
)

iucn_specific_targets <- c("EX" = 0.9, "EW" = 0.8, "CR" = 0.75, "EN" = 0.6, "VU" = 0.5)

df_with_iucn_targets <- splnr_targets_byIUCN(
  dat = df_species_iucn,
  IUCN_target = iucn_specific_targets,
  IUCN_col = "IUCN_Category"
)
print(df_with_iucn_targets)

# Example 2: Assigning a single target to all threatened categories
df_single_target <- splnr_targets_byIUCN(
  dat = df_species_iucn,
  IUCN_target = 0.4, # Apply 40% target to all threatened species
  IUCN_col = "IUCN_Category"
)
print(df_single_target)

# Example 3: When 'dat' already has a 'target' column
df_pre_targets <- data.frame(
  Species = c("A", "B", "C"),
  IUCN_Category = c("CR", "LC", "EN"),
  target = c(0.1, 0.2, 0.1) # Existing targets
)
iucn_update_targets <- c("CR" = 0.7) # Only update CR
df_updated_targets <- splnr_targets_byIUCN(df_pre_targets, iucn_update_targets)
print(df_updated_targets)
} # }
```
