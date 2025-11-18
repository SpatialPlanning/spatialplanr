# Assign Targets by Category

The `splnr_targets_byCategory()` function assigns conservation targets
to features (e.g., species) based on their assigned categories. This
allows for differentiated conservation goals for different groups of
features.

## Usage

``` r
splnr_targets_byCategory(dat, catTarg, catName = "Category")
```

## Arguments

- dat:

  An `sf` object (or data frame) containing the features and their
  associated categories. Each row should represent a feature (e.g., a
  species) with its attributes, including the category.

- catTarg:

  A named numeric vector where names are the categories (e.g.,
  `"Group1"`, `"Endangered"`) and values are the corresponding
  conservation targets (e.g., `0.5`, `0.8`).

- catName:

  A character string specifying the name of the column in `dat` that
  contains the category information. Defaults to `"Category"`.

## Value

An `sf` object (or data frame) identical to the input `dat`, but with an
additional column named `target` containing the assigned conservation
target for each feature. Features whose categories are not found in
`catTarg` will have `NA` in the `target` column unless they already have
a 'target' column.

## Details

This function is useful in conservation planning when different types of
features (e.g., endangered species, common species, ecosystem types)
require distinct conservation targets. It performs a left join with a
provided named vector (`catTarg`) where names correspond to categories
in your data and values are the desired targets.

The `dat` input should be an `sf` object (or data frame) that contains a
column (`catName`) identifying the category for each feature.

## Examples

``` r
if (FALSE) { # \dontrun{
# Assuming 'dat_category' is an existing sf object in your package
# with a column named "category" and other feature data.

# Example: Assign targets based on predefined categories
targets_by_group <- splnr_targets_byCategory(
  dat = dat_category, # Assuming dat_category has a 'category' column
  catTarg = c("Group1" = 0.5, "Group2" = 0.2),
  catName = "category"
)
print(targets_by_group)

# Example: Assign targets with a different category column name
dat_alt_cat <- data.frame(Feature = letters[1:5], Type = c("A", "B", "A", "C", "B"))
targets_by_type <- splnr_targets_byCategory(
  dat = dat_alt_cat,
  catTarg = c("A" = 0.7, "B" = 0.4),
  catName = "Type"
)
print(targets_by_type)
} # }
```
