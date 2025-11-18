# Match Species to IUCN RedList Categories

The `splnr_get_IUCNRedList` function retrieves IUCN Red List category
information for a given set of species and appends it to your input
dataframe.

## Usage

``` r
splnr_get_IUCNRedList(df, species_col = "Species")
```

## Arguments

- df:

  The input dataframe containing the species names to be matched.

- species_col:

  A character string specifying the name of the column in `df` that
  contains the species scientific names (e.g., "Species" or
  "scientific_name"). Defaults to "Species".

## Value

A dataframe identical to the input `df`, but with an additional column
named `IUCN_Category`. If a species is not found on the IUCN Red List,
its `IUCN_Category` will be `NA`.

## Details

To use this function, you must first obtain an API key from IUCN. This
is an alphanumeric string required for every request. You can visit the
IUCN website to request a key using `rl_use_iucn()`. Please note that
receiving your key might take 1-2 days after submitting the form.

Once you receive your API key, it is crucial to set it as an environment
variable named `IUCN_REDLIST_KEY`. You can do this temporarily for the
current R session using
`Sys.setenv(IUCN_REDLIST_KEY = "YOUR_API_KEY_HERE")`. To set it
permanently, you should add `IUCN_REDLIST_KEY = "YOUR_API_KEY_HERE"` to
your `.Renviron` file. You can check if the key is set correctly using
`Sys.getenv("IUCN_REDLIST_KEY")`.

The IUCN Red List uses various categories to assess extinction risk.
This function queries the Red List for the following categories:

- **DD**: Data Deficient

- **LC**: Least Concern

- **NT**: Near Threatened

- **VU**: Vulnerable

- **EN**: Endangered

- **CR**: Critically Endangered

- **EW**: Extinct in the Wild

- **EX**: Extinct

- **LRlc**: Lower Risk / least concern (old category)

- **LRnt**: Lower Risk / near threatened (old category)

- **LRcd**: Lower Risk / conservation dependent (old category)

The function will attempt to match your species against any of these
categories present in the IUCN Red List database.

## Examples

``` r
if (FALSE) { # \dontrun{
# Ensure your IUCN_REDLIST_KEY is set as an environment variable before running.
# For example: Sys.setenv(IUCN_REDLIST_KEY = "YOUR_API_KEY_HERE")

# Example: Create a dataframe with species names and retrieve their IUCN Red List categories.
df_species_redlist <- data.frame(Species = c("Diomedea exulans",
                                              "Hippocampus kuda",
                                              "Squatina squatina")) %>%
  splnr_get_IUCNRedList()
print(df_species_redlist)

# Example with a different column name for species
df_alt_col <- data.frame(ScientificName = c("Panthera leo", "Orcinus orca")) %>%
  splnr_get_IUCNRedList(species_col = "ScientificName")
print(df_alt_col)
} # }
```
