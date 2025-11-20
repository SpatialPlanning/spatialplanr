#' @title Match Species to IUCN RedList Categories
#'
#' @description
#' The `splnr_get_IUCNRedList` function retrieves IUCN Red List category information
#' for a given set of species and appends it to your input dataframe.
#'
#' @details
#' To use this function, you must first obtain an API key from IUCN. This is an
#' alphanumeric string required for every request. You can visit the IUCN website
#' to request a key using `rl_use_iucn()`. Please note that receiving your key
#' might take 1-2 days after submitting the form.
#'
#' Once you receive your API key, it is crucial to set it as an environment variable
#' named `IUCN_REDLIST_KEY`. You can do this temporarily for the current R session
#' using `Sys.setenv(IUCN_REDLIST_KEY = "YOUR_API_KEY_HERE")`. To set it permanently,
#' you should add `IUCN_REDLIST_KEY = "YOUR_API_KEY_HERE"` to your `.Renviron` file.
#' You can check if the key is set correctly using `Sys.getenv("IUCN_REDLIST_KEY")`.
#'
#' The IUCN Red List uses various categories to assess extinction risk. This function
#' queries the Red List for the following categories:
#' \itemize{
#'   \item \strong{DD}: Data Deficient
#'   \item \strong{LC}: Least Concern
#'   \item \strong{NT}: Near Threatened
#'   \item \strong{VU}: Vulnerable
#'   \item \strong{EN}: Endangered
#'   \item \strong{CR}: Critically Endangered
#'   \item \strong{EW}: Extinct in the Wild
#'   \item \strong{EX}: Extinct
#'   \item \strong{LRlc}: Lower Risk / least concern (old category)
#'   \item \strong{LRnt}: Lower Risk / near threatened (old category)
#'   \item \strong{LRcd}: Lower Risk / conservation dependent (old category)
#' }
#' The function will attempt to match your species against any of these categories
#' present in the IUCN Red List database.
#'
#' @param df The input dataframe containing the species names to be matched.
#' @param species_col A character string specifying the name of the column in `df`
#'   that contains the species scientific names (e.g., "Species" or "scientific_name").
#'   Defaults to "Species".
#'
#' @return A dataframe identical to the input `df`, but with an additional column
#'   named `IUCN_Category`. If a species is not found on the IUCN Red List, its
#'   `IUCN_Category` will be `NA`.
#' @export
#'
#' @importFrom rlang :=
#' @importFrom rlang .data
#' @importFrom purrr map_df
#' @importFrom dplyr select rename left_join
#' @importFrom assertthat assert_that
#'
#' @examples
#' \dontrun{
#' # Ensure your IUCN_REDLIST_KEY is set as an environment variable before running.
#' # For example: Sys.setenv(IUCN_REDLIST_KEY = "YOUR_API_KEY_HERE")
#'
#' # Example: Create a dataframe with species names and retrieve their IUCN Red List categories.
#' df_species_redlist <- data.frame(Species = c("Diomedea exulans",
#'                                               "Hippocampus kuda",
#'                                               "Squatina squatina")) %>%
#'   splnr_get_IUCNRedList()
#' print(df_species_redlist)
#'
#' # Example with a different column name for species
#' df_alt_col <- data.frame(ScientificName = c("Panthera leo", "Orcinus orca")) %>%
#'   splnr_get_IUCNRedList(species_col = "ScientificName")
#' print(df_alt_col)
#' }
splnr_get_IUCNRedList <- function(df, species_col = "Species") {

  # Assertions to validate input parameters.
  assertthat::assert_that(
    inherits(df, "data.frame"),
    msg = "The 'df' parameter must be a data.frame."
  )
  assertthat::assert_that(
    is.character(species_col),
    msg = "The 'species_col' parameter must be a character string."
  )
  assertthat::assert_that(
    species_col %in% names(df),
    msg = paste0("The specified 'species_col' (\"", species_col, "\") does not exist in the input dataframe.")
  )
  assertthat::assert_that(
    nchar(Sys.getenv("IUCN_REDLIST_KEY")) > 0,
    msg = "IUCN_REDLIST_KEY environment variable is not set. Please set your IUCN API key using Sys.setenv(IUCN_REDLIST_KEY = 'YOUR_KEY') or add it to your .Renviron file."
  )

  #TODO add check for rredlist package

  # Define all possible IUCN Red List categories to retrieve data for.
  cate <- c("DD", "LC", "NT", "VU", "EN", "CR", "EW", "EX", "LRlc", "LRnt", "LRcd")

  # Download all data for the defined categories using rredlist::rl_categories.
  # The map_df function iterates through each category, fetches species, and combines them into a single dataframe.
  RL <- purrr::map_df(cate, function(x) data.frame(rredlist::rl_categories(x))) %>%
    # Select only the 'category' and 'result.scientific_name' columns.
    dplyr::select("category", "result.scientific_name") %>%
    # Rename the selected columns to match the input dataframe's species column name
    # and a new column for the IUCN category for clarity.
    dplyr::rename(!!species_col := .data$result.scientific_name,
                  IUCN_Category = .data$category
    )

  # Perform a left join to link the species in the input dataframe to their
  # corresponding IUCN Red List categories. Species not found will have NA in IUCN_Category.
  df <- df %>%
    dplyr::left_join(RL, by = species_col)

  return(df)
}
