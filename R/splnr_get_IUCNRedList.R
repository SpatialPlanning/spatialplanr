#' Match Species to IUCN RedList
#'
#'
#' First of all you will need your own API key, an alphanumeric string provided by IUCN that you need to send in every request;
#' the following function takes you to their website, where you will need to fill up a form (it might take 1-2 days to receive your key)
#' rl_use_iucn()

#' Once you receive an email with your API key, set it up as an environmental variable (it MUST be named IUCN_REDLIST_KEY)
#' you will need to re-do this step everytime you restart R

#' Sys.setenv(IUCN_REDLIST_KEY = "") OR add IUCN_REDLIST_KEY = "" to your .Renviron file to permanently set it
#' Sys.getenv("IUCN_REDLIST_KEY") #' check

#' Not Evaluated
#' DD: Data Deficient
#' LC: Least Concern
#' NT: Near Threatened
#' VU: Vulnerable
#' EN: Endangered
#' CR: Critically Endangered
#' EW: Extinct in the Wild
#' EX: Extinct
#' LRlc: Low risk – least concern
#' LRnt: Low risk – near threatened
#' LRcd: Low risk - conservation dependent

#' Categories we care about
#' cate <- c("EX","EW","CR","EN","VU")
#' @param df The dataframe containing the species to be matched with the IUCN redlist
#' @param species_col A string name for the column containting the species name
#'
#' @return A dataframe with an additional column `IUCN_Category`
#' @export
#'
#' @importFrom rlang :=
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' df <- data.frame(Species = c("Diomedea exulans", "Hippocampus kuda", "Squatina squatina")) %>%
#'   splnr_get_IUCNRedList()
#' }
splnr_get_IUCNRedList <- function(df, species_col = "Species") {

  assertthat::assert_that(
    inherits(df, "data.frame"),
    is.character(species_col),
    species_col %in% names(df)
  )

  # Get all IUCN categories
  cate <- c("DD", "LC", "NT", "VU", "EN", "CR", "EW", "EX", "LRlc", "LRnt", "LRcd")

  # Download all the data for those categories
  RL <- purrr::map_df(cate, function(x) data.frame(rredlist::rl_categories(x))) %>%
    dplyr::select("category", "result.scientific_name") %>%
    dplyr::rename(!!species_col := .data$result.scientific_name,
                  IUCN_Category = .data$category
    )

  # Now try and link the species to the categories - only links 2 %
  df <- df %>%
    dplyr::left_join(RL, by = species_col)
}
