#' Function to interpolate regionalisation data onto Planning Units
#'
#' `r lifecycle::badge("deprecated")`
#'
#' This is a wrapper for `splnr_Convert2PUs()` but deals with need to processes each layer seperately
#'
#' The dataset needs to be raster or vector format.
#' If the input contains continuous data, the output is an area-averaged mean for each planning unit.
#' If the input is binary, the output is the proportion of the planning unit covered.
#'
#' @param dat Dataset in raster or sf format.
#' @param PUs `sf` object of Planning Units
#' @param cat_name A character string of all categories in the regionalisation
#' @param col_name The name of the layer
#'
#' @return `sf` object containing the Planning Units and the feature.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- splnr_convert_regionalisation(dat, PUs)
#' }
splnr_convert_regionalisation <- function(dat, PUs, cat_name = NA, col_name = NA) {

  lifecycle::deprecate_stop("0.6.2", "splnr_convert_regionalisation()", "spatialgridr::get_data_in_grid()")

}


#' Function to interpolate data onto Planning Units
#'
#' `r lifecycle::badge("deprecated")`
#'
#' The dataset needs to be raster or vector format.
#' If the input contains continuous data, the output is an area-averaged mean for each planning unit.
#' If the input is binary, the output is the proportion of the planning unit covered.
#'
#' @param dat Dataset or filename of dataset
#' @param PlanUnits `sf` object of Planning Units
#'
#' @return `sf` object containing the Planning Units and the feature.
#' @export
#'
#' @importFrom rlang .data
#' @importFrom rlang :=
#'
#' @examples
#' \dontrun{
#' df <- splnr_convert_toPUs(dat, PlanUnits)
#' }
splnr_convert_toPUs <- function(dat, PlanUnits) {

  lifecycle::deprecate_stop("0.6.2", "splnr_convert_toPUs()", "spatialgridr::get_data_in_grid()")

}




#' Plot MPAs
#'
#' `splnr_plot_MPAs()` allows to plot either the outline or the area of MPAs existing in the planning region (for example extracted with the `spatialplanr`function [splnr_get_MPAs()]) in a customisable way using `ggplot2`. This function requires an `sf` object containing the information whether a planning unit in the planning region lies within an MPA or not in a column called `wdpa` and outputs a `ggobject`. It can be combined with the `spatialplanr` function [splnr_gg_add()].
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @param df An `sf` object of marine protected areas
#' @param colorVals A `list` object of named vectors that will match the color value with the according name. "TRUE" stands for selected planning units.
#' @param showLegend A logical command on whether to show the legend of the solution (Default: TRUE).
#' @param plotTitle A character value for the title of the plot. Can be empty ("").
#' @param legendTitle A character value for the title of the legend. Can be empty ("").
#'
#' @return A ggplot object of the plot
#' @export
#'
#' @examples
#' \dontrun{
#' splnr_plot_MPAs(dat_mpas)
#' }
splnr_plot_MPAs <- function(df, colorVals = c("TRUE" = "blue", "FALSE" = "white"),
                            showLegend = TRUE, plotTitle = "Locked In Areas", legendTitle = "") {

  lifecycle::deprecate_stop("0.6.2", "splnr_plot_MPAs()", "splnr_plot()")

}


#' Plot cost
#'
#' `splnr_plot_cost()` allows to plot cost within each planning units of a planning region in a customisable way using `ggplot2`. This function requires an `sf` object with a cost column and outputs a `ggobject`. It can be combined with the `spatialplanr` function [splnr_gg_add()].
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @param cost An `sf` object of cost for `prioritizr`
#' @param costName Name of the cost column
#' @param legendTitle A character value for the title of the legend. Can be empty ("").
#' @param paletteName A string (or number) for the color palette to use. Available palettes can be found at https://ggplot2.tidyverse.org/reference/scale_brewer.html.
#' @param plotTitle A character value for the title of the plot. Can be empty ("").
#'
#' @return A ggplot object of the plot
#' @export
#'
#' @examples
#' \dontrun{
#' dat_problem <- prioritizr::problem(dat_species_bin %>% dplyr::mutate(cost = runif(n = dim(.)[[1]])),
#'   features = c("Spp1", "Spp2", "Spp3", "Spp4", "Spp5"),
#'   cost_column = "Cost"
#' ) %>%
#'   prioritizr::add_min_set_objective() %>%
#'   prioritizr::add_relative_targets(0.3) %>%
#'   prioritizr::add_binary_decisions() %>%
#'   prioritizr::add_default_solver(verbose = FALSE)
#'
#' dat_soln <- dat_problem %>%
#'   prioritizr::solve.ConservationProblem()
#'
#' dat_cost <- dat_soln %>%
#'   dplyr::mutate(Cost = runif(n = dim(.)[[1]]))
#'
#' (splnr_plot_cost(dat_cost))
#' }
splnr_plot_cost <- function(cost, costName = "Cost", legendTitle = "Cost",
                            paletteName = "YlGnBu", plotTitle = "") {

  lifecycle::deprecate_stop("0.6.2", "splnr_plot_MPAs()", "splnr_plot()")

}



#' Plot binary feature
#'
#' `splnr_plot_binFeature()` allows to plot presences and absences of a feature in the planning region in a customisable way using `ggplot2`. This function requires an `sf` object with binary information of a feature(`0` for absences and `1` for presences, for example created from continuous data with the `spatialplanr` function [splnr_apply_cutoffs()]). It outputs a `ggobject` and can be combined with the `spatialplanr` function [splnr_gg_add()].
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @param df A `data frame` with binary feature information
#' @param colInterest column of data frame that contains binary information of feature to plot
#' @param colorVals A `list` object of named vectors that will match the color value with the according name. "TRUE" stands for selected planning units.
#' @param showLegend A logical command on whether to show the legend of the solution (Default: TRUE).
#' @param plotTitle A character value for the title of the plot. Can be empty ("").
#' @param legendTitle A character value for the title of the legend. Can be empty ("").
#'
#' @return A ggplot object of the plot
#' @export
#'
#' @examples
#' \dontrun{
#' splnr_plot_binFeature(dat_species_bin, dat_species_bin$Spp1)
#' }
splnr_plot_binFeature <- function(df, colInterest,
                                  colorVals = c("Suitable" = "#3182bd", "Not Suitable" = "#c6dbef"),
                                  showLegend = TRUE, plotTitle = " ", legendTitle = "Habitat") {

  lifecycle::deprecate_stop("0.6.2", "splnr_plot_binFeature()", "splnr_plot()")

}


#' Plot number of features
#'
#' `splnr_plot_featureNo()` allows you to use `ggplot2` to visually inspect the number of features per planning unit that are used as inputs in the conservation problem. When all features are species, this map can be seen as a visualisation of species richness in the planning region.
#'
#' `r lifecycle::badge("deprecated")`
#'
#' This function requires an `sf` object with binary information of all features you want to include in the richness plot (`0` for absences and `1` for presences, for example created from continuous data with the `spatialplanr` function [splnr_apply_cutoffs()]). It outputs a `ggobject` and can be combined with the `spatialplanr` function [splnr_gg_add()].
#'
#' @param df An `sf` object of features
#' @param paletteName A string (or number) for the color palette to use. Available palettes can be found at https://ggplot2.tidyverse.org/reference/scale_brewer.html.
#' @param showLegend A logical command on whether to show the legend of the solution (Default: TRUE).
#' @param plotTitle A character value for the title of the plot. Can be empty ("").
#' @param legendTitle A character value for the title of the legend. Can be empty ("").
#'
#' @return A ggplot object of the plot
#' @export
#'
#' @importFrom rlang .data
#' @examples
#' \dontrun{
#' (splnr_plot_featureNo(dat_species_bin))
#' }
splnr_plot_featureNo <- function(df, showLegend = TRUE, paletteName = "YlGnBu",
                                 plotTitle = "Number of Features", legendTitle = "Features") {

  lifecycle::deprecate_stop("0.6.2", "splnr_plot_featureNo()", "splnr_plot()")

}



