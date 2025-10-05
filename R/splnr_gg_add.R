#' @title Add-ons for Plotting `spatialplanr` Maps
#'
#' @description
#' This function allows users to customize existing `ggplot2` maps, particularly
#' those produced by other `spatialplanr` spatial plotting functions. It provides
#' options to add various spatial layers and apply consistent theming in a
#' simple and reproducible manner.
#'
#' @details
#' The `splnr_gg_add` function enhances `ggplot2` objects by layering additional
#' spatial data such as planning unit outlines, study area boundaries, general
#' overlays, geographical contours, and 'locked-in' areas (e.g., existing protected
#' areas in a conservation prioritization). It offers fine-grained control over
#' colors, opacities, and legend appearance for each added layer.
#'
#' When using `contours`, the input `sf` object is expected to have a column
#' named `Category` that defines the different contour lines to be plotted.
#' The function currently supports up to 6 distinct contour categories for plotting.
#'
#' The `ggtheme` parameter offers flexibility in plot styling. `"Default"` applies
#' a standard `spatialplanr` theme (`theme_bw()` with custom text and axis settings).
#' A `list` of `ggplot2::theme()` elements can be provided for full customization,
#' or `NA` (logical `FALSE`) to apply no default theme, allowing the user to manage
#' all theme elements manually.
#'
#' @param PUs An `sf` object representing planning units. If provided, their
#'   outlines will be drawn. Defaults to `NULL`.
#' @param colorPUs A character string specifying the color for the outlines of the
#'   planning units. Defaults to `"grey80"`.
#' @param Bndry An `sf` object representing the main planning region boundaries.
#'   If provided, its outline will be drawn. Defaults to `NULL`.
#' @param colorBndry A character string specifying the color for the outline of the
#'   `Bndry` object. Defaults to `"black"`.
#' @param overlay An `sf` object to be plotted as a general overlay. Defaults to `NULL`.
#' @param colorOverlay A character string specifying the color for `overlay`.
#'   Defaults to `"grey20"`.
#' @param overlay2 An `sf` object for a second general overlay. Defaults to `NULL`.
#' @param colorOverlay2 A character string specifying the color for `overlay2`.
#'   Defaults to `"grey30"`.
#' @param overlay3 An `sf` object for a third general overlay. Defaults to `NULL`.
#' @param colorOverlay3 A character string specifying the color for `overlay3`.
#'   Defaults to `"grey40"`.
#' @param cropOverlay An `sf` object. Its bounding box will be used to set the
#'   `xlim` and `ylim` of the `ggplot2::coord_sf` layer, effectively cropping the view.
#'   Defaults to `NULL`.
#' @param contours An `sf` object containing contour lines (e.g., bathymetry or
#'   seamount outlines). It is expected to have a `Category` column for differentiating
#'   lines. Up to 6 categories are supported. Defaults to `NULL`.
#' @param colorConts A character string specifying the color for the contour lines.
#'   Defaults to `"black"`.
#' @param lockIn An `sf` object representing 'locked-in' areas (e.g., existing
#'   Marine Protected Areas) that are fixed in a conservation prioritization.
#'   Defaults to `NULL`.
#' @param typeLockIn A character string specifying how `lockIn` areas should be
#'   plotted. Can be `"Full"` (fills the areas with `colorLockIn`) or `"Contours"`
#'   (draws only the outlines of the areas). Defaults to `"Full"`.
#' @param nameLockIn A character string specifying the column name in the `lockIn`
#'   data frame that contains binary (0/1 or TRUE/FALSE) information indicating
#'   locked-in status. Required if `lockIn` is not `NULL`.
#' @param alphaLockIn A numeric value (0 to 1) for the opacity of the `lockIn`
#'   areas when `typeLockIn` is `"Full"`. Defaults to `0.5`.
#' @param colorLockIn A character string specifying the color for the `lockIn` areas.
#'   Defaults to `"black"`.
#' @param legendLockIn A character string for the title of the `lockIn` legend.
#'   Can be an empty string `""` to suppress the title. Defaults to `""`.
#' @param labelLockIn A character string for the legend label of the `lockIn` areas
#'   (e.g., "MPAs"). Defaults to `"MPAs"`.
#' @param ggtheme The `ggplot2` theme to apply. Can be:
#'   \itemize{
#'     \item `NA` or `FALSE`: No theme is applied, using `ggplot2` defaults.
#'     \item `"Default"`: Applies a `spatialplanr` default theme (`theme_bw()`
#'           with custom text/axis settings).
#'     \item A `list` of `ggplot2::theme()` properties for custom styling.
#'   }
#'   Defaults to `"Default"`.
#'
#' @return A `list` of `ggplot2` layers and theme elements that can be added to
#'   an existing `ggplot` object using `+`.
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter mutate rename
#' @importFrom ggnewscale new_scale_colour new_scale_fill
#' @importFrom ggplot2 aes coord_sf geom_sf guide_legend element_blank
#' @importFrom ggplot2 element_text labs scale_fill_manual scale_linetype_manual
#' @importFrom ggplot2 theme theme_bw
#' @importFrom sf st_bbox st_union st_as_sf
#' @importFrom rlang .data
#' @importFrom grid unit
#'
#' @examples
#' \dontrun{
#' # Assuming 'dat_species_bin' and 'dat_PUs' are existing sf objects
#' # in your package, suitable for prioritisation problems and plotting.
#'
#' # Create a dummy prioritizr problem and solve it for demonstration.
#' dat_problem <- prioritizr::problem(
#'   dat_species_bin %>% dplyr::mutate(Cost = runif(n = dim(.)[[1]])),
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
#' # Basic plot of the solution with default planning unit outlines and theme.
#' plot_basic <- splnr_plot_solution(dat_soln) +
#'   splnr_gg_add(PUs = dat_PUs, ggtheme = "Default")
#' print(plot_basic)
#'
#' # Example with boundary, a custom overlay, and locked-in areas shown as contours.
#' # For this example, let's create dummy `bndry_sf` and `locked_in_sf` based on `dat_PUs`
#' # In a real scenario, these would be loaded from your package or data.
#' bndry_sf <- sf::st_union(dat_PUs) %>% sf::st_as_sf()
#' locked_in_sf <- dat_PUs[1:100, ] %>% dplyr::mutate(is_mpa = 1)
#'
#' plot_custom <- splnr_plot_solution(dat_soln) +
#'   splnr_gg_add(
#'     PUs = dat_PUs,
#'     Bndry = bndry_sf,
#'     colorBndry = "darkblue",
#'     overlay = bndry_sf, # Using boundary as an example overlay
#'     colorOverlay = "lightblue",
#'     alphaOverlay = 0.3,
#'     lockIn = locked_in_sf,
#'     typeLockIn = "Contours",
#'     nameLockIn = "is_mpa",
#'     colorLockIn = "darkred",
#'     labelLockIn = "Existing MPAs",
#'     ggtheme = "Default"
#'   )
#' print(plot_custom)
#'
#' # Example with custom ggplot2 theme settings (as a list)
#' custom_theme_list <- list(
#'   ggplot2::theme_classic(),
#'   ggplot2::theme(
#'     plot.background = ggplot2::element_rect(fill = "lightyellow"),
#'     legend.position = "top"
#'   )
#' )
#' plot_with_custom_theme <- splnr_plot_solution(dat_soln) +
#'   splnr_gg_add(PUs = dat_PUs, ggtheme = custom_theme_list)
#' print(plot_with_custom_theme)
#' }
splnr_gg_add <- function(PUs = NULL, colorPUs = "grey80",
                         Bndry = NULL, colorBndry = "black",
                         overlay = NULL, colorOverlay = "grey20",
                         overlay2 = NULL, colorOverlay2 = "grey30",
                         overlay3 = NULL, colorOverlay3 = "grey40",
                         contours = NULL, colorConts = "black",
                         cropOverlay = NULL,
                         lockIn = NULL, typeLockIn = "Full", nameLockIn = NULL,
                         alphaLockIn = 1, colorLockIn = "black", legendLockIn = "",
                         labelLockIn = "MPAs",
                         lockOut = NULL, typeLockOut = "Full", nameLockOut = NULL,
                         alphaLockOut = 1, colorLockOut = "black", legendLockOut = "",
                         labelLockOut = "",
                         ggtheme = "Default"
) {

  # TODO Remove all uneeded arguments, especially the lockIn

  # TODO Update the asserts for new arguments
  # # Assertions to validate input parameters are of the correct 'sf' class if not NULL.
  # if(!is.null(PUs)){assertthat::assert_that(inherits(PUs, "sf"), msg = "'PUs' must be an 'sf' object or NULL.")}
  # if(!is.null(Bndry)){assertthat::assert_that(inherits(Bndry, "sf"), msg = "'Bndry' must be an 'sf' object or NULL.")}
  # if(!is.null(overlay)){assertthat::assert_that(inherits(overlay, "sf"), msg = "'overlay' must be an 'sf' object or NULL.")}
  # if(!is.null(overlay2)){assertthat::assert_that(inherits(overlay2, "sf"), msg = "'overlay2' must be an 'sf' object or NULL.")}
  # if(!is.null(overlay3)){assertthat::assert_that(inherits(overlay3, "sf"), msg = "'overlay3' must be an 'sf' object or NULL.")}
  # if(!is.null(contours)){assertthat::assert_that(inherits(contours, "sf"), msg = "'contours' must be an 'sf' object or NULL.")}
  # if(!is.null(lockIn)){
  #   assertthat::assert_that(inherits(lockIn, "sf"), msg = "'lockIn' must be an 'sf' object or NULL.")
  #   assertthat::assert_that(is.character(nameLockIn) && all(nameLockIn %in% names(lockIn)),
  #                           msg = "If 'lockIn' is provided, 'nameLockIn' must be a character string specifying an existing column in 'lockIn'.")
  #   assertthat::assert_that(typeLockIn %in% c("Full", "Contours"),
  #                           msg = "'typeLockIn' must be either 'Full' or 'Contours'.")
  #   assertthat::assert_that(is.numeric(alphaLockIn) && alphaLockIn >= 0 && alphaLockIn <= 1,
  #                           msg = "'alphaLockIn' must be a numeric value between 0 and 1.")
  # }
  # if(!is.null(cropOverlay)){assertthat::assert_that(inherits(cropOverlay, "sf"), msg = "'cropOverlay' must be an 'sf' object or NULL.")}
  # assertthat::assert_that(is.character(colorPUs), msg = "'colorPUs' must be a character string for a color.")
  # assertthat::assert_that(is.character(colorBndry), msg = "'colorBndry' must be a character string for a color.")
  # assertthat::assert_that(is.character(colorOverlay), msg = "'colorOverlay' must be a character string for a color.")
  # assertthat::assert_that(is.character(colorOverlay2), msg = "'colorOverlay2' must be a character string for a color.")
  # assertthat::assert_that(is.character(colorOverlay3), msg = "'colorOverlay3' must be a character string for a color.")
  # assertthat::assert_that(is.character(colorConts), msg = "'colorConts' must be a character string for a color.")
  # assertthat::assert_that(is.character(colorLockIn), msg = "'colorLockIn' must be a character string for a color.")
  # assertthat::assert_that(is.character(legendLockIn), msg = "'legendLockIn' must be a character string.")
  # assertthat::assert_that(is.character(labelLockIn), msg = "'labelLockIn' must be a character string.")
  # assertthat::assert_that(
  #   inherits(ggtheme, "character") || inherits(ggtheme, "theme") || inherits(ggtheme, "logical"),
  #   msg = "'ggtheme' must be 'Default', a ggplot2 theme, or NA/FALSE."
  # )

  # Initialize an empty list to store ggplot2 layers.
  ggList <- list()

  # Add planning units layer if PUs is an sf object.
  if (inherits(PUs, "sf")) {
    ggList <- c(
      ggList,
      list(
        ggplot2::geom_sf(data = PUs, colour = colorPUs, fill = NA, size = 0.1, show.legend = FALSE),
        ggplot2::coord_sf(xlim = sf::st_bbox(PUs)$xlim, ylim = sf::st_bbox(PUs)$ylim)
      )
    )
  }

  # Add boundary layer if Bndry is an sf object.
  if (inherits(Bndry, "sf")) {
    ggList <- c(
      ggList,
      list(
        ggplot2::geom_sf(data = Bndry, colour = colorBndry, size = 0.4, fill = NA, show.legend = FALSE),
        ggplot2::coord_sf(xlim = sf::st_bbox(Bndry)$xlim, ylim = sf::st_bbox(Bndry)$ylim)
      )
    )
  }

  # Add first overlay layer if 'overlay' is an sf object.
  if (inherits(overlay, "sf")) {
    ggList <- c(
      ggList,
      ggplot2::geom_sf(data = overlay, colour = colorOverlay, fill = colorOverlay, alpha = 0.9, size = 0.1, show.legend = FALSE)
    )}

  # Add second overlay layer if 'overlay2' is an sf object.
  if (inherits(overlay2, "sf")) {
    ggList <- c(
      ggList,
      ggplot2::geom_sf(data = overlay2, colour = colorOverlay2, fill = colorOverlay2, alpha = 0.9, size = 0.1, show.legend = FALSE)
    )}

  # Add third overlay layer if 'overlay3' is an sf object.
  if (inherits(overlay3, "sf")) {
    ggList <- c(
      ggList,
      ggplot2::geom_sf(data = overlay3, colour = colorOverlay3, fill = colorOverlay3, alpha = 0.9, size = 0.1, show.legend = FALSE)
    )}

  # Add contours layer if 'contours' is an sf object.
  if (inherits(contours, "sf")) {
    # Get unique contour categories for legend.
    nameConts <- unique(contours$Category)
    contoursRowNum <- length(nameConts)
    vals <- 1:contoursRowNum
    # Warn if more than 6 categories are provided for contours.
    if (length(vals) > 6) {
      cat("Only 6 categories allowed for plotting contours.")
    } else {
      # Add contour layers with new scale for color and linetype.
      ggList <- c(
        ggList,
        list(
          ggnewscale::new_scale_colour(), # Start a new color scale for contours.
          ggplot2::geom_sf(data = contours, colour = colorConts, fill = NA, ggplot2::aes(linetype = .data$Category), size = 0.5, show.legend = "line"),
          ggplot2::scale_linetype_manual(" ", # Set linetype based on contour categories.
                                         breaks = nameConts,
                                         values = vals,
                                         guide = ggplot2::guide_legend(
                                           override.aes = list(fill = NA),
                                           nrow = 2,
                                           direction = "horizontal",
                                           order = 3,
                                           keywidth = grid::unit(0.05, "npc")
                                         )
          )
        )
      )
    }
  }


  #TODO Consider adding locked in to the selected/not selected solution column so it plots as one.
  # Add locked-in areas layer if 'lockIn' is an sf object.
  if (inherits(lockIn, "sf")) {

    # Mutate the 'lockIn' data to create a 'lockedIn' logical column based on 'nameLockIn', then filter.
    lockIn <- lockIn %>%
      dplyr::select(tidyselect::all_of(c(nameLockIn, "geometry"))) %>%
      tidyr::pivot_longer(cols = tidyselect::all_of(c(nameLockIn)), names_to = "LI_Area", values_to = "LockedIn") %>%
      dplyr::mutate(lockedIn = as.logical(LockedIn),
                    LI_Area = stringr::str_to_title(LI_Area)) %>%
      dplyr::filter(.data$lockedIn == TRUE) # Filter for TRUE values in the 'lockedIn' column.

    # Plot locked-in areas as 'Full' polygons.
    if (typeLockIn == "Full") {
      ggList <- c(
        ggList,
        list(
          ggnewscale::new_scale_fill(), # Start a new fill scale.
          ggnewscale::new_scale_colour(), # Start a new color scale.
          ggplot2::geom_sf(data = lockIn, ggplot2::aes(fill = .data$LI_Area), alpha = alphaLockIn),
          ggplot2::scale_fill_brewer(
            palette = "Greens",
            name = legendLockIn, # Set legend title.
            # values = c("TRUE" = colorLockIn), # Map TRUE to specified color.
            # labels = labelLockIn, # Set legend label.
            # Apply color and fill aesthetics to this scale.
            aesthetics = c("colour", "fill"),
            # Configure legend appearance.
            guide = ggplot2::guide_legend(
              override.aes = list(linetype = 0), # Remove linetype from legend.
              nrow = 2,
              order = 1,
              direction = "horizontal",
              title.position = "top",
              title.hjust = 0.5
            )
          )
        )
      )
    } else if (typeLockIn == "Contours") { # Plot locked-in areas as 'Contours' (outlines).
      # Union geometries to create a single outline for locked-in areas.
      lockIn <- lockIn %>%
        sf::st_union() %>%
        sf::st_as_sf() %>%
        dplyr::rename(geometry = "x") %>%
        dplyr::mutate(lockedIn = 1) %>%
        dplyr::mutate(lockedIn = as.factor(.data$lockedIn))

      # Add contour layers with new scale for color and linetype.
      ggList <- c(
        ggList,
        list(
          ggnewscale::new_scale_fill(), # Start a new fill scale.
          ggnewscale::new_scale_colour(), # Start a new color scale.
          ggplot2::geom_sf(data = lockIn, colour = colorLockIn, fill = NA, ggplot2::aes(linetype = .data$lockedIn), size = 0.5, show.legend = "line"),
          ggplot2::scale_linetype_manual("",
                                         values = 1, # Use a single linetype for contours.
                                         labels = labelLockIn, # Set legend label.
                                         guide = ggplot2::guide_legend(
                                           override.aes = list(fill = NA), # Remove fill from legend.
                                           direction = "horizontal",
                                           keywidth = grid::unit(0.05, "npc")
                                         )
          )
        )
      )
    }
  }


  ## Lock Out ---------
  if (inherits(lockOut, "sf")) {

    # Mutate the 'lockOut' data to create a 'lockedOut' logical column based on 'nameLockOut', then filter.
    lockOut <- lockOut %>%
      dplyr::select(tidyselect::all_of(c(nameLockOut, "geometry"))) %>%
      tidyr::pivot_longer(cols = tidyselect::all_of(c(nameLockOut)), names_to = "LI_Area", values_to = "LockedOut") %>%
      dplyr::mutate(lockedOut = as.logical(LockedOut),
                    LI_Area = stringr::str_to_title(LI_Area)) %>%
      dplyr::filter(.data$lockedOut == TRUE) # Filter for TRUE values in the 'lockedOut' column.

    # Plot locked-in areas as 'Full' polygons.
    if (typeLockOut == "Full") {
      ggList <- c(
        ggList,
        list(
          ggnewscale::new_scale_fill(), # Start a new fill scale.
          ggnewscale::new_scale_colour(), # Start a new color scale.
          ggplot2::geom_sf(data = lockOut, ggplot2::aes(fill = .data$LI_Area), alpha = alphaLockOut),
          ggplot2::scale_fill_brewer(
            palette = "Reds",
            name = legendLockOut, # Set legend title.
            # Apply color and fill aesthetics to this scale.
            aesthetics = c("colour", "fill"),
            # Configure legend appearance.
            guide = ggplot2::guide_legend(
              override.aes = list(linetype = 0), # Remove linetype from legend.
              nrow = 2,
              order = 1,
              direction = "horizontal",
              title.position = "top",
              title.hjust = 0.5
            )
          )
        )
      )
    }
  }



  # Apply coordinate limits based on 'cropOverlay' if provided.
  if (inherits(cropOverlay, "sf")) {
    ggList <- c(
      ggList,
      ggplot2::coord_sf(xlim = sf::st_bbox(cropOverlay)$xlim, ylim = sf::st_bbox(cropOverlay)$ylim)
    )
  }


  # Apply the specified ggplot2 theme.
  if (inherits(ggtheme, "character") && ggtheme == "Default") {
    # Apply the default spatialplanr theme.
    ggList <- c(
      ggList,
      list(
        ggplot2::theme_bw(),  # Black and white theme.
        ggplot2::theme(
          legend.position = "bottom", # Legend at the bottom.
          legend.direction = "horizontal", # Horizontal legend.
          text = ggplot2::element_text(size = 20, colour = "black"), # Global text size and color.
          axis.text = ggplot2::element_text(size = 16, colour = "black"), # Axis text size and color.
          plot.title = ggplot2::element_text(size = 16), # Plot title size.
          axis.title = ggplot2::element_blank() # Remove axis titles.
        )
      )
    )


  } else if (inherits(ggtheme, "theme")) {
    # If a theme object is provided, append it.
    ggList <- c(ggList, list(ggtheme))

  } else if (inherits(ggtheme, "list")) {
    # If a list of theme elements is provided, append them.
    ggList <- c(ggList, ggtheme)

  } else if (inherits(ggtheme, "logical") && !ggtheme) {
    # If ggtheme is FALSE or NA, do nothing (no default theme applied).
    ggList <- ggList
  }

  # browser()

  return(ggList)
}
