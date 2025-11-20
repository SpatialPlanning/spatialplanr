#' @title Add-ons for Plotting `spatialplanr` Solution Maps
#'
#' @description
#' This function allows users to customize existing `ggplot2` solution maps produced
#' by `spatialplanr` spatial plotting functions (e.g., `splnr_plot_solution()`). It
#' provides options to add various spatial layers and apply consistent theming in a
#' simple and reproducible manner.
#'
#' @details
#' The `splnr_gg_add()` function enhances `ggplot2` objects by layering additional
#' spatial data such as Planning Unit outlines, study area boundaries, general
#' overlays, geographical contours, locked-in areas (e.g., existing Marine Protected
#' Areas (MPAs) that must be included in a conservation prioritization), and
#' locked-out areas (e.g., areas that must be excluded from selection such as
#' shipping lanes or oil and gas leases). It offers fine-grained control over
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
#' @param PUs An `sf` object representing Planning Units. If provided, their
#'   outlines will be drawn. Defaults to `NULL`.
#' @param colorPUs A character string specifying the color for the outlines of the
#'   Planning Units. Defaults to `"grey80"`.
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
#'   `xlim` and `ylim` of the `ggplot2::coord_sf()` layer, effectively cropping the view.
#'   Defaults to `NULL`.
#' @param contours An `sf` object containing contour lines (e.g., bathymetry or
#'   seamount outlines). It is expected to have a `Category` column for differentiating
#'   lines. Up to 6 categories are supported. Defaults to `NULL`.
#' @param colorConts A character string specifying the color for the contour lines.
#'   Defaults to `"black"`.
#' @param lockIn An `sf` object representing locked-in areas (e.g., existing
#'   Marine Protected Areas (MPAs)) that are fixed in a conservation prioritization.
#'   Defaults to `NULL`.
#' @param typeLockIn A character string specifying how `lockIn` areas should be
#'   plotted. Can be `"Full"` (fills the areas with `colorLockIn`) or `"Contours"`
#'   (draws only the outlines of the areas). Defaults to `"Full"`.
#' @param nameLockIn A character string specifying the column name in the `lockIn`
#'   data frame that contains binary (0/1 or TRUE/FALSE) information indicating
#'   locked-in status. Required if `lockIn` is not `NULL`.
#' @param alphaLockIn A numeric value (0 to 1) for the opacity of the `lockIn`
#'   areas when `typeLockIn` is `"Full"`. Defaults to `1`.
#' @param colorLockIn A character string specifying the color for the `lockIn` areas.
#'   Defaults to `"black"`.
#' @param legendLockIn A character string for the title of the `lockIn` legend.
#'   Can be an empty string `""` to suppress the title. Defaults to `""`.
#' @param labelLockIn A character string for the legend label of the `lockIn` areas
#'   (e.g., "MPAs"). Defaults to `"MPAs"`.
#' @param lockOut An `sf` object representing locked-out areas (e.g., shipping lanes,
#'   oil and gas leases, or other excluded zones) that must not be selected in a
#'   conservation prioritization. Defaults to `NULL`.
#' @param typeLockOut A character string specifying how `lockOut` areas should be
#'   plotted. Can be `"Full"` (fills the areas with `colorLockOut`) or `"Contours"`
#'   (draws only the outlines of the areas). Defaults to `"Full"`.
#' @param nameLockOut A character string specifying the column name in the `lockOut`
#'   data frame that contains binary (0/1 or TRUE/FALSE) information indicating
#'   locked-out status. Required if `lockOut` is not `NULL`.
#' @param alphaLockOut A numeric value (0 to 1) for the opacity of the `lockOut`
#'   areas when `typeLockOut` is `"Full"`. Defaults to `1`.
#' @param colorLockOut A character string specifying the color for the `lockOut` areas.
#'   Defaults to `"black"`.
#' @param legendLockOut A character string for the title of the `lockOut` legend.
#'   Can be an empty string `""` to suppress the title. Defaults to `""`.
#' @param labelLockOut A character string for the legend label of the `lockOut` areas
#'   (e.g., "Shipping Lanes"). Defaults to `""`.
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
#' # in your package, suitable for prioritization problems and plotting.
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
#' # Basic plot of the solution with default Planning Unit outlines and theme.
#' plot_basic <- splnr_plot_solution(dat_soln) +
#'   splnr_gg_add(PUs = dat_PUs, ggtheme = "Default")
#' print(plot_basic)
#'
#' # Example with boundary, a custom overlay, and locked-in areas shown as contours.
#' # For this example, let's create dummy `bndry_sf` and `locked_in_sf` based on `dat_PUs`.
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
                         ggtheme = "Default") {

  # TODO Remove all uneeded arguments, especially the lockIn

  # Assertions to validate input parameters are of the correct 'sf' class if not NULL.
  if(!is.null(PUs)){assertthat::assert_that(inherits(PUs, "sf"), msg = "'PUs' must be an 'sf' object or NULL.")}
  if(!is.null(Bndry)){assertthat::assert_that(inherits(Bndry, "sf"), msg = "'Bndry' must be an 'sf' object or NULL.")}
  if(!is.null(overlay)){assertthat::assert_that(inherits(overlay, "sf"), msg = "'overlay' must be an 'sf' object or NULL.")}
  if(!is.null(overlay2)){assertthat::assert_that(inherits(overlay2, "sf"), msg = "'overlay2' must be an 'sf' object or NULL.")}
  if(!is.null(overlay3)){assertthat::assert_that(inherits(overlay3, "sf"), msg = "'overlay3' must be an 'sf' object or NULL.")}
  if(!is.null(contours)){assertthat::assert_that(inherits(contours, "sf"), msg = "'contours' must be an 'sf' object or NULL.")}
  if(!is.null(cropOverlay)){assertthat::assert_that(inherits(cropOverlay, "sf"), msg = "'cropOverlay' must be an 'sf' object or NULL.")}
  
  # Validate lockIn parameters
  if(!is.null(lockIn)){
    assertthat::assert_that(inherits(lockIn, "sf"), msg = "'lockIn' must be an 'sf' object or NULL.")
    assertthat::assert_that(is.character(nameLockIn) && !is.null(nameLockIn) && all(nameLockIn %in% names(lockIn)),
                            msg = "If 'lockIn' is provided, 'nameLockIn' must be a character string specifying an existing column in 'lockIn'.")
    assertthat::assert_that(typeLockIn %in% c("Full", "Contours"),
                            msg = "'typeLockIn' must be either 'Full' or 'Contours'.")
    assertthat::assert_that(is.numeric(alphaLockIn) && alphaLockIn >= 0 && alphaLockIn <= 1,
                            msg = "'alphaLockIn' must be a numeric value between 0 and 1.")
  }
  
  # Validate lockOut parameters
  if(!is.null(lockOut)){
    assertthat::assert_that(inherits(lockOut, "sf"), msg = "'lockOut' must be an 'sf' object or NULL.")
    assertthat::assert_that(is.character(nameLockOut) && !is.null(nameLockOut) && all(nameLockOut %in% names(lockOut)),
                            msg = "If 'lockOut' is provided, 'nameLockOut' must be a character string specifying an existing column in 'lockOut'.")
    assertthat::assert_that(typeLockOut %in% c("Full", "Contours"),
                            msg = "'typeLockOut' must be either 'Full' or 'Contours'.")
    assertthat::assert_that(is.numeric(alphaLockOut) && alphaLockOut >= 0 && alphaLockOut <= 1,
                            msg = "'alphaLockOut' must be a numeric value between 0 and 1.")
  }
  
  # Validate color parameters
  assertthat::assert_that(is.character(colorPUs), msg = "'colorPUs' must be a character string for a color.")
  assertthat::assert_that(is.character(colorBndry), msg = "'colorBndry' must be a character string for a color.")
  assertthat::assert_that(is.character(colorOverlay), msg = "'colorOverlay' must be a character string for a color.")
  assertthat::assert_that(is.character(colorOverlay2), msg = "'colorOverlay2' must be a character string for a color.")
  assertthat::assert_that(is.character(colorOverlay3), msg = "'colorOverlay3' must be a character string for a color.")
  assertthat::assert_that(is.character(colorConts), msg = "'colorConts' must be a character string for a color.")
  assertthat::assert_that(is.character(colorLockIn), msg = "'colorLockIn' must be a character string for a color.")
  assertthat::assert_that(is.character(colorLockOut), msg = "'colorLockOut' must be a character string for a color.")
  
  # Validate legend and label parameters
  assertthat::assert_that(is.character(legendLockIn), msg = "'legendLockIn' must be a character string.")
  assertthat::assert_that(is.character(labelLockIn), msg = "'labelLockIn' must be a character string.")
  assertthat::assert_that(is.character(legendLockOut), msg = "'legendLockOut' must be a character string.")
  assertthat::assert_that(is.character(labelLockOut), msg = "'labelLockOut' must be a character string.")
  
  # Validate ggtheme parameter
  assertthat::assert_that(
    inherits(ggtheme, "character") || inherits(ggtheme, "theme") || inherits(ggtheme, "list") || inherits(ggtheme, "logical"),
    msg = "'ggtheme' must be 'Default', a ggplot2 theme, a list of theme elements, or NA/FALSE."
  )

  # Initialize an empty list to store ggplot2 layers.
  ggList <- list()

  # Planning units (no legend)
  if (inherits(PUs, "sf")) {
    ggList <- c(
      ggList,
      list(
        ggplot2::geom_sf(data = PUs, colour = colorPUs, fill = NA, size = 0.1, show.legend = FALSE),
        ggplot2::coord_sf(xlim = sf::st_bbox(PUs)$xlim, ylim = sf::st_bbox(PUs)$ylim)
      )
    )
  }

  # Boundary (no legend)
  if (inherits(Bndry, "sf")) {
    ggList <- c(
      ggList,
      list(
        ggplot2::geom_sf(data = Bndry, colour = colorBndry, size = 0.4, fill = NA, show.legend = FALSE),
        ggplot2::coord_sf(xlim = sf::st_bbox(Bndry)$xlim, ylim = sf::st_bbox(Bndry)$ylim)
      )
    )
  }

  # Overlays (no legend)
  if (inherits(overlay, "sf")) {
    ggList <- c(
      ggList,
      ggplot2::geom_sf(data = overlay, colour = colorOverlay, fill = colorOverlay, alpha = 0.9, size = 0.1, show.legend = FALSE)
    )
  }
  if (inherits(overlay2, "sf")) {
    ggList <- c(
      ggList,
      ggplot2::geom_sf(data = overlay2, colour = colorOverlay2, fill = colorOverlay2, alpha = 0.9, size = 0.1, show.legend = FALSE)
    )
  }
  if (inherits(overlay3, "sf")) {
    ggList <- c(
      ggList,
      ggplot2::geom_sf(data = overlay3, colour = colorOverlay3, fill = colorOverlay3, alpha = 0.9, size = 0.1, show.legend = FALSE)
    )
  }

  # Contours (linetype legend, force nrow = 2)
  if (inherits(contours, "sf")) {
    nameConts <- unique(contours$Category)
    vals <- seq_along(nameConts)
    if (length(vals) > 6) {
      warning("Only 6 contour categories are supported; extra categories will share types.")
    }
    ggList <- c(
      ggList,
      list(
        # linetype scale only; no new fill scale needed
        ggplot2::geom_sf(
          data = contours,
          ggplot2::aes(linetype = .data$Category),
          colour = colorConts, fill = NA, size = 0.5, show.legend = TRUE
        ),
        ggplot2::scale_linetype_manual(
          name = " ",
          breaks = nameConts,
          values = vals,
          guide = ggplot2::guide_legend(
            override.aes = list(fill = NA, colour = colorConts),
            nrow = 2, byrow = TRUE,
            direction = "horizontal",
            order = 3,
            title.position = "top",
            title.hjust = 0.5,
            keywidth = grid::unit(0.05, "npc")
          )
        )
      )
    )
  }

  # Lock-in (Full) — fill legend, force nrow = 2, do NOT couple to colour
  if (inherits(lockIn, "sf")) {
    li <- lockIn %>%
      dplyr::select(tidyselect::all_of(c(nameLockIn, "geometry"))) %>%
      tidyr::pivot_longer(
        cols = tidyselect::all_of(c(nameLockIn)),
        names_to = "LI_Area", values_to = "LockedIn"
      ) %>%
      dplyr::mutate(
        lockedIn = as.logical(.data$LockedIn),
        LI_Area = ifelse(stringr::str_to_title(.data$LI_Area) == "Mpas", "MPAs", stringr::str_to_title(.data$LI_Area))
      ) %>%
      dplyr::filter(.data$lockedIn)

    if (nrow(li) > 0) {
      if (identical(typeLockIn, "Full")) {
        ggList <- c(
          ggList,
          list(
            # Start a new fill scale so we don't collide with solution fill
            ggnewscale::new_scale_fill(),
            ggplot2::geom_sf(data = li, ggplot2::aes(fill = .data$LI_Area), alpha = alphaLockIn, colour = NA),
            ggplot2::scale_fill_brewer(
              palette = "Greens",
              name = legendLockIn,
              guide = ggplot2::guide_legend(
                override.aes = list(linetype = 0),
                nrow = 2, byrow = TRUE,
                direction = "horizontal",
                order = 1,
                title.position = "top",
                title.hjust = 0.5
              )
            )
          )
        )
      } else if (identical(typeLockIn, "Contours")) {
        li_ct <- li %>%
          sf::st_union() %>%
          sf::st_as_sf() %>%
          dplyr::rename(geometry = "x") %>%
          dplyr::mutate(lockedIn = factor(1L))

        ggList <- c(
          ggList,
          list(
            # linetype only; no new fill/colour scales needed
            ggplot2::geom_sf(
              data = li_ct,
              ggplot2::aes(linetype = .data$lockedIn),
              colour = colorLockIn, fill = NA, size = 0.5, show.legend = TRUE
            ),
            ggplot2::scale_linetype_manual(
              name = "",
              values = 1,
              labels = labelLockIn,
              guide = ggplot2::guide_legend(
                override.aes = list(fill = NA, colour = colorLockIn),
                nrow = 2, byrow = TRUE,
                direction = "horizontal",
                order = 2,
                title.position = "top",
                title.hjust = 0.5,
                keywidth = grid::unit(0.05, "npc")
              )
            )
          )
        )
      }
    }
  }

  # Lock-out (Full) — fill legend, force nrow = 2, do NOT couple to colour
  if (inherits(lockOut, "sf")) {
    lo <- lockOut %>%
      dplyr::select(tidyselect::all_of(c(nameLockOut, "geometry"))) %>%
      tidyr::pivot_longer(
        cols = tidyselect::all_of(c(nameLockOut)),
        names_to = "LI_Area", values_to = "LockedOut"
      ) %>%
      dplyr::mutate(
        lockedOut = as.logical(.data$LockedOut),
        LI_Area = ifelse(stringr::str_to_title(.data$LI_Area) == "Mpas", "MPAs", stringr::str_to_title(.data$LI_Area))
      ) %>%
      dplyr::filter(.data$lockedOut)

    if (nrow(lo) > 0) {
      if (identical(typeLockOut, "Full")) {
        ggList <- c(
          ggList,
          list(
            ggnewscale::new_scale_fill(),
            ggplot2::geom_sf(data = lo, ggplot2::aes(fill = .data$LI_Area), alpha = alphaLockOut, colour = NA),
            ggplot2::scale_fill_brewer(
              palette = "Reds",
              name = legendLockOut,
              guide = ggplot2::guide_legend(
                override.aes = list(linetype = 0),
                nrow = 2, byrow = TRUE,
                direction = "horizontal",
                order = 1,
                title.position = "top",
                title.hjust = 0.5
              )
            )
          )
        )
      }
    }
  }

  # Crop extents if provided
  if (inherits(cropOverlay, "sf")) {
    ggList <- c(
      ggList,
      ggplot2::coord_sf(xlim = sf::st_bbox(cropOverlay)$xlim, ylim = sf::st_bbox(cropOverlay)$ylim)
    )
  }

  # Theme block
  if (is.character(ggtheme) && ggtheme == "Default") {
    ggList <- c(
      ggList,
      list(
        ggplot2::theme_bw(),
        ggplot2::theme(
          legend.position = "bottom",
          legend.direction = "horizontal",
          text = ggplot2::element_text(size = 20, colour = "black"),
          axis.text = ggplot2::element_text(size = 16, colour = "black"),
          plot.title = ggplot2::element_text(size = 16),
          axis.title = ggplot2::element_blank()
        )
      )
    )
  } else if (inherits(ggtheme, "theme")) {
    ggList <- c(ggList, list(ggtheme))
  } else if (inherits(ggtheme, "list")) {
    ggList <- c(ggList, ggtheme)
  }

  ggList
}
