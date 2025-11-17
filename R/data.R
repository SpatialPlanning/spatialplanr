#' Planning Units
#'
#' An sf dataframe of Planning Units for spatial prioritization
#'
#' @format `dat_PUs`
#' A data frame with XXXX rows and XXX columns:
#' \describe{
#'   \item{geometry}{sf geometry column}
#'   ...
#' }
#' @source "Made up data"
"dat_PUs"


# -------------------------------------------------------------------------

#' Boundary of Planning Units
#'
#' An sf dataframe of Planning Units for spatial prioritization
#'
#' @format `dat_bndry`
#' A data frame with XXXX rows and XXX columns:
#' \describe{
#'   \item{geometry}{sf geometry column}
#'   ...
#' }
#' @source "Made up data"
"dat_bndry"

# -------------------------------------------------------------------------


#' Regionalisation
#'
#' An sf dataframe of Planning Units for spatial prioritization
#'
#' @format `dat_region`
#' A data frame with XXXX rows and XXX columns:
#' \describe{
#'   \item{geometry}{sf geometry column}
#'   ...
#' }
#' @source "Made up data"
"dat_region"

# -------------------------------------------------------------------------

#' Binary Species Data
#'
#' An sf dataframe of Planning Units for spatial prioritization
#'
#' @format `dat_species_bin`
#' A data frame with XXXX rows and XXX columns:
#' \describe{
#'   \item{geometry}{sf geometry column}
#'   ...
#' }
#' @source "Made up data"
"dat_species_bin"


# -------------------------------------------------------------------------

#' Binary Species Data 2
#'
#' An sf dataframe of Planning Units for spatial prioritization
#'
#' @format `dat_species_bin2`
#' A data frame with XXXX rows and XXX columns:
#' \describe{
#'   \item{geometry}{sf geometry column}
#'   ...
#' }
#' @source "Made up data"
"dat_species_bin2"


# -------------------------------------------------------------------------

#' Probability Species Data
#'
#' An sf dataframe of Planning Units for spatial prioritization
#'
#' @format `dat_species_prob`
#' A data frame with XXXX rows and XXX columns:
#' \describe{
#'   \item{geometry}{sf geometry column}
#'   ...
#' }
#' @source "Made up data"
"dat_species_prob"


# -------------------------------------------------------------------------

#' Marine Protected Areas (MPAs) Data
#'
#' An sf dataframe of Planning Units for spatial prioritization
#'
#' @format `dat_mpas`
#' A data frame with XXXX rows and XXX columns:
#' \describe{
#'   \item{geometry}{sf geometry column}
#'   \item{wdpa}{binary MPA information (1: MPA)}
#'   ...
#' }
#' @source "Made up data"
"dat_mpas"

# -------------------------------------------------------------------------


#' Category vector
#'
#' A tibble containing specific grouping of features
#'
#' @format `dat_category`
#' A data frame with XXXX rows and XXX columns:
#' \describe{
#'   \item{feature}{all features available for spatial prioritization}
#'   \item{category}{the categories of all features}
#'   ...
#' }
#' @source "Made up data"
"dat_category"


# -------------------------------------------------------------------------


#' Category vector
#'
#' A tibble containing specific grouping of features
#'
#' @format `dat_category2`
#' A data frame with XXXX rows and XXX columns:
#' \describe{
#'   \item{feature}{all features available for spatial prioritization}
#'   \item{category}{the categories of all features}
#'   ...
#' }
#' @source "Made up data"
"dat_category2"


# -------------------------------------------------------------------------


#' Climate metric data
#'
#' An sf dataframe of Planning Units for spatial prioritization
#'
#' @format `dat_clim`
#' A data frame with 780 rows and 3 columns:
#' \describe{
#'   \item{geometry}{sf geometry column}
#'   \item{metric}{climate metric information}
#'   ...
#' }
#' @source "Made up data"
"dat_clim"

# -------------------------------------------------------------------------

# Aquamaps species example data
#
# sf file containing the suitable habitat for a few marine species of the coral sea
#
# @format `spDataFiltered`
# A data frame with 397 rows and 17 columns:
# \describe{
#   \item{Chelonia_mydas}{suitable habitat for Chelonia mydas}
#   ...
# }
# @source Aquamaps.org
# "spDataFiltered"


# -------------------------------------------------------------------------


#' Current Marine Protected Areas (MPAs) of the Coral Sea
#'
#' sf file containing the current Marine Protected Areas (MPAs) of the coral sea
#'
#' @format `MPAsCoralSea`
#' A data frame with 397 rows and 17 columns:
#' \describe{
#'   \item{wdpa}{binary vector denoting presence or absence of a current MPA}
#'   ...
#' }
#' @source https://www.protectedplanet.net/en/thematic-areas/wdpa?tab=WDPA
"MPAsCoralSea"


# -------------------------------------------------------------------------


#' IUCN and Inverse Area based Targets for Aquamaps example species
#'
#' tbl_df file containing IUCN and IA targets of the selected species of the coral sea
#'
#' @format `IUCN_IA_Targets`
#' A data frame with 15 rows and 4 columns:
#' \describe{
#'   \item{Species}{Species names as found in the IUCN database}
#'   \item{Area km2}{total suitable habitat of a species}
#'   \item{Target}{Individual species-specific target}
#'   \item{IUCN_Category}{IUCN category of the species}
#'   ...
#' }
#' @source https://www.iucnredlist.org/
"IUCN_IA_Targets"


# -------------------------------------------------------------------------


#' Example climate velocity for the example region of the coral sea
#'
#' tsf file containing climate velocity (SSP5-8.5) of the coral sea
#'
#' @format `CoralSeaVelocity`
#' A data frame with 397 rows and 3 columns:
#' \describe{
#'   \item{vocMagg_transformed}{Climate velocity column}
#'   \item{geometry}{Geometry column of sf object}
#'   ...
#' }
#' @source https://www.iucnredlist.org/
"CoralSeaVelocity"


#' Example data
#'
#' Example data
#'
#' @format `spDataFiltered`
#' A data frame with sample species data
#' \describe{
#' \item{Species}{Species names}
#' }
#' @source "Made up data"
"spDataFiltered"


#' Bathymetry Data
#'
#' Bathymetry data downloaded via the oceandatr package
#'
#' @format `dat_bathy`
#' A data frame with bathymetry data
#' \describe{
#
#'   \item{bathymetry}{Bathymetry data}
#'   \item{geometry}{sf geometry column}
#'   ...
#' }
#' @source "oceandatr package"
"dat_bathy"
