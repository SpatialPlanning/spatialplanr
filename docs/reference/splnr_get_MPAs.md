# Get Marine Protected Areas (MPAs) from WDPA

This function serves as a wrapper for the `wdpar` package, facilitating
the retrieval of Marine Protected Areas (MPAs) from the World Database
on Protected Areas (WDPA) and intersecting them with provided planning
units. The result is an `sf` object indicating the area of planning
units covered by the selected marine protected areas.

## Usage

``` r
splnr_get_MPAs(
  PlanUnits,
  Countries,
  Status = c("Designated", "Established", "Inscribed"),
  Desig = c("National", "Regional", "International", "Not Applicable"),
  Category = c("Ia", "Ib", "II", "III", "IV"),
  ...
)
```

## Arguments

- PlanUnits:

  An `sf` object representing the planning units to be used for
  intersection. This object should have a valid CRS defined.

- Countries:

  A character vector specifying the countries for which to extract MPAs.
  To retrieve all global MPAs, use the value `"global"`. Country names
  should match those recognized by the WDPA database.

- Status:

  A character vector specifying the desired status of protected areas to
  include. Defaults to `c("Designated", "Established", "Inscribed")`.

- Desig:

  A character vector specifying the desired designation types of
  protected areas. Defaults to
  `c("National", "Regional", "International", "Not Applicable")`.

- Category:

  A character vector specifying the desired IUCN Protected Area
  Management Categories. Defaults to `c("Ia", "Ib", "II", "III", "IV")`.

- ...:

  Other arguments that are passed directly to the `wdpa_fetch()`
  function from the `wdpar` package (e.g., `verbose = TRUE`).

## Value

An `sf` object. This object contains the planning units, with an
additional `wdpa` column (set to 1) for areas that intersect with the
selected MPAs.

## Details

This function leverages the robust capabilities of the `wdpar` package
by Jeffrey O. Hanson to access and process WDPA data. It allows
filtering of MPAs based on country, status, designation type, and IUCN
category, and then spatially intersects these MPAs with your defined
planning units.

For a comprehensive understanding of the WDPA data fields:

- **Status**: Refers to the establishment, designation, or proposal
  status of a protected area at the time of data submission. Valid
  options include "Designated", "Established", "Inscribed", "Proposed",
  and "Adopted".

- **Desig** (Designation Type): Categorizes the legal or official
  designation of the protected area. Valid options include "National",
  "Regional", "International", and "Not Applicable".

- **Category** (IUCN Protected Area Management Categories): Represents
  the IUCN management categories for protected areas. Valid options
  include "Ia", "Ib", "II", "III", "IV", "V", "VI", "Not Reported", "Not
  Applicable", and "Not Assigned".

## Examples

``` r
if (FALSE) { # \dontrun{
# Assuming 'dat_PUs' is an existing sf object of planning units in your package.

# Example: Get MPAs for Australia and intersect with planning units.
dat_mpas <- splnr_get_MPAs(PlanUnits = dat_PUs, Countries = "Australia")

# Example: Get MPAs for multiple countries with specific status and categories.
dat_mpas_specific <- splnr_get_MPAs(
  PlanUnits = dat_PUs,
  Countries = c("Australia", "New Zealand"),
  Status = c("Designated", "Proposed"),
  Category = c("II", "IV")
)

# Example: Visualize the result using ggplot2.
# Assuming 'aust' is an sf object representing Australia's coastline,
# perhaps loaded from rnaturalearth::ne_countries.
aust <- rnaturalearth::ne_countries(country = "Australia", returnclass = "sf")

gg <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = dat_mpas, ggplot2::aes(fill = wdpa)) +
  ggplot2::geom_sf(data = aust, fill = "grey50") +
  ggplot2::labs(title = "Marine Protected Areas in Australia") +
  ggplot2::theme_minimal()
print(gg)
} # }
```
