# Calculate Distance to Coastline

This function calculates the shortest distance from the centroid of each
Planning Unit in an `sf` object to the nearest coastline. It can use
either a default coastline from the `rnaturalearth` package or a
custom-provided coastline `sf` object.

## Usage

``` r
splnr_get_distCoast(dat_sf, custom_coast = NULL, res = "medium")
```

## Arguments

- dat_sf:

  `[sf]`\
  An `sf` object containing polygon or point features representing the
  Planning Units. Must have a valid CRS.

- custom_coast:

  `[sf]`\
  An optional `sf` object representing a custom coastline. If `NULL`
  (the default), the coastline is downloaded from `rnaturalearth`.

- res:

  `[character(1)]`\
  The resolution of the `rnaturalearth` coastline to use. Options are
  `"small"`, `"medium"` (default), or `"large"`. This parameter is
  ignored if `custom_coast` is provided.

## Value

An `sf` object identical to `dat_sf` but with an added column
`coastDistance_km` representing the distance to the nearest coastline in
kilometers.

## Details

The function adds a new column named `coastDistance_km` to the input
`sf` object, containing the calculated distances in kilometers. The CRS
of the input data is preserved. It is crucial to ensure the input `sf`
object has a suitable projected CRS for accurate distance calculations.

## Examples

``` r
if (FALSE) { # \dontrun{
# Example 1: Calculate distance to coast for a simple grid
bbox <- sf::st_bbox(c(xmin = 0, ymin = 0, xmax = 3, ymax = 3))
grid <- sf::st_as_sf(sf::st_make_grid(bbox, n = c(3, 3)))
grid_with_dist <- splnr_get_distCoast(grid)
plot(grid_with_dist["coastDistance_km"])

# Example 2: Using a specific resolution for the coastline
# Note: Requires the 'dat_sf' object to be created first, e.g., using
# splnr_get_planning_units()
if (exists("dat_sf")) {
  dat_sf_dist <- splnr_get_distCoast(dat_sf, res = "large")
  summary(dat_sf_dist$coastDistance_km)
}

# Example 3: Using a custom coastline
# First, create a custom coastline (e.g., from a country polygon)
landmass <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
)

if (exists("dat_sf") && exists("landmass")) {
   # Transform landmass to the same CRS as the planning units
  landmass_proj <- sf::st_transform(landmass, sf::st_crs(dat_sf))
  dat_sf_custom_coast <- splnr_get_distCoast(dat_sf, custom_coast = landmass_proj)
  summary(dat_sf_custom_coast$coastDistance_km)
}
} # }
```
