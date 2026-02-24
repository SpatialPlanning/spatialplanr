# Create a Planning Region Boundary

This function generates a spatial boundary for the planning region as an
`sf` polygon object. The boundary can be defined in several ways:

1.  A simple rectangular bounding box using numeric coordinates.

2.  A global boundary spanning the entire world.

3.  A complex shape based on marine ecoregions from `rnaturalearth`.

## Usage

``` r
splnr_get_boundary(Limits, Type = NULL, res = 1, cCRS = "ESRI:54009")
```

## Arguments

- Limits:

  A required input that defines the spatial extent. This can be:

  - A named numeric vector of four elements:
    `c("xmin" = ..., "xmax" = ..., "ymin" = ..., "ymax" = ...)`.

  - The string `"Global"` to create a worldwide boundary.

  - A character vector of ocean/sea names (e.g.,
    `"North Atlantic Ocean"`) to be used with `Type = "Ocean"`.

- Type:

  **\[deprecated\]** The type of Limits being provided. This is only
  required if `Limits` is a character vector of ocean names, in which
  case it should be `"Ocean"`. It is no longer required and will be
  removed in a future version.

- res:

  `[numeric(1)]`\
  The resolution (in decimal degrees) used to construct the polygon
  vertices when `Limits` is numeric or `"Global"`. Defaults to `1`. Must
  be a positive number.

- cCRS:

  `[character(1)]`\
  The coordinate reference system (CRS) for the output `sf` object. Can
  be a PROJ4 string or an EPSG code. Defaults to `"ESRI:54009"`
  (Mollweide).

## Value

An `sf` object containing a single polygon feature representing the
planning boundary.

## Details

A planning region boundary is the foundational first step for most
spatial conservation planning exercises. All subsequent analyses and
data preparation steps within the `spatialplanr` package rely on a
defined boundary. The coordinate reference system (CRS) of the returned
object is projected by default (Mollweide), which is suitable for
equal-area calculations.

## Examples

``` r
if (FALSE) { # \dontrun{
# Example 1: Create a boundary from an ocean name.
# This fetches polygon data for the specified ocean.
bndry_ocean <- splnr_get_boundary(Limits = "North Atlantic Ocean", Type = "Ocean")
plot(bndry_ocean)

# Example 2: Create a global boundary.
bndry_global <- splnr_get_boundary(Limits = "Global")
plot(bndry_global)

# Example 3: Create a boundary from a numeric bounding box.
bndry_coords <- splnr_get_boundary(
  Limits = c("xmin" = 150, "xmax" = 170, "ymin" = -40, "ymax" = -20)
)
plot(bndry_coords)
} # }
```
