# Create Spatial Polygon from Coordinates

`splnr_create_polygon()` constructs an `sf` polygon object from a series
of longitude and latitude coordinates provided in a tibble.

## Usage

``` r
splnr_create_polygon(x, cCRS = "EPSG:4326")
```

## Arguments

- x:

  A `tibble` (or `tbl_df`) object with at least two columns, typically
  named `x` (for longitude) and `y` (for latitude), representing the
  vertices of the polygon in sequence. The first and last coordinate
  pair should be the same to form a closed polygon.

- cCRS:

  A character string specifying the target CRS for the output polygon in
  an EPSG code format (e.g., "EPSG:4326"). Defaults to "EPSG:4326" (WGS
  84).

## Value

An `sf` object representing the created polygon, with the specified CRS.

## Details

This utility function simplifies the creation of spatial polygons from a
tabular format of coordinates. It takes a tibble where columns 'x' and
'y' represent longitude and latitude, respectively. These coordinates
are converted into a matrix, then to an `sf` polygon, and finally to an
`sf` object with the specified Coordinate Reference System (CRS).

The function assumes that the input coordinates (`x`) are initially in
WGS 84 (EPSG:4326) and then transforms them to the `cCRS` if a different
CRS is specified.

## Examples

``` r
# Example: Create a simple square polygon
square_coords <- dplyr::tibble(
  x = c(-50, 50, 50, -50, -50),
  y = c(120, 120, 180, 180, 120)
)
simple_polygon <- splnr_create_polygon(x = square_coords)
print(simple_polygon)
#> Simple feature collection with 1 feature and 0 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -50 ymin: 120 xmax: 50 ymax: 180
#> Geodetic CRS:  WGS 84
#>                         geometry
#> 1 POLYGON ((-50 120, 50 120, ...

# Example: Create a polygon and transform to a different CRS (e.g., a UTM zone)
if (FALSE) { # \dontrun{
# Note: EPSG:32611 is UTM Zone 11N. Ensure it's appropriate for your coordinates.
transformed_polygon <- splnr_create_polygon(x = square_coords, cCRS = "EPSG:32611")
print(transformed_polygon)
} # }
```
