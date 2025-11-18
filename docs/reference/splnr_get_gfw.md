# Retrieve Global Fishing Watch Data

The `splnr_get_gfw` function retrieves Global Fishing Watch (GFW) data
and returns it as an `sf` (simple features) object. This function allows
for flexible data queries based on geographical region, time range, and
desired spatial and temporal resolutions.

## Usage

``` r
splnr_get_gfw(
  region,
  start_date,
  end_date,
  temp_res,
  spat_res = "LOW",
  region_source = "EEZ",
  key = gfwr::gfw_auth(),
  cCRS = "EPSG:4326",
  compress = FALSE
)
```

## Arguments

- region:

  A character string specifying the name of the region (e.g., an EEZ
  name) or a numeric ID for the region, or an `sf` object if
  `region_source` is set to "USER_SHAPEFILE".

- start_date:

  The start date for data retrieval, expected in "%Y-%m-%d" format
  (e.g., "2021-01-01").

- end_date:

  The end date for data retrieval, expected in "%Y-%m-%d" format (e.g.,
  "2022-12-31").

- temp_res:

  The desired temporal resolution for the data. Must be one of: "DAILY",
  "MONTHLY", or "YEARLY".

- spat_res:

  The desired spatial resolution for the data. Must be one of: "LOW"
  (0.1 degree) or "HIGH" (0.01 degree). Defaults to "LOW".

- region_source:

  The source of the region definition. Must be one of: 'EEZ', 'MPA',
  'RFMO', or 'USER_SHAPEFILE'. Defaults to "EEZ".

- key:

  Your API token for the GFW API. If not provided, it attempts to
  authenticate using
  [`gfwr::gfw_auth()`](https://globalfishingwatch.github.io/gfwr/reference/gfw_auth.html).
  See the GlobalFishingWatch vignette for details on obtaining a key.

- cCRS:

  The Coordinate Reference System (CRS) to which the output `sf` object
  will be transformed. Defaults to "EPSG:4326".

- compress:

  A logical value. If `TRUE`, the data will be compressed (aggregated)
  by coordinates, summing fishing hours for each unique location. If
  `FALSE`, the raw data points are returned. Defaults to `FALSE`.

## Value

An `sf` object containing the requested GFW data. The structure of the
`sf` object will vary depending on the `compress` and `temp_res`
parameters.

## Details

The possibilities offered by this function are extensively explained in
[`vignette("GlobalFishingWatch")`](https://mathmarecol.github.io/spatialplanr/articles/GlobalFishingWatch.md).

This function shares many parameters with the `get_raster` function from
the `gfwr` package, with the addition of `cCRS` for specifying the
Coordinate Reference System of the output `sf` object.

Fishing activity data can be aggregated (`group_by`) by
"FLAGANDGEARTYPE" by default, combining flags and gear types.

**Notes:**

- Currently, the function is primarily designed for data within
  Exclusive Economic Zones (EEZs), but it can potentially be extended to
  specific Marine Protected Areas (MPAs) or RFMOs.

- Days specified in the `start_date` and `end_date` variables are
  inclusive in the data recovery.

## Examples

``` r
if (FALSE) { # \dontrun{
# Example: Retrieve yearly GFW data for Australia, transformed to a
# Mollweide projection (ESRI:54009) and compressed (aggregated) by location.
gfw_data <- splnr_get_gfw(
  region = 'Australia',
  start_date = "2021-01-01",
  end_date = "2022-12-31",
  temp_res = "YEARLY",
  cCRS = "ESRI:54009",
  compress = TRUE
)

# Example: Retrieve monthly GFW data for a specific EEZ ID,
# keeping individual time ranges and locations.
# Note: Replace 1000 with an actual EEZ ID if needed for testing.
gfw_data_monthly <- splnr_get_gfw(
  region = 1000, # Example numeric EEZ ID
  start_date = "2022-01-01",
  end_date = "2022-03-31",
  temp_res = "MONTHLY",
  region_source = "EEZ",
  compress = FALSE
)
} # }
```
