# Testing MinPatch to check Number of Valid Patches

library(minpatch)
library(prioritizr)
library(prioritizrdata)
library(terra)
library(sf)
library(ggplot2)
library(dplyr)
library(oceandatr)

#############################################################################################
## TASMANIA EXAMPLE
#############################################################################################
# load data
tas_pu <- get_tas_pu() %>%
  mutate(cost = cost*10000)

# At present minpatch works with sf objects. Here we convert the data to sf.
tas_features <- get_tas_features() %>%
  stars::st_as_stars() %>%
  sf::st_as_sf()

tas <- sf::st_interpolate_aw(tas_features, tas_pu, extensive = FALSE, keep_NA = FALSE, na.rm = FALSE) %>%
  st_join(tas_pu, join = st_equals)


features = tas %>%
  sf::st_drop_geometry() %>%
  dplyr::select(-all_of(c("id", "cost", "locked_in", "locked_out"))) %>%
  names()


# Convert data to binary again
tas <- tas %>%
  mutate(across(all_of(features), ~ if_else(.x > 0, 1, 0)))

##******************************************************************************************************
# diagnostics: check whether most cells would have 2-4 neighbors
nb_all <- st_relate(tas_pu, tas_pu, pattern = "F***1****") # check spatial relationships between geometries
deg_all <- lengths(nb_all) # counts how many neighbors each polygon has
summary(deg_all) # the mean and median of 6 is expected from a hexagonal grid
table(deg_all) # most cells would have 6 grid cell neighbors
##*******************************************************************************************************

# Run prioritizr
p <- problem(tas, features = features, cost_column = "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.30) %>%  # 30% of each feature
  add_binary_decisions() %>%
  add_default_solver(verbose = FALSE)

s <- solve(p)

plot_prioritizr(s)


# Calculate reasonable parameters based on planning unit characteristics
median_area <- median(st_area(tas))

# Set minimum patch size to 5x median planning unit area
min_patch_size <- median_area * 5

# Set patch radius to encompass approximately 10 planning units
patch_radius <- sqrt(median_area * 10)

cat("MinPatch parameters:\n")
cat("- Minimum patch size:", round(min_patch_size, 3), "square meters\n")
cat("- Patch radius:", round(patch_radius,3), "meters\n")
cat("- This means patches must be at least", round(min_patch_size/median_area, 3),
    "times the median planning unit size\n")

result <- run_minpatch(
  prioritizr_problem = p,
  prioritizr_solution = s,
  min_patch_size = min_patch_size,
  patch_radius = patch_radius,
  remove_small_patches = TRUE,
  add_patches = TRUE,
  whittle_patches = TRUE,
  verbose = TRUE
)


plot_minpatch(result, title = "MinPatch Results")
mp3_solution <- result$solution
mp3_solution <- mp3_solution |>
  dplyr::rename(solution_1 = minpatch)
plot_prioritizr(mp3_solution)


print_minpatch_summary(result)





# Compare original vs MinPatch solutions
comparison <- compare_solutions(result)

# Print overall comparison
cat("=== Overall Solution Comparison ===\n")
print(comparison$overall)

# Note: for the summary table. Instead of showing valida patches >= min size without context, perhaps do ratio between valid and invalid patches for a better valu?


# Print feature-level comparison
cat("\n=== Feature-Level Area Comparison ===\n")
print(comparison$features)

# Print summary statistics
cat("\n=== Feature Change Summary ===\n")
print(comparison$summary)










################################################################
## SEYCHELLES
#################################################################
Seychelles_eez <- get_boundary(name = "Seychelles")
# Note: needes to update the packageVersion of curl before this worked

# plot to check we have Seychelles' EEZ
plot(Seychelles_eez[1],
     col = "lightgreen",
     main = "Seychelles EEZ",
     axes = TRUE)
projection_Seychelles <- "+proj=laea +lon_0=55 +lat_0=-4.5 +datum=WGS84 +units=m +no_defs"

# grid the planning area
Seychelles_grid <- get_grid(
  boundary   = Seychelles_eez,
  resolution = 30000, # 30,000 just to test the code but a finer resolution can be opted with a more powerful PC
  crs        = projection_Seychelles
)

# project the eez into same projection as grid for plotting
Seychelles_eez_proj <- Seychelles_eez %>%
  sf::st_transform(crs = projection_Seychelles) %>%
  sf::st_geometry()

# plot the grid
terra::plot(Seychelles_grid,
            col = "gold3",
            axes = FALSE,
            legend = FALSE,
            main = "Seychelles spatial grid (30 km)")
plot(Seychelles_eez_proj,
     add = TRUE,
     border = "black",
     lwd = 1)

Seychelles_pu <- Seychelles_grid %>%
  terra::as.polygons(dissolve = FALSE) %>%
  sf::st_as_sf()


##******************************************************************************************************
# diagnostics: check whether most cells would have 2-4 neighbors
nb_all <- st_relate(Seychelles_pu, Seychelles_pu, pattern = "F***1****")
deg_all <- lengths(nb_all)
summary(deg_all) # the median is 3 [for a square grid, it should be 4]
table(deg_all) # many cells have 2-4 neighbors, a few have 0 neighbors!!, and a lot have 1 neighbor
# there are thin silver, and complex boundaries, perhaps as a result of clipping to a complex EEZ
##*******************************************************************************************************

Seychelles_pu_adj <- Seychelles_pu |>
  sf::st_make_valid() |>
  lwgeom::st_snap_to_grid(size = 1)

nb_all <- st_relate(Seychelles_pu_adj, Seychelles_pu_adj, pattern = "F***1****")
deg_all <- lengths(nb_all)
summary(deg_all) # median is 4, which is correct
table(deg_all) # most have 4 neighbors


# Notes: perhaps the silver polygons resulted from converting the raster into sf polygon, and using a square grid

####################################################################################################
## SEYCHELLES PART 2
#####################################################################################################


Seychelles_eez <- get_boundary(name = "Seychelles")
# Note: needes to update the packageVersion of curl before this worked

# plot to check we have Seychelles' EEZ
plot(Seychelles_eez[1],
     col = "lightgreen",
     main = "Seychelles EEZ",
     axes = TRUE)
projection_Seychelles <- "+proj=laea +lon_0=55 +lat_0=-4.5 +datum=WGS84 +units=m +no_defs"

# grid the planning area
Seychelles_grid <- get_grid(
  boundary   = Seychelles_eez,
  resolution = 30000, # 30,000 just to test the code but a finer resolution can be opted with a more powerful PC
  crs        = projection_Seychelles,
  output = "sf_hex" # so the default was raster, hence, I changed it to sf_hex to begin with as minpatch also just works with sf objects
)

# project the eez into same projection as grid for plotting
Seychelles_eez_proj <- Seychelles_eez %>%
  sf::st_transform(crs = projection_Seychelles) %>%
  sf::st_geometry()

# plot the grid
terra::plot(Seychelles_grid,
            col = "gold3",
            axes = FALSE,
            legend = FALSE,
            main = "Seychelles spatial grid (30 km)")

plot(Seychelles_eez_proj,
     add = TRUE,
     border = "black",
     lwd = 1)

#Seychelles_pu <- Seychelles_grid %>%
#  sf::st_make_valid()


##******************************************************************************************************
# diagnostics: check whether most cells would have 2-4 neighbors
nb_all <- st_relate(Seychelles_pu, Seychelles_pu, pattern = "F***1****")
deg_all <- lengths(nb_all)
summary(deg_all) # the median is 6 which is correct
table(deg_all) # many cells have 6 neighbors which is correct
##*******************************************************************************************************















# Scenario 1:
# get bathymetry with classifications into different depth zones
depth_zones <- get_bathymetry(spatial_grid = Seychelles_grid,
                              classify_bathymetry = TRUE)
# this may take seconds to minutes, depending on grid size

# plot
terra::plot(depth_zones,
            col = c("grey60", "navyblue"),
            axes = FALSE, fun =
              function(){terra::lines(terra::vect(Seychelles_eez_proj))})
# value of 1 indicates that depth zone is present




























########
# ============================================================
# Label MinPatch patches (rook adjacency) as valid/invalid
# Non-function version, skips snap-to-grid
# Robust handling of sf/units on Windows (udunits quirks)
# Requires: result (from run_minpatch), min_patch_size (from your code)
# ============================================================

library(sf)
library(dplyr)
library(igraph)
library(ggplot2)
library(shadowtext)
library(units)

# --- start from your MinPatch solution ---
mp3_solution <- result$solution |>
  dplyr::rename(solution_1 = minpatch) |>
  mutate(
    pu_id    = row_number(),
    selected = (solution_1 == 1)
  )

# --- selected PUs only for patch detection ---
sel <- mp3_solution |> filter(selected)
stopifnot(nrow(sel) > 0)

# make geometries valid (no snapping)
sel2 <- sel |> sf::st_make_valid()

# ------------------------------------------------------------
# MIN PATCH AREA THRESHOLD (units-safe, udunits-safe)
# ------------------------------------------------------------
# st_area() returns a units object. We extract its unit in a robust way.
# If udunits lookup fails on your machine, we fall back to "m^2".
area_unit_str <- tryCatch(
  units::deparse_unit(sf::st_area(sel2)[1]),
  error = function(e) "m^2"
)

# Ensure min_patch_area is a units object with the same units as st_area()
if (inherits(min_patch_size, "units")) {
  # keep units but coerce to the same unit string (just in case)
  min_patch_area <- tryCatch(
    units::set_units(min_patch_size, area_unit_str),
    error = function(e) min_patch_size
  )
} else {
  # min_patch_size is numeric; assign area units
  min_patch_area <- units::set_units(min_patch_size, area_unit_str)
}

# --- rook adjacency via DE-9IM: boundary touch (edge-sharing style) ---
nb_list <- sf::st_relate(sel2, sel2, pattern = "F***1****")

# build edge list for igraph
edges <- do.call(
  rbind,
  lapply(seq_along(nb_list), function(i) {
    if (length(nb_list[[i]]) == 0) return(NULL)
    cbind(from = i, to = nb_list[[i]])
  })
)

g <- igraph::make_empty_graph(n = nrow(sel2), directed = FALSE)
if (!is.null(edges) && nrow(edges) > 0) {
  g <- igraph::add_edges(g, as.vector(t(edges)))
}

comp <- igraph::components(g)
sel2$patch_id <- comp$membership

# --- patch polygons + patch area + validity ---
patch_info <- sel2 |>
  group_by(patch_id) |>
  summarise(
    n_pu       = dplyr::n(),
    patch_area = sum(sf::st_area(geometry), na.rm = TRUE),  # keep units!
    geometry   = sf::st_union(geometry),
    .groups    = "drop"
  ) |>
  sf::st_as_sf() |>
  mutate(
    is_valid = patch_area >= min_patch_area,
    label_pt = sf::st_point_on_surface(geometry)
  )

# label coords
xy <- sf::st_coordinates(patch_info$label_pt)
patch_info$x <- xy[, 1]
patch_info$y <- xy[, 2]

# counts
n_patches         <- nrow(patch_info)
n_valid_patches   <- sum(patch_info$is_valid)
n_invalid_patches <- n_patches - n_valid_patches

# threshold (km^2) for printing only (safe fallback)
threshold_km2 <- tryCatch(
  as.numeric(units::set_units(min_patch_area, "km^2")),
  error = function(e) NA_real_
)

cat(
  "Patches: ", n_patches,
  " | valid: ", n_valid_patches,
  " | invalid: ", n_invalid_patches,
  " | threshold area ≥ ", ifelse(is.na(threshold_km2), "NA", round(threshold_km2, 3)), " km^2\n",
  sep = ""
)

# --- join patch_id + validity back to all PUs for plotting ---
sel_class <- sel2 |>
  sf::st_drop_geometry() |>
  select(pu_id, patch_id) |>
  left_join(
    patch_info |> sf::st_drop_geometry() |> select(patch_id, is_valid),
    by = "patch_id"
  ) |>
  mutate(status = ifelse(is_valid, "Valid patch", "Invalid patch"))

plot_df <- mp3_solution |>
  left_join(sel_class |> select(pu_id, patch_id, status), by = "pu_id") |>
  mutate(
    status = ifelse(is.na(status), "Not selected", status),
    status = factor(status, levels = c("Not selected", "Valid patch", "Invalid patch"))
  )

# --- plot with patch labels ---
p <- ggplot() +
  geom_sf(
    data = plot_df,
    aes(fill = status),
    colour = "white",
    linewidth = 0.15
  ) +
  shadowtext::geom_shadowtext(
    data = patch_info,
    aes(x = x, y = y, label = patch_id),
    size = 3.8,
    fontface = "bold",
    colour = "black",
    bg.colour = "white",
    bg.r = 0.12
  ) +
  scale_fill_manual(
    values = c(
      "Not selected"  = "grey92",
      "Valid patch"   = "#2ca25f",
      "Invalid patch" = "red"
    )
  ) +
  labs(
    title = "MinPatch solution: rook-connected patches",
    subtitle = paste0(
      n_patches, " patches | ",
      n_valid_patches, " valid | ",
      n_invalid_patches, " invalid | threshold area ≥ ",
      ifelse(is.na(threshold_km2), "NA", round(threshold_km2, 3)), " km²"
    ),
    fill = NULL
  ) +
  theme_minimal()

print(p)








library(sf)
library(dplyr)

projection_Seychelles <- "+proj=laea +lon_0=55 +lat_0=-4.5 +datum=WGS84 +units=m +no_defs"

# 1) EEZ in projected CRS (metres)
Seychelles_eez <- get_boundary("Seychelles") |>
  st_transform(projection_Seychelles)

# 2) Build a CLEAN square grid (cells are perfect neighbours)
cellsize <- 30000  # 30 km

grid_sq <- st_make_grid(
  Seychelles_eez,
  cellsize = cellsize,
  square   = TRUE
)

Seychelles_pu <- st_as_sf(grid_sq) |>
  mutate(pu_id = row_number())

# 3) Keep WHOLE cells that intersect the EEZ (no cutting!)
keep <- lengths(st_intersects(Seychelles_pu, Seychelles_eez)) > 0
# subset first
Seychelles_pu <- Seychelles_pu[keep, ]

# compute area explicitly
area_m2 <- as.numeric(sf::st_area(Seychelles_pu))

# add columns
Seychelles_pu$area_m2 <- area_m2
Seychelles_pu$cost    <- area_m2 / 1e6


# 4) Check neighbours (should be square-grid behaviour)
nb_all  <- st_relate(Seychelles_pu, Seychelles_pu, pattern = "F***1****")
deg_all <- lengths(nb_all)
summary(deg_all)
table(deg_all)

# 5) Quick plot
plot(st_geometry(Seychelles_pu), col = "grey90", border = "white")
plot(st_geometry(Seychelles_eez), add = TRUE, border = "black", lwd = 2)

