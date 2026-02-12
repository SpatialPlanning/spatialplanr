#load oceandatr package
library(oceandatr)

Seychelles_eez <- get_boundary(name = "Seychelles")
# Note: needes to update the packageVersion of curl before this worked

# plot to check we have Seychelles' EEZ
plot(Seychelles_eez[1],
     col = "lightgreen",
     main = "Seychelles EEZ",
     axes = TRUE)

projection_Seychelles <- "+proj=laea +lon_0=55 +lat_0=-4.5 +datum=WGS84 +units=m +no_defs"

Seychelles_grid <- get_grid(boundary = Seychelles_eez, resolution = 30000, crs = projection_Seychelles)

#project the eez into same projection as grid for plotting
Seychelles_eez_proj <- Seychelles_eez %>%
  sf::st_transform(crs = projection_Seychelles) %>%
  sf::st_geometry()

# plot the grid
terra::plot(Seychelles_grid, col = "gold3", axes = FALSE, legend = FALSE)
plot(Seychelles_eez_proj, add=TRUE)

# features


# Seychelles to sf object
Seychelles_pu <- Seychelles_grid %>%
  stars::st_as_stars() %>%
  sf::st_as_sf() %>%
  dplyr::mutate(
    id   = dplyr::row_number(),
    cost = as.numeric(sf::st_area(.)) / 1e6  # cost = area in km²
  ) %>%
  dplyr::select(-layer)









p_base <-
  prioritizr::problem(
    x              = Seychelles_sf,
    features       = feature_set,
    cost_column    = "cost"
  ) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.30) %>% # 30% of each feature
  add_binary_decisions() %>%
  add_rsymphony_solver(verbose = FALSE) # change this to cbc later

p_base


t_base <- system.time({
  s_base <- solve(p_base)
})

# Plot the baseline solution
p_base_plot <- plot_prioritizr(s_base) +
  ggtitle("Baseline (no MinPatch)")

p_base_plot




# Calculate reasonable parameters based on planning unit characteristics
median_area <- median(st_area(Seychelles_sf))

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
  prioritizr_problem = p_base,
  prioritizr_solution = s_base,
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
