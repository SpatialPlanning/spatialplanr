library(spatialplanr)
library(prioritizr)
library(prioritizrdata)
library(minpatch)

library(dplyr)
library(sf)
library(ggplot2)
library(ggrepel)
library(tibble)
library(tidyr)
library(patchwork)



# load data
tas_pu <- get_tas_pu()

# At present minpatch works with sf objects. Here we convert the data to sf.
tas_features <- get_tas_features() %>%
  stars::st_as_stars() %>%
  sf::st_as_sf()

tas <- sf::st_interpolate_aw(
  tas_features,
  tas_pu,
  extensive = FALSE,
  keep_NA = FALSE,
  na.rm = FALSE
) %>%
  st_join(tas_pu, join = st_equals)

summary(tas$cost)

features <- tas %>%
  sf::st_drop_geometry() %>%
  dplyr::select(-all_of(c("id", "cost", "locked_in", "locked_out"))) %>%
  names()

# Convert data to binary again
tas <- tas %>%
  mutate(across(all_of(features), ~ if_else(.x > 0, 1, 0)))


# To ease runtime: Crop the study area to a manageable size

# bbox <- sf::st_bbox(tas)
#
# frac <- 0.50  # use to adjust the scope of the planning region
#
# bbox_small <- bbox
# bbox_small["xmax"] <- bbox["xmin"] + frac * (bbox["xmax"] - bbox["xmin"])
# bbox_small["ymax"] <- bbox["ymin"] + frac * (bbox["ymax"] - bbox["ymin"])
#
# tas_small <- sf::st_crop(tas, bbox_small)
#
# tas <- tas_small
#
# nrow(tas)

plot(sf::st_geometry(tas))


# base problem
p <- problem(tas, features = features, cost_column = "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.30) %>%  # 30% of each feature
  add_binary_decisions() %>%
  add_rsymphony_solver(verbose = TRUE)

print(p)


set.seed(1) # for reproducibility

base_time <- system.time({
  base_solution <- solve(p)
})

base_time

plot_prioritizr(base_solution)


# cohen calibration
p_bp_cohon <- p |>
  add_boundary_penalties(penalty = 0.0001)

cohon_time <- system.time({
  cohon_penalty <- calibrate_cohon_penalty(
    p_bp_cohon,
    approx  = TRUE,     # approximate mode for speed
    verbose = TRUE
  )
})

cohon_penalty
cohon_time








###############################################
# USING A RASTER BASED APPROACH
library(spatialplanr)
library(prioritizr)
library(prioritizrdata)
library(minpatch)

library(dplyr)
library(sf)
library(ggplot2)
library(ggrepel)
library(tibble)
library(tidyr)
library(patchwork)
library(terra)



#-----------------------------------------------------------
# 1. Load data
#-----------------------------------------------------------

# Planning units as polygons (for the cost field)
tas_pu <- get_tas_pu()

# Features as raster – IMPORTANT: keep as SpatRaster
tas_features <- get_tas_features()



#-----------------------------------------------------------
# 2. Make a cost raster aligned with feature rasters
#-----------------------------------------------------------

# Convert sf -> terra vect
tas_pu_v <- terra::vect(tas_pu)

# Rasterize polygon costs to the same grid as tas_features
cost_raster <- terra::rasterize(
  tas_pu_v,
  tas_features[[1]],  # template raster: same extent/resolution
  field = "cost"
)



#-----------------------------------------------------------
# 3. Convert features to binary rasters (presence/absence)
#    (equivalent to mutate(across(... > 0, 1, 0)))
#-----------------------------------------------------------

features_raster <- terra::app(
  tas_features,
  fun = function(x) as.integer(x > 0)
)

# Make sure layer names are valid variable names
names(features_raster) <- make.names(names(features_raster))



#-----------------------------------------------------------
# 4. Use ENTIRE planning region (no cropping or aggregation)
#-----------------------------------------------------------

cost_raster_full     <- cost_raster
features_raster_full <- features_raster

plot(cost_raster_full, main = "Planning unit cost (full region)")

# If you want to see how many PUs:
n_pu_rast <- sum(!is.na(terra::values(cost_raster_full)))
n_pu_rast



#-----------------------------------------------------------
# 5. Base problem (RASTER VERSION)
#-----------------------------------------------------------

# With rasters:
#   - first arg: cost raster
#   - second arg: feature raster stack
p <- problem(cost_raster_full, features_raster_full) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.30) %>%  # 30% of each feature
  add_binary_decisions() %>%
  add_rsymphony_solver(verbose = TRUE)

print(p)



#-----------------------------------------------------------
# 6. Solve base problem
#-----------------------------------------------------------

set.seed(1)  # for reproducibility

base_time <- system.time({
  base_solution <- solve(p)
})

base_time

# base_solution is a SpatRaster of selected cells (0/1)



#-----------------------------------------------------------
# 7. Cohen calibration (raster problem)
#-----------------------------------------------------------

p_bp_cohon <- p %>%
  add_boundary_penalties(penalty = 0.0001)

cohon_time <- system.time({
  cohon_penalty <- calibrate_cohon_penalty(
    p_bp_cohon,
    approx  = TRUE,   # approximate mode for speed
    verbose = TRUE
  )
})

cohon_penalty
cohon_time
