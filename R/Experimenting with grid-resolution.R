## How does the Cohen penalty change with planning-unit resolution?
So far, we have changed the targets and the number of features while keeping the planning units fixed. In practice, however, we also make choices about the resolution of the plannin grid. For example, in a marine planning problem, we might use relatively coasrse coastal polygons in one analysis and a finer grid in another.

Here, we explore a simple question:
  How does the Cohon-calibrated boundary penalty behave - its runtime and fragmentation metrics - when we change the planning-unit resolution, while keeping targets and features fixed?

  ### Creating coarse and fine planning-unit grids
  First, we keep a copy of the current Tasmania layer (tas) as the base resolution, and then create coarser and finer grids by overlaying regular grids and aggregating cost and feature values into each grid cell.

```{r}
library(sf)
library(dplyr)

# keep the current cropped Tasmania layer as the "base" resolution

tas_base <- tas

# helper: create a planning-unit grid and aggregate cost + features

make_grid_pus <- function(pu_sf, feature_names, nx, ny, id_col = "grid_id") {
  bb <- sf::st_bbox(pu_sf)

  grid <- sf::st_make_grid(sf::st_as_sfc(bb), n = c(nx, ny)) |>
    sf::st_as_sf() |>
    dplyr::mutate(!!id_col := dplyr::row_number())

  # intersect original planning units with grid cells

  intersected <- sf::st_intersection(grid, pu_sf)

  # aggregate cost and features into each grid cell

  # for presence/absence features, the mean is roughly the fraction of area with presence

  agg <- intersected |>
    dplyr::group_by(.data[[id_col]]) |>
    dplyr::summarise(
      cost = mean(cost, na.rm = TRUE),
      dplyr::across(all_of(feature_names), ~ mean(.x, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    # keep only cells that have data
    dplyr::filter(!is.na(cost))

  agg
}

# choose grid densities (coarser vs finer than the base layer)

tas_coarse <- make_grid_pus(tas_base, all_features, nx = 6,  ny = 6)
tas_fine   <- make_grid_pus(tas_base, all_features, nx = 12, ny = 12)

# quick check: how many planning units at each resolution?

nrow(tas_coarse); nrow(tas_base); nrow(tas_fine)

```

Running Cohon calibration at each resolution

Next, we define a small helper that is similar to run_cohon_experiment(), but takes a planning-unit layer as an argument. This lets us reuse the same workflow for the coarse, base, and fine grids.
```{r}
run_cohon_experiment_pu <- function(label,
                                    target,
                                    pu_data,
                                    feature_names) {

  # 1) build the base problem

  prob <- problem(
    x           = pu_data,
    features    = feature_names,
    cost_column = "cost"
  ) |>
    add_min_set_objective() |>
    add_relative_targets(target) |>
    add_binary_decisions() |>
    add_cbc_solver(verbose = TRUE)

  # 2) add a dummy boundary penalty for calibration

  prob_bp <- prob |>
    add_boundary_penalties(penalty = 0.0001, edge_factor = 1)

  # 3) calibrate using Cohon's method

  calib_time <- system.time({
    cohon_penalty <- calibrate_cohon_penalty(
      prob_bp,
      approx  = TRUE,   # approximate mode for speed
      verbose = TRUE
    )
  })

  # 4) solve the problem with the calibrated penalty

  prob_final <- prob |>
    add_boundary_penalties(penalty = cohon_penalty, edge_factor = 1)

  solve_time <- system.time({
    solution <- solve(prob_final)
  })

  # 5) summarise the solution

  metrics <- summarise_solution(
    label            = label,
    solution_sf      = solution,
    solve_time_sec   = solve_time[["elapsed"]],
    min_patch_size   = NULL,
    boundary_penalty = as.numeric(cohon_penalty)
  )

  metrics |>
    dplyr::mutate(
      scenario       = label,
      n_features     = length(feature_names),
      target         = target,
      cohon_penalty  = as.numeric(cohon_penalty),
      calib_time_sec = calib_time[["elapsed"]],
      total_time_sec = calib_time[["elapsed"]] + solve_time[["elapsed"]]
    )
}

```

We now run Cohon calibration for the three resolutions, keeping the target fixed at 30% and using all features.

```{r}
target_res  <- 0.30
feature_set <- all_features

# collect the three planning-unit layers into a named list

pu_resolutions <- list(
  coarse = tas_coarse,
  base   = tas_base,
  fine   = tas_fine
)

cohon_resolution_results <- purrr::imap_dfr(
  pu_resolutions,
  ~ {
    pu_layer <- .x
    res_name <- .y

    message(
      "Running resolution: ", res_name,
      " (n_pu = ", nrow(pu_layer), "), target = ", target_res * 100, "%"
    )

    run_cohon_experiment_pu(
      label         = paste0("res_", res_name),
      target        = target_res,
      pu_data       = pu_layer,
      feature_names = feature_set
    ) |>
      dplyr::mutate(
        resolution = res_name,
        n_pu       = nrow(pu_layer)
      )

  }
)

cohon_resolution_results |>
  dplyr::select(
    resolution, n_pu,
    target, n_features,
    cohon_penalty,
    calib_time_sec, solver_time_sec, total_time_sec,
    boundary_length_km, n_patches, median_patch_km2,
    pu_cost_total, total_cost
  ) |>
  knitr::kable(
    digits  = 2,
    caption = "Cohon-calibrated penalties and metrics across planning-unit resolutions (target 30%, all features)."
  )

cohon_resolution_results
```

How do runtime and penalties scale with resolution?

  Finally, we plot the total runtime and the calibrated penalty against the number of planning units to see how calibration responds as we move from coarse to fine grids.
```{r}
# runtime vs number of planning units

ggplot(cohon_resolution_results,
       aes(x = n_pu, y = total_time_sec, label = resolution)) +
  geom_point(size = 3) +
  ggrepel::geom_text_repel(show.legend = FALSE) +
  theme_minimal() +
  labs(
    x = "Number of planning units",
    y = "Calibration + solve time (s)",
    title = "Runtime for Cohon calibration at different planning-unit resolutions"
  )

# calibrated penalty vs number of planning units

ggplot(cohon_resolution_results,
       aes(x = n_pu, y = cohon_penalty, label = resolution)) +
  geom_point(size = 3) +
  ggrepel::geom_text_repel(show.legend = FALSE) +
  theme_minimal() +
  labs(
    x = "Number of planning units",
    y = "Calibrated boundary penalty",
    title = "How does the Cohon penalty change with planning-unit resolution?"
  )

```


## Comparison and interpretation
Finally, we combine all runs into one table and plot. This is not meant
to be exhaustive, but it helps to see patterns at a glance.

```{r}
cohon_all <- dplyr::bind_rows(
  cohon_target_results,
  cohon_feature_results
)

cohon_all |>
  select(
    scenario, n_features, target,
    cohon_penalty,
    calib_time_sec, solver_time_sec, total_time_sec,
    boundary_length_km, pu_cost_total, total_cost
  ) |>
  arrange(target, n_features) |>
  knitr::kable(
    digits  = 2,
    caption = "Summary of Cohon calibration runs across targets and feature sets."
  )
```
**randomization, and lessen the number of features (do it 4x)
**try to do other targets when experimenting the number of features

### What does this mean in practice?
A few practical points for how we might use this in *spatialplanr* and
*shinyplanr:

  * The **calibrated penalty is not fixed**. It responds to how hard the problem is: *higher targets and more features can shift the penalty.*

  * Calibration has a **non-trivial runtime cost**. In some cases, the
time spent on `calibrate_cohon_penalty()` is similar to or larger than
the time spent solving the calibrated problem.

* When building an interactive app, we probably do not want to re-run
calibration for every small change the user makes. Instead, we may:
  - calibrate once for a “typical” problem and reuse that penalty, or
- allow the user to choose between a default calibrated penalty and
simple manual slider for fine-tuning.

The small experiments here are not meant to be definitive, but they do
show that Cohon-style calibration is sensitive to both the **targets**
  and the **number of features**, and that this sensitivity comes with a
runtime cost. This is useful context when deciding when, and how, to
offer automatic boundary calibration in a workflow.





