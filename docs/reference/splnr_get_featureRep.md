# Prepare Data to Plot How Well Targets Are Met

`splnr_get_featureRep()` calculates the representation of conservation
features within a `prioritizr` solution. This function determines how
much of each feature's total abundance (or area) is captured in the
selected planning units, and compares it against specified conservation
targets. It can also account for different climate-smart planning
approaches.

## Usage

``` r
splnr_get_featureRep(
  soln,
  pDat,
  targets = NA,
  climsmart = FALSE,
  climsmartApproach = 0,
  solnCol = "solution_1"
)
```

## Arguments

- soln:

  An `sf` object representing the `prioritizr` solution, containing a
  column indicating selected planning units (default: `solution_1`).

- pDat:

  A `prioritizr` problem object, as defined by
  [`prioritizr::problem()`](https://prioritizr.net/reference/problem.html).
  This object provides the original feature data and targets.

- targets:

  A `data.frame` (optional). If provided, it should contain a `feature`
  column (character) and a `target` column (numeric). This is used to
  override or supplement targets from `pDat`, especially for
  climate-smart approaches where targets might be pre-adjusted. Defaults
  to `NA`.

- climsmart:

  A logical value (`TRUE` or `FALSE`). If `TRUE`, special handling for
  climate-smart approaches is enabled. Defaults to `FALSE`.

- climsmartApproach:

  An integer (0, 1, 2, or 3) indicating the type of climate-smart
  approach used:

  - `0`: No climate-smart approach.

  - `1`: Climate Priority Area approach (features split into CS/NCS).

  - `2`: Feature approach (not explicitly handled in this function's
    `climsmart` logic, targets taken from `pDat` by default).

  - `3`: Percentile approach (features are filtered).

  Defaults to `0`.

- solnCol:

  A character string specifying the name of the column in `soln` that
  contains the binary solution (1 for selected, 0 for not selected).
  Defaults to `"solution_1"`.

## Value

A `tibble` dataframe containing the `feature` names, their
`total_amount` (total units available), `absolute_held` (total units
selected), `relative_held` (proportion held), `target` (conservation
target), and `incidental` (TRUE if target was 0 or NA, but feature still
present).

## Details

This function processes the output of a `prioritizr` conservation
problem (`soln`) and its corresponding problem definition (`pDat`) to
provide a summary of feature representation. It is designed to work
whether or not explicit targets are provided, and can adjust
calculations based on the climate-smart approach used.

The function calculates:

- `total_amount`: The total available amount/area of each feature across
  all planning units.

- `absolute_held`: The total amount/area of each feature captured in the
  *selected* planning units (where `solution_1` is 1).

- `relative_held`: The proportion of `absolute_held` relative to
  `total_amount`, indicating the percentage representation of the
  feature in the solution.

- `target`: The conservation target for each feature (either from the
  `pDat` problem definition or the `targets` dataframe).

- `incidental`: A logical flag indicating if a feature's representation
  was 'incidental' (i.e., its target was 0 or NA, but it was still
  partially or fully captured in the solution).

**Climate-Smart Considerations (`climsmart = TRUE`):** If `climsmart` is
`TRUE`, the function adjusts its calculations based on the
`climsmartApproach` parameter:

- `climsmartApproach = 1` (Climate Priority Area): The function sums the
  `absolute_held` and `total_amount` for features that were split into
  `_CS` (Climate-Smart) and `_NCS` (Non-Climate-Smart) components. This
  provides a single, aggregated representation for the original feature,
  allowing comparison with its original target.

- `climsmartApproach = 3` (Percentile Approach): The function directly
  uses the targets provided in the `targets` dataframe, which are
  expected to be adjusted for the percentile approach.

- For other `climsmartApproach` values or if `climsmart` is `FALSE`,
  targets are taken directly from the `prioritizr` problem's target
  data.

The output dataframe is designed to be directly plottable by functions
like
[`splnr_plot_featureRep()`](https://mathmarecol.github.io/spatialplanr/reference/splnr_plot_featureRep.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# Assuming 'dat_species_bin' is an existing sf object with binary species data
# and 'Cost' column.

# Create a dummy prioritizr problem for basic demonstration
pDat_basic <- prioritizr::problem(
  dat_species_bin %>% dplyr::mutate(Cost = runif(n = dim(.)[[1]])),
  features = c("Spp1", "Spp2", "Spp3", "Spp4", "Spp5"),
  cost_column = "Cost"
) %>%
  prioritizr::add_min_set_objective() %>%
  prioritizr::add_relative_targets(0.3) %>%
  prioritizr::add_binary_decisions() %>%
  prioritizr::add_default_solver(verbose = FALSE)

# Solve the problem
soln_basic <- pDat_basic %>%
  prioritizr::solve.ConservationProblem()

# Get feature representation for a basic (non-climate-smart) solution
df_basic_rep <- splnr_get_featureRep(
  soln = soln_basic,
  pDat = pDat_basic
)
print(df_basic_rep)

# Example with Climate Priority Area (CPA) approach
# Assuming 'dat_clim' is an sf object with a 'metric' column.
# These would typically come from splnr_climate_priorityAreaApproach()
# For example purposes, we'll create some dummy data and targets.

# Simulate CPA processed features and targets
cpa_features_sim <- dat_species_bin %>%
  dplyr::mutate(
    Spp1_CS = ifelse(Spp1 == 1 & runif(n()) < 0.5, 1, 0),
    Spp1_NCS = ifelse(Spp1 == 1 & Spp1_CS == 0, 1, 0),
    Spp2_CS = ifelse(Spp2 == 1 & runif(n()) < 0.6, 1, 0),
    Spp2_NCS = ifelse(Spp2 == 1 & Spp2_CS == 0, 1, 0),
    Spp3_CS = ifelse(Spp3 == 1 & runif(n()) < 0.7, 1, 0),
    Spp3_NCS = ifelse(Spp3 == 1 & Spp3_CS == 0, 1, 0)
  ) %>%
  dplyr::select(Spp1_CS, Spp1_NCS, Spp2_CS, Spp2_NCS, Spp3_CS, Spp3_NCS, geometry)

cpa_targets_sim <- data.frame(
  feature = c("Spp1_CS", "Spp1_NCS", "Spp2_CS", "Spp2_NCS", "Spp3_CS", "Spp3_NCS"),
  target = c(0.8, 0.2, 0.9, 0.1, 0.7, 0.3) # Example targets for CS/NCS parts
)

# Create a problem with the simulated CPA features
pDat_cpa_sim <- prioritizr::problem(
  cpa_features_sim %>% dplyr::mutate(Cost = runif(n = dim(.)[[1]])),
  features = c("Spp1_CS", "Spp1_NCS", "Spp2_CS", "Spp2_NCS", "Spp3_CS", "Spp3_NCS"),
  cost_column = "Cost"
) %>%
  prioritizr::add_min_set_objective() %>%
  prioritizr::add_relative_targets(cpa_targets_sim$target, cpa_targets_sim$feature) %>%
  prioritizr::add_binary_decisions() %>%
  prioritizr::add_default_solver(verbose = FALSE)

# Solve the CPA problem
soln_cpa_sim <- pDat_cpa_sim %>%
  prioritizr::solve.ConservationProblem()

# Get feature representation for CPA approach
df_cpa_rep <- splnr_get_featureRep(
  soln = soln_cpa_sim,
  pDat = pDat_cpa_sim,
  targets = cpa_targets_sim, # Pass the original CPA targets
  climsmart = TRUE,
  climsmartApproach = 1 # Indicate CPA approach
)
print(df_cpa_rep)
} # }
```
