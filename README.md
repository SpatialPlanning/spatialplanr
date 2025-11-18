
<!-- README.md is generated from README.Rmd. Please edit that file -->

# spatialplanr <a href="https://SpatialPlanning.github.io/spatialplanr"><img src="man/figures/logo.png" align="right" height="139" alt="spatialplanr website"></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Ubuntu](https://github.com/SpatialPlanning/spatialplanr/actions/workflows/Ubuntu.yaml/badge.svg)](https://github.com/SpatialPlanning/spatialplanr/actions/workflows/Ubuntu.yaml)
[![MacOS](https://github.com/SpatialPlanning/spatialplanr/actions/workflows/MacOS.yaml/badge.svg)](https://github.com/SpatialPlanning/spatialplanr/actions/workflows/MacOS.yaml)
[![Windows](https://github.com/SpatialPlanning/spatialplanr/actions/workflows/Windows.yaml/badge.svg)](https://github.com/SpatialPlanning/spatialplanr/actions/workflows/Windows.yaml)
[![Codecov test
coverage](https://codecov.io/gh/SpatialPlanning/spatialplanr/branch/main/graph/badge.svg)](https://app.codecov.io/gh/SpatialPlanning/spatialplanr/tree/main)
[![Issues](https://img.shields.io/github/issues/SpatialPlanning/spatialplanr)](https://github.com/SpatialPlanning/spatialplanr/issues)
<!-- badges: end -->

# Introduction to spatialplanr

Welcome to *spatialplanr*, an R package designed to streamline and
enhance spatial conservation prioritization efforts by explicitly
integrating climate change considerations. Building upon the powerful
*prioritizr* package, *spatialplanr* provides a suite of tools for
conservation planners to develop robust protected area networks.

## Installation

Be aware that this package is in the very early stages of development.
Functions and documentation are not complete so installing at the moment
is at your own risk. If you are still interested, you can install the
development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("https://github.com/SpatialPlanning/spatialplanr")
```

<!-- # Purpose and Goals -->
<!-- The escalating impacts of climate change necessitate a paradigm shift in how we approach conservation. Traditional conservation planning often focuses on static biodiversity patterns, which may not adequately account for the dynamic nature of species distributions and ecosystem processes under a changing climate. _spatialplanr_ aims to address this gap by: -->
<!-- **Facilitating Climate-Smart Planning**: Providing functions that incorporate climate data directly into the planning process, allowing for the identification of areas critical for both biodiversity and resilience to future climate conditions. -->
<!-- Offering Diverse Methodological Approaches: Implementing multiple established climate-smart conservation planning frameworks (e.g., Climate Priority Areas, Climate Features, Climate Percentiles) to offer flexibility based on specific planning goals and data availability. -->
<!-- **Streamlining Workflow**: Offering end-to-end functionality, from data preprocessing and integration to advanced visualization of results, simplifying complex analytical tasks for users. -->
<!-- **Enhancing Decision-Making**: Producing outputs that directly feed into spatial prioritization software like prioritizr, enabling the generation of optimal conservation solutions that balance biodiversity representation with climate resilience. -->
<!-- By using spatialplanr, practitioners can move beyond reactive conservation and proactively design protected area systems better equipped to safeguard biodiversity in a rapidly changing world. -->
<!-- # Core Climate-Smart Planning Approaches -->
<!-- _spatialplanr_ implements several key approaches for integrating climate change into spatial prioritization, largely drawing inspiration from frameworks like that presented in Buenafe et al. (2023) "A metric‐based framework for climate‐smart conservation planning" (DOI: 10.1002/eap.2852). -->
<!-- These approaches are designed to transform biodiversity features and conservation targets based on climate metrics, allowing for a more nuanced understanding of how climate change may impact conservation priorities. The main approaches included in _spatialplanr_ are: -->
<!--   * **Climate Priority Area (CPA) Approach**: Identifies climate-smart areas within the distribution of each conservation feature, creating new components for climate-smart and non-climate-smart areas. -->
<!--   * **Climate Feature Approach**: Treats climate-smart areas as a distinct conservation feature, allowing for explicit conservation targets on climate resilience. -->
<!--   * **Climate Percentile Approach**: Sets conservation targets based on percentile ranges of climate metrics, allowing for targeted protection of areas within specific climate resilience thresholds. -->
<!-- # Data Acquisition and Preprocessing Utilities -->
<!-- _spatialplanr_ also provides convenience functions for acquiring and preparing data, which can be crucial for climate-smart planning. -->
<!-- ```{r} -->
<!-- # Load spatialplanr -->
<!-- library(spatialplanr) -->
<!-- library(tidyverse) -->
<!-- ``` -->
<!-- 1. Get IUCN Red List Data -->
<!-- Function: `splnr_get_IUCNRedList()` -->
<!-- This function interfaces with the IUCN Red List API to retrieve conservation status information for a list of species. This data can be valuable for assigning species-specific targets (e.g., higher targets for more threatened species) or filtering species based on their conservation status. -->
<!-- Note: Requires an IUCN Red List API token. -->
<!-- ```{r} -->
<!-- # # Example: Fetch IUCN data for a few marine species -->
<!-- # # Ensure your IUCN Red List API token is set: -->
<!-- # # Sys.setenv(IUCN_REDLIST_KEY = "YOUR_API_KEY") # Replace with your actual key -->
<!-- #  -->
<!-- # # Example species list -->
<!-- # my_species <- c("Orcinus orca", "Chelonia mydas", "Thunnus thynnus") -->
<!-- #  -->
<!-- # # Create a dataframe matching the expected input format -->
<!-- # species_df <- data.frame( -->
<!-- #   scientific_name = my_species -->
<!-- # ) -->
<!-- #  -->
<!-- # # Get IUCN data -->
<!-- # iucn_data <- splnr_get_IUCNRedList(df = species_df, species_col = "scientific_name") -->
<!-- # print(iucn_data) -->
<!-- ``` -->
<!-- 2. Get Global Fishing Watch (GFW) Data -->
<!-- Function: `splnr_get_gfw()` -->
<!-- This function facilitates the retrieval of fishing activity data (e.g., apparent fishing hours) from Global Fishing Watch (GFW). GFW data can be used to inform cost layers (e.g., higher fishing effort areas might have higher opportunity costs for conservation) or as a proxy for human impact in planning units. -->
<!-- Note: Requires a GFW API token. -->
<!-- ```{r} -->
<!-- # Example: Get yearly fishing hours for Australia EEZ -->
<!-- gfw_data_aus <- splnr_get_gfw( -->
<!--   region = 'Australia', -->
<!--   start_date = "2021-01-01", -->
<!--   end_date = "2021-12-31", -->
<!--   temp_res = "YEARLY", -->
<!--   spat_res = "LOW", -->
<!--   region_source = "EEZ", -->
<!--   cCRS = "EPSG:4326", -->
<!--   compress = TRUE # Returns polygons aggregated by fishing hours -->
<!-- ) -->
<!-- print(head(gfw_data_aus)) -->
<!-- ``` -->
<!-- ## Visualization Tools -->
<!-- _spatialplanr_ offers a rich set of plotting functions to visualize input data, climate metrics, and most importantly, the outputs of your spatial prioritization analyses. These functions are built on ggplot2 and sf for high-quality spatial visualizations. -->
<!-- 1. Plot Climate Data -->
<!-- Function: `splnr_plot_climData()` -->
<!-- Visualizes the spatial distribution of your climate metric, allowing you to quickly inspect patterns in climate velocity, temperature anomaly, or other relevant climate variables. -->
<!-- ```{r} -->
<!-- # Assuming 'dat_clim' from previous examples is available -->
<!-- # Plot the 'metric' column from dat_clim -->
<!-- splnr_plot_climData( -->
<!--   df = dat_clim, -->
<!--   colInterest = "metric", -->
<!--   plotTitle = "Example Climate Metric Distribution", -->
<!--   legendTitle = "Metric Value" -->
<!-- ) -->
<!-- ``` -->
<!-- 2. Plot Climate Kernel Density -->
<!-- Functions: `splnr_plot_climKernelDensity_Basic()`, `splnr_plot_climKernelDensity_Fancy()`, `splnr_plot_climKernelDensity()` -->
<!-- These functions help visualize the distribution of climate metric values within selected planning units (e.g., a proposed protected area network) compared to the overall distribution. This helps assess if climate-smart areas are being adequately captured. -->
<!-- `_Basic()`: Simple kernel density plot. -->
<!-- `_Fancy()`: More customizable kernel density plot, allowing multiple solutions and specific zones. -->
<!-- `_climKernelDensity()`: A wrapper function that selects between basic and fancy plots based on input type. -->
<!-- ```{r} -->
<!-- solution_df <- dat_clim %>% -->
<!--   dplyr::mutate( -->
<!--     solution_1 = sample(c(0, 1), size = dplyr::n(), replace = TRUE, prob = c(0.7, 0.3)), -->
<!--     metric_category = cut(metric, breaks = 3, labels = c("Low", "Medium", "High")) -->
<!--   ) -->
<!-- # Basic kernel density plot of the metric for selected areas vs. all areas -->
<!-- splnr_plot_climKernelDensity( -->
<!--   soln = solution_df, -->
<!--   type = "Basic", -->
<!--   names = "solution_1" # Column indicating selected PUs (1=selected) -->
<!-- ) -->
<!-- # Fancy kernel density plot, perhaps with zones (if your solution has them) -->
<!-- # For this example, let's pretend 'solution_df' has a 'zone' column -->
<!-- # (this would come from a prioritizr zoned solution) -->
<!-- zoned_solution_df <- solution_df %>% -->
<!--   dplyr::mutate( -->
<!--     zone = sample(c("Zone A", "Zone B"), size = dplyr::n(), replace = TRUE) -->
<!--   ) -->
<!-- # Example for Fancy (requires a list of solutions or specific structure) -->
<!-- # If you have multiple prioritizr solutions, you would put them in a list. -->
<!-- # For simplicity, we'll create a mock 'soln' list: -->
<!-- soln_list <- list( -->
<!--   Solution1 = zoned_solution_df, -->
<!--   Solution2 = zoned_solution_df %>% mutate(solution_1 = abs(solution_1 - 1)) # Invert selection for demo -->
<!-- ) -->
<!-- # plot_climKernelDensity handles both basic and fancy automatically -->
<!-- splnr_plot_climKernelDensity( -->
<!--   soln = soln_list, # Pass the list of solutions -->
<!--   names = c("metric", "metric"), -->
<!--   type = "Normal" # Explicitly request fancy -->
<!--   # zone_column = "zone" # If your solutions have zones -->
<!-- ) -->
<!-- ``` -->
<!-- 3. Plot Prioritization Solutions -->
<!-- Function: `splnr_plot_solution()` -->
<!-- Visualizes the spatial output of a prioritizr problem, showing which planning units were selected for conservation. It supports both single-zone and multi-zone solutions. -->
<!-- ```{r} -->
<!-- # Plot a single-zone solution -->
<!-- splnr_plot_solution( -->
<!--   soln = solution_df, -->
<!--   colorVals = c("grey", "darkgreen"), -->
<!--   legendLabels = c("Not Selected", "Selected"), -->
<!--   plotTitle = "Prioritization Solution (Single Zone)" -->
<!-- ) -->
<!-- # Plot a zoned solution (assuming 'zone' column exists) -->
<!-- # In a real scenario, this 'zone' column would be part of your prioritizr output. -->
<!-- # splnr_plot_solution( -->
<!-- #   soln = zoned_solution_df, -->
<!-- #   colorVals = c("red", "blue", "grey"), # Colors for each zone + not selected -->
<!-- #   legendLabels = c("Zone A", "Zone B", "Not Selected"), -->
<!-- #   plotTitle = "Prioritization Solution (Zoned)", -->
<!-- #   zones = TRUE -->
<!-- # ) -->
<!-- ``` -->
<!-- 4. Plot Cost Overlay -->
<!-- Function: `splnr_plot_costOverlay()` -->
<!-- Overlays cost data on top of a prioritization solution, helping to visualize the spatial distribution of costs relative to selected areas. -->
<!-- ```{r} -->
<!-- # Assuming 'solution_df' and adding a 'cost' column -->
<!-- solution_with_cost <- solution_df %>% -->
<!--   dplyr::mutate(cost = runif(dplyr::n(), 100, 1000)) -->
<!-- splnr_plot_costOverlay( -->
<!--   soln = solution_with_cost, -->
<!--   costName = "cost", # Name of the cost column -->
<!--   # costName = "Acquisition Cost", -->
<!--   # colorVals = c("grey", "darkgreen"), -->
<!--   # legendLabels = c("Not Selected", "Selected"), -->
<!--   plotTitle = "Prioritization Solution with Cost Overlay" -->
<!-- ) -->
<!-- ``` -->
<!-- 5. Plot Comparison -->
<!-- Function: `splnr_plot_comparison()` -->
<!-- Compares two different prioritization solutions, highlighting areas that are uniquely selected by each solution, or selected by both. -->
<!-- ```{r} -->
<!-- # Assuming two solutions for comparison -->
<!-- soln1_df <- solution_df %>%  -->
<!--   dplyr::mutate(solution_1 = sample(c(0, 1), dplyr::n(), replace = TRUE, prob = c(0.6, 0.4))) -->
<!-- soln2_df <- solution_df %>%  -->
<!--   dplyr::mutate(solution_1 = sample(c(0, 1), dplyr::n(), replace = TRUE, prob = c(0.5, 0.5))) -->
<!-- splnr_plot_comparison( -->
<!--   soln1 = soln1_df, -->
<!--   soln2 = soln2_df, -->
<!-- ) -->
<!-- ``` -->
<!-- 6. Plot Selection Frequency -->
<!-- Function: `splnr_plot_selectionFreq()` -->
<!-- Visualizes the frequency with which each planning unit is selected across multiple runs of a prioritization problem (e.g., from a sensitivity analysis or robust solution generation). -->
<!-- ```{r} -->
<!-- # # Create selection frequency data -->
<!-- # selection_freq_df <- solution_df %>% -->
<!-- #   dplyr::mutate( -->
<!-- #     selection_frequency = runif(dplyr::n(), 0, 1) # Frequency between 0 and 1 -->
<!-- #   ) -->
<!-- #  -->
<!-- # splnr_plot_selectionFreq( -->
<!-- #   selFreq = selection_freq_df, -->
<!-- #   plotTitle = "Selection Frequency of Planning Units", -->
<!-- #   legendTitle = "Selection Frequency" -->
<!-- # ) -->
<!-- ``` -->
<!-- 7. Plot Importance Score -->
<!-- Function: `splnr_plot_importanceScore()` -->
<!-- Calculates and visualizes an "importance score" for each planning unit, which can represent its contribution to achieving targets or its irreplaceability. Useful for identifying key areas. -->
<!-- ```{r} -->
<!-- #  -->
<!-- # splnr_plot_importanceScore( -->
<!-- #   soln = solution_df, -->
<!-- #   pDat = dat_species_bin, # Original biodiversity features -->
<!-- #   method = "basic", # Or "complex" -->
<!-- #   decimals = 2, -->
<!-- #   plotTitle = "Planning Unit Importance Score" -->
<!-- # ) -->
<!-- ``` -->
<!-- 8. Plot Correlation Matrix -->
<!-- Function: `splnr_plot_corrMat()` -->
<!-- Generates a correlation matrix plot, useful for understanding relationships between different features or variables in your planning data. -->
<!-- ```{r} -->
<!-- # # Create a data matrix for correlation -->
<!-- #  -->
<!-- # corr_data <- data.frame( -->
<!-- #   Var1 = rnorm(100), -->
<!-- #   Var2 = rnorm(100, mean = 0.5, sd = 1), -->
<!-- #   Var3 = rnorm(100, mean = -0.2, sd = 0.5) -->
<!-- # ) -->
<!-- # # Add some correlation -->
<!-- # corr_data$Var2 <- corr_data$Var2 + 0.5 * corr_data$Var1 -->
<!-- # corr_data$Var3 <- corr_data$Var3 - 0.3 * corr_data$Var1 -->
<!-- #  -->
<!-- # # Calculate correlation matrix -->
<!-- # corr_matrix <- cor(corr_data) -->
<!-- #  -->
<!-- # splnr_plot_corrMat( -->
<!-- #   matrix = corr_matrix, -->
<!-- #   plotTitle = "Correlation Matrix of Variables" -->
<!-- # ) -->
<!-- ``` -->
<!-- 9. Generic Plotting Utility -->
<!-- Function: `splnr_plot()` -->
<!-- A versatile plotting function that can be used to visualize any continuous or categorical data column within an sf dataframe, with options for color palettes and legend customization. Many other plotting functions in _spatialplanr_ internally use this function. -->
<!-- ```{r} -->
<!-- # Plotting the 'metric' column from 'dat_clim' using the generic plotter -->
<!-- splnr_plot( -->
<!--   df = dat_clim, -->
<!--   colNames = "metric", -->
<!--   plotTitle = "Generic Plot of Climate Metric", -->
<!--   legendTitle = "Metric Value", -->
<!--   paletteName = "viridis" # Use a viridis color palette -->
<!-- ) -->
<!-- ``` -->
<!-- # Example Workflow -->
<!-- The following example illustrates a typical workflow using _spatialplanr_ for climate-smart conservation planning. It assumes you have already prepared your planning units, biodiversity features, and cost layers. -->
<!-- A typical workflow using _spatialplanr_ for climate-smart conservation planning might look like this: -->
<!-- ## Data Preparation: -->
<!-- * Load your planning units (PUs). -->
<!-- * Load your biodiversity features (features). -->
<!-- * Load your cost layer (costs). -->
<!-- * Load your climate metric data (climate_data). -->
<!-- * (Optional) Use splnr_get_IUCNRedList() to refine species targets or splnr_get_gfw() to generate a cost layer. -->
<!-- Choose Climate-Smart Approach: -->
<!-- * Decide which climate-smart approach best suits your planning goals: -->
<!--   * `splnr_climate_priorityAreaApproach()` for prioritizing refugia for each feature. -->
<!--   * `splnr_climate_featureApproach()` for treating climate resilience as a standalone feature. -->
<!--   * `splnr_climate_percentileApproach()` for targeting specific climate metric ranges. -->
<!-- * Run the chosen function to obtain climate_features and climate_targets. -->
<!-- * Define and Solve Prioritization Problem (using _prioritizr_): -->
<!-- Create a prioritizr problem using your PUs, climate_features, and climate_targets. -->
<!-- * Add objectives (e.g., `add_min_set_objective()`, `add_max_targets_objective()`). -->
<!-- * Add constraints (e.g., `add_budget_constraint()`, `add_contiguity_constraint()`, `add_locked_in_constraint()` for existing MPAs). -->
<!-- * Add solvers (e.g., `add_gurobi_solver()`, `add_cbc_solver()`). -->
<!-- * Solve the problem to get your solution. -->
<!-- Visualize and Analyze Results (using _spatialplanr_ plotting functions): -->
<!-- * `splnr_plot_solution()` to visualize the selected planning units. -->
<!-- * `splnr_plot_costOverlay()` to see costs in selected areas. -->
<!-- * `splnr_plot_climKernelDensity()` to assess the climate characteristics of the solution. -->
<!-- * `splnr_plot_comparison()` to compare different scenarios or solutions. -->
<!-- * `splnr_plot_selectionFreq()` for robust solutions. -->
<!-- * `splnr_plot_importanceScore()` to identify key planning units. -->
<!-- * `splnr_plot_corrMat()` to understand feature relationships. -->
<!-- # Conclusion -->
<!-- _spatialplanr_ provides a powerful and flexible toolkit for integrating climate change considerations into spatial conservation prioritization. By offering multiple methodological approaches, streamlining data handling, and providing comprehensive visualization capabilities, it aims to empower conservation planners to create more resilient and effective protected area networks for the future. -->
