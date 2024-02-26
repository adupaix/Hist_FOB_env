# Historical floating objects distributions

Scripts to perform simulations of natural floating objects (NLOGs) distributions from 2000 to 2020

The study uses __Ichthyop__ (available here: [website](https://www.ichthyop.org) ; [github](https://github.com/ichthyop/ichthyop)) and the __Ichthyop mpi wrapper__ (available here: [github](https://github.com/ichthyop/ichthyop-mpi))

## `0-Generate_river_shp`

Script to generate a river template used to map forest cover around the Indian Ocean

## `1-Input_Ichthyop`

Scripts to generate a data frame containing the coordinates of the input locations for Ichthyop.
Also generates Ichthyop cfg files and .pbs scripts (using provided templates) to run Ichthyop simulations on a cluster.

## `2-Post_Ichthyop`

Scripts to process Ichthyop outputs, using precipitations, coastline, forest cover, and river discharge data.
Generate matrices containing the simulated NLOG densities, for each scenarios, on 3 dimensions (longitude, latitude and time).

## `3-Simulation_processing`

Scripts using the outputs from 2-Post_Ichthyop to generate trends of simulated NLOG numbers (`NLOG_trends.R`), compare the simulation ouputs with observers data (`Comparison_with_data.R`), build maps of simulated densities (`Map_density.R`) and of the study areas (`Map_areas.R`).

## `4-Data_description`

Build maps and time series of the precipitations (`temporal_trend_precipitations.R`), times series of the forest cover (`temporal_trend_forest_cover.R`) and of the total input of the different weighting scenarios (`temporal_trend_scenarios.R`)
