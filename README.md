# Historical floating objects distributions

Scripts to perform simulations of natural floating objects (NLOGs) distributions from 1990 to 2020

The study uses __Ichthyop__ (available here: [website](https://www.ichthyop.org) ; [github](https://github.com/ichthyop/ichthyop)) and the __Ichthyop mpi wrapper__ (available here: [github](https://github.com/ichthyop/ichthyop-mpi))

## `0-Generate_river_shp`

Script to generate a river template used to map forest cover around the Indian Ocean

## `1-Input_Ichthyop`

Scripts to generate a data frame containing the coordinates of the input locations for Ichthyop

Also generates Ichthyop cfg files and .pbs scripts to run Ichthyop simulations on a cluster, using provided templates

## `2-Post_Ichthyop`

Scripts to process Ichthyop outputs, using precipitations, coastline, forest cover, and river discharge data
