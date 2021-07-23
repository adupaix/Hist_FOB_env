# Historical FOB environment

Scripts to perform simulations of NLOGs distributions from 1990 to 2020

The study uses __Ichthyop__ (available here: [website](www.ichthyop.org) ; [github](https://github.com/ichthyop/ichthyop)) and the __Ichthyop mpi wrapper__ (available here: [github](https://github.com/ichthyop/ichthyop-mpi))

## `0-Generate_river_shp`

Script to generate a river template used to map forest cover around the Indian Ocean

## `1-Input_Ichthyop`

Scripts to generate a data frame containing the coordinates of the input locations for Ichthyop

Also generates Ichthyop cfg files, using a provided template

## `2-Launch_Ichthyop_datarmor`

Scripts used to launch Ichthyop simulations and to pre-process Ichthyop results, on a cluster.

## `3-Post_Ichthyop`

Scripts to process Ichthyop outputs, using precipitation, forest cover, and river discharge data
