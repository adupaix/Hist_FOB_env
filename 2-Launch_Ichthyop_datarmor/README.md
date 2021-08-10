# Scripts to launch simulations on a cluster

## `jobs`

Files `.pbs` used to run simulations (`simu-ichthyop*`) and for pre-processing of Ichthyop results (`post-simu-ichthyop*`)

## `mpi_command_lists`

Files `.txt` containing the bash command lines used by the mpi wrapper to launch the simulations and the pre-processing

## Other files

`determine_missing_sim.R`: In case an error occurred in the cluster job, allows to determine which simulations have run and which did not

`ich_to_rds.R`: Script used for the outputs pre-processing (from .nc containing particles locations, to .rds containing 3D arrays with density maps)

`r-ichthyop.yml`: conda environment used to run pre-processing scripts
