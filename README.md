# Historical FOB environment

Scripts pre (to generate input locations) and post (to build maps from simulations) Ichthyop simulations

## Post-Ichthyop

Inside a folder named after the simulation, builds a folder named as follow: "ltime[ltime_method]-[ltime]_w[weight_method]_gsize[gsize]_thr-disch[thr_disch]"

The different parameters used are:

* ltime: life time of logs to be used

* ltime_method: filter used for the life time : 1. probability to sink at each time step, so that mean(ltimes)=ltime; 2. life time fixed at ltime    

* weight_method:  method used to give weight to input points
can be different from one only if the input_location is river;
1. homogeneous weight for each particle;
2. weight proportional to mean water discharge of rivers;
3. weight proportional to the percentage of forest cover in the river basin;
4. weight proportional to the surface of forest cover in the river basin.

* gsize: size of the map and array cells

* thr_disch: the mean water discharge of the input river is used as a filter. Only particles originating from a river with a value > thr_disch (in m3/s) are kept; 
if thr_disch = 0, only rivers with a mean discharge > 0 m3/s will be kept;
if you don't want any filter to be used, set thr_disch = NULL

* agg.time_scale: time scale used to aggregate the dates (day, month, quarter or year).
!! quarters start in December: for example, first quarter of 2012 is from 12.2011 to 02.2012 included

## develop:
	
* function to link rivers and basins in Post-Ichthyop
	
* apply ltime method 1
