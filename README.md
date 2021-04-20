# Historical FOB environment

Scripts pre (to generate input locations) and post (to build maps from simulations) Ichthyop simulations

## Post-Ichthyop

Inside a folder named after the simulation, builds a folder named as follow: "ltime[ltime_method]-[ltime]_w[weight_method]_gsize[gsize]_thr-disch[thr_disch]"

### Parameters

#### Script parameters:

* *ltime*: life time of logs to be used

* *ltime_method*: filter used for the life time : 1. probability to sink at each time step, so that mean(ltimes)=ltime; 2. life time fixed at ltime    

* *weight_method*: method used to give weight to input points
can be different from one only if the input_location is river;
1. homogeneous weight for each particle;
2. weight proportional to mean water discharge of rivers;
3. weight proportional to the percentage of forest cover in the river basin;
4. weight proportional to the surface of forest cover in the river basin.

* *gsize*: size of the map and array cells

* *thr_disch*: the mean water discharge of the input river is used as a filter. Only particles originating from a river with a value > thr_disch (in m3/s) are kept; 
if thr_disch = 0, only rivers with a mean discharge > 0 m3/s will be kept;
if you don't want any filter to be used, set thr_disch = NULL

* *agg.time_scale*: time scale used to aggregate the dates (day, month, quarter or year).
!! quarters start in December: for example, first quarter of 2012 is from 12.2011 to 02.2012 included

#### Ichthyop simulation parameters:

forcing, input_location, input_method, dist, timestep, dispersion, bouncing

#### "Technical" script parameters:

* *Parallel*: Run in parallel ?
    
    - First element of the vector: If F, runs in sequential. If T, runs in parallel
    - Second element of the vector: fraction of the cores to be used

!! One part of the script can rapidly fill the RAM memory, to be taken into account when choose to run in parallel or not
!! If the script is running on a Windows machine, the script is executed in sequential

* *RESET*: Whether to delete the results obtained for these arguments (T) or not (F): 1: arrays and matrices obtained with 1.read.ncs and maps obtained with 2.map.array. 2: only maps obtained with 2.map.array

#### Plot parameters:

log_color_scale, common_scale_max, color_scale_pos


### Outputs

* **1.global_array.rds** (3D array: longitude x latitude x timestep): contains a matrix with the number of particles per cell for each timestep. If several time scales are calculated, with the same parameters, the global array will be re-used.


* **Per_[agg.time_scale]_0.log.txt**: log files containing the time of execution, the simulation characteristics, the arguments used and the paths to the saved files


* **Per_[agg.time_scale]_2.mean_aggregated_array.rds** (3D array: longitude x latitude x (agg.time_scale/n_years)): contains a matrix with the **mean** number of particles per timescale chosen with agg.time_scale. For example, if agg.time_scale == "month", dim = long x lat x 12, with the first map containing the mean values for January (over the whole study period)

* **Per_[agg.time_scale]_2.mean_maps.rds**: list of ggplot objects, with the maps built from the above array


* **Per_[agg.time_scale]_3.aggregated_array.rds** (3D array: longitude x latitude x agg.time_scale): contains a matrix with the mean number of particles **for each** timescale chosen with agg.time_scale. For example, if agg.time_scale == "month", dim = long x lat x (12*number of years in the study), with the first map containing the mean values for January of Year1

* **Per_[agg.time_scale]_3.maps.rds** : list of ggplot objects, with the maps built from the above array


* **Per_[agg.time_scale]_maps.png** : image with maps in each panels. If agg.time_scale = quarter or month, build the maps from mean_agg_array.
If agg.time_scale = year, build the maps from the agg_array.

## develop:
	
* function to link rivers and basins in Post-Ichthyop

* ltime method 1 - crop beginning and end of simulation
	

# "Protocole" simulation historique

Début produit de forçage: 1980-01-01 à 12:00

Fin produit de forçage: 2009-12-31 à 12:00

Donc résultats obtenus du **01-06-1980** au **31-07-2009** (si on garde ltime = 180 jours, car on supprime début et fin de la simulation). Dernière simulations avec durée de dérive moins longue (sinon bug sur Ichthyop).

Relache ~ 18 703 particules (nombre d'entrées de rivière) par simulation (une tous les mois?). Et on fait 100 (?) réplicats de chacune des simulations. J'aurais bien fait 1000 réplicats, mais la sortie Ichthyop va être trop grosse.

Datarmor: 100 simulations avec même laché en séquentiel, et N périodes de laché en parallèle (N = 360, soit 30 ans x 12 mois).

## Estimations

* **Taille** des sorties Ichthyop:

|                         | Intervalle entre deux données: | 6h | 12h | 24h   |
|-------------------------|--------------------------------|----|-----|-------|
|Nombre de jours de dérive|                                |    |     |       |
|       360               |                                | 6Tb| 3Tb | 1.5Tb |
|       300               |                                | 5Tb|2.5Tb|1.26Tb |
|       180               |                                | 3Tb|1.5Tb| 752Gb |


* **Temps d'execution**, en heures (avec Datarmor, on a 28 processeurs, soit 168 coeurs, donc on lance trois run):

|                         | Intervalle entre deux données: | 6h | 12h | 24h   |
|-------------------------|--------------------------------|----|-----|-------|
|Nombre de jours de dérive|                                |    |     |       |
|       360               |                                |35.1| 35.1| 35.1  |
|       300               |                                |30  |30   | 30    |
|       180               |                                |20.1|20.1 | 20.1  |


* Autre possibilité pour gagner du temps et de la mémoire: appliquer un filtre sur les débit des rivières dès le début. Pas très fan, parce que si on change d'avis sur le filtre au milieu, il faut refaire tourner toute la simulation. De plus, les "rivières" avec des très faibles débit sont probablement des ravines qui relarguent des logs quand il y a une tempête, donc ça serait pas mal de les prendre en compte.

  - Si on supprime les entrées avec débit moyen = 0 m3/s : 17 728 particules (réduction de 5%)

  - Si on supprime les entrées avec débit moyen <= 0.1 m3/s : 15 271 particules (réduction de 18%)

  - Si on supprime les entrées avec débit moyen <= 0.2 m3/s : 11 479 particules (réduction de 39%)

