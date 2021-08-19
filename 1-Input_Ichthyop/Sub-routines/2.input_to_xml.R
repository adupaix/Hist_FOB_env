#'#*******************************************************************************************************************
#'@author : Amael DUPAIX
#'@update : 2021-08-19
#'@email : 
#'#*******************************************************************************************************************
#'@description :  Sub-routine generating Ichthyop config files (xml) from the input_icht.txt
#'#*******************************************************************************************************************
#'@revision
#'#*******************************************************************************************************************

  
cat(crayon::bold("\n\n2. Generating xml cfg files from points\n"))
  
#'@1 Generate xml config files
if (!all(Exist$output_2)){
  # get the number of directories to generate
  n_dirs <- floor(dim(input_coords_id)[1]/n_cfg_per_dir)
  
  n_iter <- dim(input_coords_id)[1]
  pb <- txtProgressBar(min = 0, 
                       max = n_iter,
                       style = 3)
  
  # generate the initial times from the start/end dates and the frequency
  yrs = seq(first_release_year, last_release_year)
  mths = 1:12
  dys = seq(1, 30-30/release_frequency, length.out = release_frequency)
  
  initial_time <- c(as.Date(paste(rep(first_release_year-1,2),
                                           rep("12",2),
                                           c("01","15"),sep = "-")),
                    as.Date(paste(rep(yrs, each = length(mths)*length(dys)),
                                           rep(mths, length(yrs), each = length(dys)),
                                           rep(dys, length(mths)*length(yrs)),
                                           sep = "-")),
                    as.Date(paste(rep(last_release_year+1,2),
                                           rep("01",2),
                                           c("01","15"),sep = "-"))
                     )
  
  #~ Read the xml template file
  tplate <- scan(Template$xml, what = "", sep = "\n", quiet = T)
  
  for (i in 1:n_dirs){
    
    t <- paste(rep(0, 2), collapse = "")
    i_chr <- as.character(i)
    i_chr <- paste0(substr(t, 1, nchar(t)-nchar(i_chr)), i_chr)
    
    dir_name.i <- file.path(cfg_path, paste0("cfgs_", i_chr))
    
    dir.create(dir_name.i)
    
    for (j in 1:n_cfg_per_dir){
      long <- input_coords_id[n_cfg_per_dir*(i-1)+j,1]
      lat <- input_coords_id[n_cfg_per_dir*(i-1)+j,2]
      cfg_nb <- input_coords_id[n_cfg_per_dir*(i-1)+j,3]
      
      #~ Transform cfg_nb into "000xx" format
      t <- paste(rep(0, 5), collapse = "")
      cfg_nb <- as.character(cfg_nb)
      nb <- paste0(substr(t, 1, nchar(t)-nchar(cfg_nb)), cfg_nb)
      
      #~ Create the xml cfg for ichthyop
      write.cfg.xml(initial_time,
                    tplate,
                    sim_input_path,
                    sim_output_path,
                    record_frequency,
                    dir_name.i,
                    i_chr,
                    
                    long, lat,
                    nb
      )
      
      setTxtProgressBar(pb, n_cfg_per_dir*(i-1)+j)
    }
    
  }
  
  i=n_dirs+1
  t <- paste(rep(0, 2), collapse = "")
  i_chr <- as.character(i)
  i_chr <- paste0(substr(t, 1, nchar(t)-nchar(i_chr)), i_chr)
  
  dir_name.i <- file.path(cfg_path, paste0("cfgs_", i_chr))
  
  dir.create(dir_name.i, showWarnings = F)
  
  for (j in 1:(n_iter - n_dirs*n_cfg_per_dir)){
    long <- input_coords_id[n_cfg_per_dir*(i-1)+j,1]
    lat <- input_coords_id[n_cfg_per_dir*(i-1)+j,2]
    cfg_nb <- input_coords_id[n_cfg_per_dir*(i-1)+j,3]
    
    #~ Transform cfg_nb into "000xx" format
    t <- paste(rep(0, 5), collapse = "")
    cfg_nb <- as.character(cfg_nb)
    nb <- paste0(substr(t, 1, nchar(t)-nchar(cfg_nb)), cfg_nb)
    
    #~ Create the xml cfg for ichthyop
    write.cfg.xml(initial_time,
                  tplate,
                  sim_input_path,
                  sim_output_path,
                  record_frequency,
                  dir_name.i,
                  i_chr,
                  
                  long, lat,
                  nb
    )
    
    setTxtProgressBar(pb, n_cfg_per_dir*(i-1)+j)
    
  }
  
  close(pb)
  
  #'@2 Generate command lists and pbs jobs
  
  generate.command.list(sim_input_path, cfg_path, cfg_dir, n_pbs_jobs)
  
  generate.jobs.pbs(Template$pbs, sim_input_path, cfg_path, cfg_dir, last_release_year,
                    n_pbs_jobs, n_mpi, walltime)


} else {
  
  cat("\n### Config files, commands lists and pbs jobs already generated\n")
  
}