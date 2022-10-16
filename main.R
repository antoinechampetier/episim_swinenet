#######################################
#' @title Main Simulation development module  SWINENET
#' @author : Antoine Champetier
#' @date : 21.12.2021
#' @description: designed to check run of simulation from final data and variable formats
#######################################


## SET DIRECTORIES AND PACKAGE-LIBRARIES####
rm(list = ls())
Sys.setenv(LANG = "en")
start_time <- Sys.time()
getwd()
#setwd("./episim_swinenet")
#setwd("~/swinenet_episim/episim_swinenet")
relative_path_to_output = "../simulation_outputs/"
relative_path_to_scenarios = "../simulation_inputs/"
relative_path_to_data = "../epi_data/"

suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(tidyverse))
suppressMessages(library(ggplot2))
suppressMessages(library(matrixStats))
suppressMessages(library(lubridate))
suppressMessages(library(reshape2))

## PRE-PROCESSING SCRIPTS (not run every time)  ####
# source( "pre_process_import_source_AGIS_TVD_trader.R" )
# source( "pre_process_agis_labor_surface.R" )
# source( "pre_process_AGIS_TVD_matching.R" )
# source( "pre_process_population.R" )
# source( "pre_process_wildboar.R" )
# source( "pre_process_contact_networks.R" )

## LOOP THROUGH THE SCENARIOS ####
for(scenario in list.files(relative_path_to_scenarios)){ 
    print(scenario)
    ## LOAD ENVIRONMENT FOR CURRENT SCENARIO
    source(paste(relative_path_to_scenarios,scenario,sep = "")) 
  
    ## INITIALISE OUTPUT OF SIMULATION SET : ####
    sim_output <- data.frame(t_step=integer(), 
                         prevalence_farm=numeric(),
                         prevalence_wb_units=numeric(),
                         incidence_farm=numeric(),
                         incidence_wb_units=numeric(),
                         prevalence_pigs=numeric(),
                         prevalence_wb=numeric(),
                         incidence_pigs=numeric(),
                         incidence_wb=numeric(),
                         detections= numeric(),
                         rep_number = numeric()) 

    output_infected_id <- c()
    output_detected_id <- c()
    
    ## RUN REPLICATIONS ####
    for(rep_number in 1:sim_replications){ 
  
         print(paste(rep_number, " replication out of ", sim_replications,sep = ""))
 
         source("mk_simulation.R") # run one simulation

         sim_output<-rbind(sim_output,cbind(output_run, "replication" = rep(rep_number, nrow(output_run))) ) ## add outputs of replication to simulation output
    }



    ## SAVE OUTPUT TO FILE AND MAKE PLOT #### Note it all goes into the output_simulation folder ####
    dir.create(paste(relative_path_to_output,scenario,sep="")) # make a simulation folder with the name of the input file
    file.copy(file.path(relative_path_to_scenarios, scenario), paste(relative_path_to_output,"/",scenario,sep=""), overwrite = TRUE) # including a copy of the input scenario file
    save(sim_output, file = paste(relative_path_to_output,"/",scenario,"/","output_simulation_set.RData",sep=""))  
    unlink(file.path(relative_path_to_scenarios, scenario))
    source("mk_plot_simulation_set.R")
    # for some reason the plots dont work on UBELIX, so it has to be run separately after run on ubelix using mk_post_plot.R.


}


## CHECK COMPUTATION TIME ####
end_time <- Sys.time()
print(paste("Total computation time for set of ", sim_replications ," simulations: ",round(as.numeric((end_time - start_time), units="mins"),2), " minutes",sep=""))
print(paste("Average computation time per simulation: " ,round(as.numeric((end_time - start_time) /sim_replications, units="secs"),2), " seconds",sep=""))

