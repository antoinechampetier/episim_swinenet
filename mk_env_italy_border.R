#################################################################################################
#' @title Environment builder
#' @author : Antoine Champetier
#' @date : 12.02.2022
#' @description: builds a set of well-conformed variables needed to run the set of simulation
#################################################################################################

### SET REPLICATION PARAMETERS ####
sim_replications = 500

### SET SIMULATION TIMING PARAMETERS ####
time_step = "days" ## remove from here later?
date_start = "2019-05-01" ## applied to anonymized TVD data for this test environment
simulation_steps = 200 ## number of time steps the simulation is run (here days).


### SET DISEASE PARAMETERS ####
compartment_list = c("susceptible", "latent","subclinical_1","subclinical_2","clinical", "immune", "carcass","removed")
compartment_infect = c(0,0,0.6,0.8,1,0) # this is the weight of each compartment in infectiousness relative to a clinical pig, used to within farm and across farm infections

compartment_list = c("susceptible", "latent","subclinical_0","subclinical_1","subclinical_2","clinical_0","clinical_1", "removed")
compartment_infect = c(0,0,0.5,0.6,0.8,1,1,0) # this is the weight of each compartment in infectiousness relative to a clinical pig, used to within farm and across farm infections

compartment_num = length(compartment_list)
infected_compartments = c(compartment_list[3:compartment_num-1]) # select the compartments that count as infected for incidence and prevalence

transition_proba.farm= c(0,rep(1,compartment_num-2),0)
transition_proba.wildboar=c(0,rep(1,compartment_num-2),0)
transition_proba = list("farm" = transition_proba.farm, "wildboar" = transition_proba.wildboar)


probas_infections_param <- c("self" = 0.9,
                            "p2p"= 0.2,
                            "f"= 0.1,
                            "t"= 0.1,
                            "v"= 0.02,
                            "s" = 0.2 )

space_cutoff = 2000 # cutoff distance for space contacts.
distance_param_haltung = 200 # distance-equivalent Haltings form step (e.g. with distance_param = 200, Haltungsform 4 is equivalent to 200 meters, 3 like 400, 2 like 600, )
distance_param_wb_to_wb = 200 # distance-equivalent for wild boards next to each othe

resimul_tours = FALSE # to re'simulate the sequence of tours (transport and vet)




### LOAD DATA IMPORT FUNCTIONS ####
source("all_data_import_functions.R")
# Default functions present in "all_data_import_functions.R"
# mk_vertex_key
# mk_vertex_param
# mk_vertex_pop
# mk_vertex_variables
# mk_transport
# mk_contact_network
# mk_index_case
# mk_surveillance

### Defining non default index case  ####


source("mk_index_case_italy_border.R")
#source("mk_index_case_labor.R")
#source("mk_index_case_rest_area.R")
index_case_scenario = "defaut"
index_case_parameter = 20



### Defining non default surveillance ####
#source("mk_surveillance_target_ring.R")
surveillance_policy = "random"
surveillance_parameter = 0.0500 # for random is it the share of vertices tested each time period.

# Parameters for the tests of type 1 and 2: 
tests_sensitivities = list("1" = 0.95, "2" = 0.95)
share_animal_tested = list("1" = 0.30, "2" = 0.3)



### LOAD SIMULATION FUNCTIONS ####
source("all_simulation_functions.R")
# f_inoculate
# f_transport
# # f_contact_infection
# f_farm_infection
# f_population
# f_prevalence
# f_detection



### RUN DATA IMPORT FUNCTIONS FROM PRE-PROCESSED DATA ####
relative_path_to_processed_data = "../epi_data/processed/"
# The reference lists are used get AGIS, TVD and Swinenet_ID correspondences
load(paste(relative_path_to_processed_data,"reference_list_swinenet_AGIS_TVD_ids.RData",sep=""))
load(paste(relative_path_to_processed_data,"id_reference_flat_AGIS.RData",sep ="" ))
load(paste(relative_path_to_processed_data,"id_reference_flat_TVD.RData",sep ="" ))


vertex_key <- mk_vertex_key(date_start, simulation_steps, paste(relative_path_to_processed_data,"activity_dates_final.RData",sep=""))

# old fix, check if needed..
# vertex_key  <- vertex_key[ vertex_key$ID_vertex != 32273,]  



vertex_parameters <- mk_vertex_param(vertex_key, 
                                     paste(relative_path_to_processed_data,"parameters_all_final.RData",sep ="" ),
                                     paste(relative_path_to_processed_data,"wildboar_units.Rdata",sep ="" ))

vertex_pop <- mk_vertex_pop(vertex_key, date_start, simulation_steps, paste(relative_path_to_processed_data,"pop_change_vertex_final.RData",sep=""))

init_vertex_variable <- mk_vertex_variables(vertex_key, 
                                            date_start, 
                                            compartment_list, 
                                            paste(relative_path_to_processed_data,"pop_vertex_final.RData",sep=""),
                                            paste(relative_path_to_processed_data ,"wildboar_units.Rdata",sep =""))


transport_network_edges <- mk_transport(paste(relative_path_to_processed_data ,"TVD_edge_swinenet.RData",sep =""),vertex_key,date_start,simulation_steps )



contact_network_edges<- mk_contact_network(vertex_key,
                                           date_start,
                                           simulation_steps,
                                           space_cutoff,
                                           distance_param_haltung,
                                           distance_param_wb_to_wb,
                                           paste(relative_path_to_processed_data,"spatial_data_final.RData",sep=""),
                                           paste(relative_path_to_processed_data,"tour_TVD_final.RData",sep=""),
                                           paste(relative_path_to_processed_data,"tour_vet_final.RData",sep=""),
                                           resimul_tours)


surveillance_schedule <- mk_surveillance(vertex_key, surveillance_parameter, surveillance_policy)

index_case_probabilities <- mk_index_case(vertex_key,index_case_scenario, index_case_parameter)





