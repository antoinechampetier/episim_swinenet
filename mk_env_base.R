#################################################################################################
#' @title Environment builder
#' @author : Antoine Champetier
#' @date : 12.02.2022
#' @description: builds a set of well-conformed variables needed to run the set of simulation
#################################################################################################

### SET REPLICATION PARAMETERS ####
sim_replications = 300

### SET SIMULATION TIMING PARAMETERS ####
#time_step = "days" 
date_start = "2019-01-02" 
simulation_steps = 300 ## number of time steps the simulation is run (here days).


### SET DISEASE PARAMETERS ####

compartment_list = c("susceptible", "latent","subclinical_0","subclinical_1","subclinical_2","clinical_0","clinical_1", "carcass", "removed")
compartment_infect = c(0,0,0.5,0.6,0.8,1,1,1,0) # this is the weight of each compartment in infectiousness relative to a clinical pig, used to within farm and across farm infections

compartment_num = length(compartment_list)
infected_compartments = c(compartment_list[2:(compartment_num-2)]) # select the compartments that count as infected for incidence and prevalence


carcass_decay_wild_boar = 0.1 # probability of each carcass being removed per day in a binomial draw


probas_infections_param <- c("self" = 0.9,
                            "p2p"= 0.2,
                            "f"= 0.1,
                            "t"= 0.1,
                            "v"= 0.02,
                            "s" = 0.2 )

space_cutoff = 2000 # cutoff distance for space contacts.
distance_param_haltung = 400 # distance-equivalent Haltings form step (e.g. with distance_param = 200, Haltungsform 4 is equivalent to 200 meters, 3 like 400, 2 like 600, )
distance_param_wb_to_wb = 400 # distance-equivalent for wild boards next to each other

wildboar_repopulation_speed = .1 #  fraction of base wild boar population that gets replenished per day.

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

### LOAD SIMULATION FUNCTIONS ####
source("all_simulation_functions.R")
# f_inoculate
# f_transport
# # f_contact_infection
# f_farm_infection
# f_population
# f_prevalence
# f_detection


### Defining index case  ####
# Un-comment one of the following for non-default index case
#source("scenarios_index_case/mk_index_case_italy_border.R")
#source("scenarios_index_case/mk_index_case_labor.R")
source("scenarios_index_case/mk_index_case_rest_area.R")
index_case_parameter = 20 # used to set the probabilities of index cases from the vertex property for the scenario
index_case_number = 100 # the maximum number of units infected as index case. the probabilities of index case are still used. See f_inoculate in all_simulaiton_functions.R




### Defining non default surveillance ####
# Un-comment one of the following for non-default surveillance
source("scenarios_surveillance/mk_surveillance_farm_morbidity.R")
#source("scenarios_surveillance/mk_surveillance_farm_mortality.R")
surveillance_parameter = 0.0500 # for random is it the share of vertices tested each time period.

# Parameters for the tests of type 1 and 2: 

tests_parameters  <- data.frame("type" = as.character(),
                                "share_tested" = as.numeric(),
                                "number_tested" = as.numeric(),
                                "sensitivity" = as.numeric(),
                                "specificity" = as.numeric(),
                                "other_parameter_1" = as.numeric(),
                                "other_parameter_2" = as.numeric() )

tests_parameters[1,] = c(1, 1, NA, 1, 1, 5, 0.05) # param 1 is number of animals in clinical, param 2 is share of animals in clinical
tests_parameters$type[1] = "farmer_daily_morbidity"
tests_parameters[2,] = c(2, 1, NA, 1, 1, 2, 0.035) # param 1 is number of animals in carcass, param 2 is share of animals in carcass
tests_parameters$type[2] = "farmer_daily_mortality"
tests_parameters[3,] = c(3, 1, NA, 0.95, 1, NA, NA)
tests_parameters$type[3] = "vet_visit"






### RUN DATA IMPORT FUNCTIONS FROM PRE-PROCESSED DATA ####
relative_path_to_processed_data = "../epi_data/processed/"
# The reference lists are used get AGIS, TVD and Swinenet_ID correspondences
load(paste(relative_path_to_processed_data,"reference_list_swinenet_AGIS_TVD_ids.RData",sep=""))
load(paste(relative_path_to_processed_data,"id_reference_flat_AGIS.RData",sep ="" ))
load(paste(relative_path_to_processed_data,"id_reference_flat_TVD.RData",sep ="" ))


vertex_key <- mk_vertex_key(date_start, simulation_steps, paste(relative_path_to_processed_data,"activity_dates_final.RData",sep=""))



vertex_parameters <- mk_vertex_param(vertex_key, 
                                     paste(relative_path_to_processed_data,"parameters_all_final.RData",sep ="" ),
                                     paste(relative_path_to_processed_data,"wildboar_units.Rdata",sep ="" ))


# this is an ad hoc fix that removes some of the vertex that donät have proper parameter information
vertex_key  <- vertex_key[ - which(!(vertex_key$ID_vertex %in% vertex_parameters$ID_vertex)),]  



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


surveillance_schedule <- mk_surveillance(vertex_key, surveillance_parameter)

index_case_probabilities <- mk_index_case(vertex_key, index_case_parameter)





