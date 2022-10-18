#################################################################################################
#' @title Environment builder for anonymized data
#' @author : Antoine Champetier
#' @date : 12.02.2022
#' @description: builds a set of well-conformed variables needed to run the set of simulation
#################################################################################################

### SET REPLICATION PARAMETERS ####
sim_replications = 20

### SET SIMULATION TIMING PARAMETERS ####
date_start = "2019-01-01" ## applied to anonymized TVD data for this test environment
simulation_steps = 50 ## number of time steps the simulation is run (here days).


### SET DISEASE PARAMETERS ####

compartment_list = c("susceptible", "latent","subclinical","clinical","removed")
compartment_infect = c(0,0,0.5,1,0) # this is the weight of each compartment in infectiousness relative to a clinical pig, used to within farm and across farm infections

compartment_num = length(compartment_list)
infected_compartments = c(compartment_list[3:compartment_num-1]) # select the compartments that count as infected for incidence and prevalence

probas_infections_param <- c("self" = 0.9,
                            "p2p"= 0.2,
                            "f"= 0.1,
                            "t"= 0.1,
                            "v"= 0.02,
                            "s" = 0.2 )

space_cutoff = 2000 # cutoff distance for space contacts.
distance_param_haltung = 200 # distance-equivalent Haltings form step (e.g. with distance_param = 200, Haltungsform 4 is equivalent to 200 meters, 3 like 400, 2 like 600, )
distance_param_wb_to_wb = 200 # distance-equivalent for wild boards next to each other

resimul_tours = FALSE # to re-simulate the sequence of tours (transport and vet)




### LOAD DATA IMPORT FUNCTIONS #####
source("all_data_import_functions.R") # most of them are not used in this template version since data is generated
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


#source("mk_index_case_italy_border.R")
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





### Import anonymized TVD data ####
tranport_data <- read.csv("pseudo_tvd19_entry.csv")

### Select period for simulation ####
tranport_data$date <- ymd(tranport_data$Event_Date)
tranport_data <- tranport_data[tranport_data$date >= ymd(date_start) & tranport_data$date <= ymd(date_start) + days(simulation_steps), ]

### Generate list of ID for vertices from what is in the transport data ####
sources_list <- unique(tranport_data$ID_ANH_Source)
destinations_list <- unique(tranport_data$ID_ANH_Dest)
vertex_list <- unique(c(sources_list, destinations_list))

### Generating characteristics for farms (not all required) ####
vertex_parameters <- data.frame("ID_vertex" = c(1:(length(vertex_list))),"TVD_ID" = vertex_list)
vertex_parameters$type <- "farm"
vertex_parameters$haltungsform <- sample(c(1,2,3,4),nrow(vertex_parameters), replace = TRUE)
vertex_parameters$farm_type_ML <- sample(c("Breed_repl","Ring_ins","Fat_lfreq" ,"Breed_norepl", "Nucleus","Ring_farr","Breed_10kg","Multiplier" ,"Fat_10kg" , "Fat_hfreq",""),nrow(vertex_parameters), replace = TRUE)
vertex_parameters$gemeinde <- sample(c(1:1000),nrow(vertex_parameters), replace = TRUE)
rm(destinations_list, vertex_list, sources_list)


###  Building a parameter data frame for the wild boar units  ####
nb_wb <- 500 # number of wild boar units
vertex_wb <- data.frame("ID_vertex" = c(90000:(90000+nb_wb-1)),
                        "TVD_ID" = rep(NA,nb_wb),
                        "type" =   rep("wildboar",nb_wb),
                        "haltungsform"  = rep(NA,nb_wb),
                        "farm_type_ML" = rep(NA,nb_wb),
                        "gemeinde" = sample(c(1:900),nb_wb, replace = TRUE))  ## here we are only drawing from 900 Gemeinde which means that there will be up to 100 Gemeinde with farms and no wild boar


###  Combining all vertex parameters and creating a key of vertices ####
vertex_parameters <- rbind(vertex_parameters, vertex_wb)
  
vertex_key <- data.frame(ID_vertex = vertex_parameters$ID_vertex)
rm(vertex_wb, nb_wb)

###  Generate the changes in populations at each time step for each vertex ####
vertex_pop <- matrix(runif((simulation_steps+1)*nrow(vertex_key),min = -2, max = 10), nrow = simulation_steps+1, ncol = nrow(vertex_key))




###  Generate the variables for compartments for each vertex and initialize susceptible to starting population ####
init_vertex_variable <- data.frame("ID_vertex" = vertex_key$ID_vertex, 
                                   "susceptible" = runif(nrow(vertex_key),min = 100, max = 3000))
for(comp in 1:(compartment_num+1)){
  init_vertex_variable <- cbind(init_vertex_variable, rep(0,nrow(vertex_key)))
}
names(init_vertex_variable)<-c("ID_vertex",compartment_list, "status_infected_pigs","infectiousness")



###  Extract the transport edge list from the anonymized data ####
tranport_data$t_step <- as.numeric(difftime(tranport_data$date , ymd(date_start),units = "days"))
tranport_data <- left_join(tranport_data, vertex_parameters[, c("ID_vertex"  ,  "TVD_ID" )], by  = c( "ID_ANH_Dest" = "TVD_ID"))
tranport_data <- left_join(tranport_data, vertex_parameters[, c("ID_vertex"  ,  "TVD_ID" )], by  = c( "ID_ANH_Source" = "TVD_ID"))

transport_network_edges <- tranport_data[, c("t_step", "N_Pigs", "ID_vertex.x","ID_vertex.y") ]
names(transport_network_edges) = c( "t_step" , "n_pigs","ID_vertex_source", "ID_vertex_dest")



###  Generate contacts ####
# note that this a simplified version and the contacts are not consistent with for space and transport logic
density_space <- 0.1
edge_num <- (density_space*nrow(vertex_key))^2
contact_network_edges <- data.frame("ID_vertex_source" = sample(vertex_key$ID_vertex, edge_num, replace = TRUE),
                                   "ID_vertex_dest" = sample(vertex_key$ID_vertex, edge_num, replace = TRUE),
                                   "t_step" = rep("Inf",edge_num),
                                   "n_susceptibles" = rep(NA,edge_num),
                                   "infectiousness_factor" = runif(edge_num, min = 1, max = 2000),
                                   "contact_type" = rep("s",edge_num))
density_vet<- 0.01
edge_num <- (density_vet*nrow(vertex_key))^2
contact_network_edges_vet <- data.frame("ID_vertex_source" = sample(vertex_key$ID_vertex, edge_num, replace = TRUE),
                                   "ID_vertex_dest" = sample(vertex_key$ID_vertex, edge_num, replace = TRUE),
                                   "t_step" = sample( 1:simulation_steps, edge_num, replace = TRUE ),
                                   "n_susceptibles" = rep(NA,edge_num),
                                   "infectiousness_factor" = rep(NA,edge_num),
                                   "contact_type" = rep("v",edge_num))

edge_num <- nrow(tranport_data)*10
contact_network_edges_tours<- data.frame("ID_vertex_source" = sample(vertex_key$ID_vertex, edge_num, replace = TRUE),
                                        "ID_vertex_dest" = sample(vertex_key$ID_vertex, edge_num, replace = TRUE),
                                        "t_step" = sample( 1:simulation_steps, edge_num, replace = TRUE ),
                                        "n_susceptibles" = sample( 1:max(tranport_data$N_Pigs ), edge_num, replace = TRUE ),
                                        "infectiousness_factor" = rep(NA,edge_num),
                                        "contact_type" = sample( c( "t","f" ,"p2p" ), edge_num, replace = TRUE ))

    
contact_network_edges <- rbind(contact_network_edges,contact_network_edges_vet)
contact_network_edges <- rbind(contact_network_edges,contact_network_edges_tours)

# given the brute force used in generating the contacts, we remove at least the edges where the destination and source are the same
contact_network_edges <- contact_network_edges[contact_network_edges$ID_vertex_source != contact_network_edges$ID_vertex_dest,]


###  Generate contacts ####
surveillance_schedule <- mk_surveillance(vertex_key, surveillance_parameter, surveillance_policy)





###  Generate a random probability of being index case ####
index_case_probabilities <- data.frame("ID_vertex" =  vertex_key$ID_vertex,
                                        "probability" = runif(nrow(vertex_key), min = 0, max = 1)) 


index_case_probabilities <- mk_index_case(vertex_key,index_case_scenario, index_case_parameter)



