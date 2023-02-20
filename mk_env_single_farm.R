#################################################################################################
#' @title Environment builder for single farm
#' @author : Antoine Champetier
#' @date : 12.02.2023
#' @description: builds a set of well-conformed variables needed to run the set of simulation for single farm
#################################################################################################

### SET REPLICATION PARAMETERS ####
sim_replications = 5

### SET SIMULATION TIMING PARAMETERS ####
date_start = "2019-01-01" ## applied to anonymized TVD data for this test environment
simulation_steps = 200 ## number of time steps the simulation is run (here days).


### SET DISEASE PARAMETERS ####

compartment_list = c("susceptible", "latent","subclinical_1","subclinical_2", "clinical","carcass","removed")
compartment_infect = c(0,0,0.5,0.6,1,0.01,0) # this is the weight of each compartment in infectiousness relative to a clinical pig, used to within farm and across farm infections


compartment_num = length(compartment_list)
infected_compartments = c(compartment_list[3:compartment_num-1]) # select the compartments that count as infected for incidence and prevalence

probas_infections_param <- c("self" = 0.1,
                             "p2p"= 0.2,
                             "f"= 0.1,
                             "t"= 0.1,
                             "v"= 0.02,
                             "s" = 0.2 )

space_cutoff = 2000 # cutoff distance for space contacts.
distance_param_haltung = 200 # distance-equivalent Haltings form step (e.g. with distance_param = 200, Haltungsform 4 is equivalent to 200 meters, 3 like 400, 2 like 600, )
distance_param_wb_to_wb = 200 # distance-equivalent for wild boards next to each other

resimul_tours = FALSE # to re-simulate the sequence of tours (transport and vet)

#source("mk_index_case_rest_area.R")
index_case_scenario = "defaut"
index_case_parameter = 20

#source("mk_surveillance_target_ring.R")
surveillance_policy = "random"
surveillance_parameter = 0.0500 # for random is it the share of vertices tested each time period.

# Parameters for the tests of type 1 and 2: 
tests_sensitivities = list("1" = 0.95, "2" = 0.95)
share_animal_tested = list("1" = 0.30, "2" = 0.3)

transition_proba = data.frame(
  "farm" =     c(0, .6,.6,.6,.6, 0,0),
  "wildboar" = c(0, .6,.6,.6,.6,.01,0)
)
  
  transition_proba$farm

source("all_simulation_functions.R")

source("mk_f_population_transition.R")

vertex_key = data.frame(
  "ID_vertex" = as.integer(1)
)

index_case_probabilities = data.frame("ID_vertex" = c(as.integer(1)),
                                      "probability" = c(1))

index_case_probabilities = data.frame("ID_vertex" = c(as.integer(1),as.integer(2)),
                                      "probability" = c(1,1))

vertex_parameters = data.frame(
  "ID_vertex" =  c(as.integer(1)),
  "TVD_ID"     =  c(as.integer(1)),
  "type"     =  c(as.character("farm")),
  "haltungsform" = as.numeric(2),
  "farm_type_ML" =  c(as.character("farm")),
  "gemeinde"  =  c(as.integer(1))
)

vertex_parameters = data.frame(
  "ID_vertex" =  c(as.integer(1),as.integer(2)),
  "TVD_ID"     =  c(as.integer(1),NA),
  "type"     =  c(as.character("farm"),as.character("wildboar")),
  "haltungsform" = c(as.numeric(2),NA),
  "farm_type_ML" =  c(as.character("farm"),NA),
  "gemeinde"  =  c(as.integer(1),as.integer(2))
)


transport_network_edges = data.frame(
  "t_step"    = as.integer(),      
  "n_pigs"     = as.integer(),   
  "ID_vertex_source"= as.integer(),
  "ID_vertex_dest"  = as.integer()
)

init_vertex_variable = data.frame(
  "ID_vertex"     = as.integer(1),
  "susceptible"   = as.numeric(100),
  "latent"        = as.numeric(0),
  "subclinical_1"   = as.numeric(0),
  "subclinical_2"   = as.numeric(0),
  "clinical"      = as.numeric(0),
  "carcass"      = as.numeric(0),
  "removed"      = as.numeric(0),
  "status_infected_pigs" = as.numeric(0),
  "infectiousness"    = as.numeric(0)
)


init_vertex_variable = data.frame(
  "ID_vertex"     =  c(as.integer(1),as.numeric(2)),
  "susceptible"   = c(as.numeric(100),as.numeric(100)),
  "latent"        = c(as.numeric(0),as.numeric(0)),
  "subclinical_1"   = c(as.numeric(0),as.numeric(0)),
  "subclinical_2"   = c(as.numeric(0),as.numeric(0)),
  "clinical"      = c(as.numeric(0),as.numeric(0)),
  "carcass"      = c(as.numeric(0),as.numeric(0)),
  "removed"      = c(as.numeric(0),as.numeric(0)),
  "status_infected_pigs" = c(as.numeric(0),as.numeric(0)),
  "infectiousness"    = c(as.numeric(0),as.numeric(0))
)

surveillance_schedule = data.frame(
  "ID_vertex"= as.integer(),
  "t_step"= as.integer(),
  "test_type"= as.numeric()
)
  
contact_network_edges = data.frame(
  "ID_vertex_source" = as.integer(),
  "ID_vertex_dest" = as.integer() ,
  "t_step"    = as.character() ,
  "n_susceptibles"  = as.integer() , 
    "infectiousness_factor" = as.numeric(),
  "contact_type"  = as.integer() )


contact_network_edges_tours = data.frame(
  "ID_vertex_source" = as.integer(),
  "ID_vertex_dest" = as.integer() ,
  "t_step"    = as.character() ,
  "n_susceptibles"  = as.integer() , 
  "infectiousness_factor" = as.numeric(),
  "contact_type"  = as.integer() )

contact_network_edges_vet = data.frame(
  "ID_vertex_source" = as.integer(),
  "ID_vertex_dest" = as.integer() ,
  "t_step"    = as.character() ,
  "n_susceptibles"  = as.integer() , 
  "infectiousness_factor" = as.numeric(),
  "contact_type"  = as.integer() )


vertex_pop = matrix(0, nrow = simulation_steps+1, ncol = 1)
vertex_pop = matrix(0, nrow = simulation_steps+1, ncol = 2)