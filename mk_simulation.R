#################################################################################################
#' @title SIMULATION RUN
#' @author : Antoine Champetier
#' @date : 21.12.2021
#' @description: Runs one simulation, assumes complete swinenet_environment loaded
#################################################################################################



## INITIALIZE OUTPUT VARIABLE ####
output_run <- data.frame(t_step=c(1:simulation_steps), 
                         prevalence_farm=rep(0,simulation_steps),
                         prevalence_wb_units=rep(0,simulation_steps),
                         incidence_farm=rep(0,simulation_steps),
                         incidence_wb_units=rep(0,simulation_steps),
                         prevalence_pigs=rep(0,simulation_steps),
                         prevalence_wb=rep(0,simulation_steps),
                         incidence_pigs=rep(0,simulation_steps),
                         incidence_wb=rep(0,simulation_steps),
                         detections=rep(0,simulation_steps)) 



output_detection <- data.frame(t_step=1:simulation_steps, detections=rep(NA,simulation_steps))

## GENERATE INDEX CASE(S)####

vertex_variables <- f_inoculate(init_vertex_variable, index_case_probabilities) # note that the index case is applied to the initialized, disease-free vertex state variable

t=1
## LOOP OVER SIMULATION TIME STEPS #### t=t+1
for(t in 1:simulation_steps){

  # Set a minimum population of 1 for the vertices that are zero (coming from lack of AGIS data for instance)
  vertex_variables[rowSums(vertex_variables[,compartment_list]) <=1,"susceptible" ] = 1     
  # fill in the the prevalence and incidences in the output variable
  output_run[output_run$t_step == t, c(2:7)] <- f_preval_incid(vertex_variables)

  # update the two state variables that are used to calculate infections and prevalence/incidence with correct timing (accounting for transport or population mixing and change effects)
  vertex_variables$status_infected_pigs[vertex_parameters$type != "slaughter"] <- rowSums(vertex_variables[vertex_parameters$type != "slaughter",infected_compartments])  # update the status_infected_pigs variable 
  vertex_variables$infectiousness[vertex_parameters$type != "slaughter"] <- rowSums(sweep(vertex_variables[vertex_parameters$type != "slaughter",compartment_list],2,compartment_infect,"*")) # update the infectiousness (from 0 to 1) of the vertex based on its compartments and weights

  # transport animals (use the transports occurring at current time step t)
  vertex_variables <- f_transport(vertex_variables, transport_network_edges[transport_network_edges$t_step == t,])
  
  # implement detection based on schedule at time step t and save to output for detection
  output_run$detections[output_run$t_step==t] <- f_detection(vertex_variables,surveillance_schedule[surveillance_schedule$t_step == t,])

  # advance infection states and population changes from population data
  vertex_variables <- f_population(vertex_variables,vertex_pop[t,])
  
  
  # draw infections through contact networks (use the edges active in current time step t)
  if(sum(vertex_variables$infectiousness)>0){
  infections <- f_infection(vertex_variables, contact_network_edges[contact_network_edges$t_step == t | contact_network_edges$t_step == Inf ,])

  
  output_run$incidence_pigs[output_run$t_step==t] <- sum(infections$contact_draw[vertex_parameters$type == "farm"])
  output_run$incidence_wb[output_run$t_step==t] <- sum(infections$contact_draw[vertex_parameters$type == "wildboar"])
  

  vertex_variables$latent <- vertex_variables$latent+infections$contact_draw # update the latent compartments when infection occurs
  vertex_variables$susceptible <- vertex_variables$susceptible-infections$contact_draw# update the susceptible compartments when infection occurs
  }
  
}
