#################################################################################################
#' @title Defines all the  functions that will be use in the simulation core 
#' @author : Antoine Champetier
#' @date : 01.01.2022
#' @description: 
#################################################################################################



f_inoculate <-function(vertex_variables, index_case_probabilities, index_case_number){
  infected = data.frame(index_case_state = rep(0,nrow(vertex_parameters))) # initialize a vector with 0 cases
  while (sum(infected$index_case_state) <=  index_case_number){
    infected$index_case_state <- as.numeric(runif(length(index_case_probabilities$probability)) < index_case_probabilities$probability) # note that runif is the uniform distribution draw
  }
  if (sum(infected$index_case_state) >  index_case_number){
    index_cases_long = which(infected$index_case_state >0 )
    index_cases_long_remove = sample(index_cases_long, sum(infected$index_case_state)-index_case_number, replace = FALSE)
    infected$index_case_state[index_cases_long_remove] = 0
  }
  
  vertex_variables$susceptible[infected$index_case_state>0] <- vertex_variables$susceptible[infected$index_case_state>0] - 1 # remove susceptible where index cases have occurred
  vertex_variables$latent[infected$index_case_state>0] <- vertex_variables$latent[infected$index_case_state>0] + 1 # add to latent where index cases have occurred
  vertex_variables$status_infected_pigs[infected$index_case_state>0] <- vertex_variables$status_infected_pigs[infected$index_case_state>0] + 1 # add to status of where index cases have occurred
  return(vertex_variables)
}





f_preval_incid <-function(vertex_variables){ 
  # this function calculates prevalence and incidence in farms, pigs, and wild boar units. 
  # the incidence is based on the difference with the status variable "status_infected_pigs" which is then reset after this function call

  temp_vertex <-  left_join(vertex_parameters[,c("ID_vertex", "type")], vertex_variables[,c("ID_vertex", infected_compartments ,"status_infected_pigs")],by = c("ID_vertex"="ID_vertex"))
  temp_vertex$count_infected  <- rowSums(temp_vertex[,infected_compartments])
  
  # this is where the incidence is calculated as a difference from previous count of infected pigs. with transports and population, this is not exact for pig and wild board counts.
  # note that this is not a problem for farm incidence numbers but only for pigs and wild boars which is calculated from the output of the infection function

  prevalence_farm <- nrow(temp_vertex[(temp_vertex$count_infected>0)&(temp_vertex$type=="farm"),])
  indidence_farm <- nrow(temp_vertex[((temp_vertex$status_infected_pigs==0)&(temp_vertex$count_infected>0))&(temp_vertex$type=="farm"),])
  
  prevalence_wb_units <- nrow(temp_vertex[(temp_vertex$count_infected>0)&(temp_vertex$type=="wildboar"),])
  indidence_wb_units <- nrow(temp_vertex[((temp_vertex$status_infected_pigs==0)&(temp_vertex$count_infected>0))&(temp_vertex$type=="wildboar"),])
  
  prevalence_pigs <- sum(temp_vertex$count_infected[temp_vertex$type=="farm"])
  prevalence_wb <- sum(temp_vertex$count_infected[temp_vertex$type=="wildboar"])
  
  output <- c("prevalence_farm" = prevalence_farm, "prevalence_wb_units" = prevalence_wb_units, 
              "indidence_farm" = indidence_farm, "indidence_wb_units" = indidence_wb_units,
              "prevalence_pigs" = prevalence_pigs, "prevalence_wb" = prevalence_wb)
  return(output)
}



f_transport <-function(vertex_variables, active_edges){
  ## for TESTING purpose active_edges <- transport_network_edges[transport_network_edges$t_step == t,]
  if (nrow(active_edges)>0){
    active_edges_source_info <- left_join(active_edges, vertex_variables[,c("ID_vertex",compartment_list)], by = c("ID_vertex_source"="ID_vertex"))#combine the source compartment information with the active edges
    active_edges_source_info[,compartment_list] <- active_edges_source_info[,compartment_list]/rowSums(active_edges_source_info[,compartment_list]) # calculate proportions in each compartment of the transport
    active_edges_source_info[,compartment_list] <-   round(active_edges_source_info[,compartment_list] * active_edges_source_info[,"n_pigs"]) # multiply by the numbers of pigs transported and round
    active_edges_source_info$susceptible <- active_edges_source_info$n_pigs - rowSums(active_edges_source_info[,compartment_list[-1]]) # this corrects for rounding errors by adjusting susceptible in transport so the n_pigs in transport is kept 
    
    destination_additions <- active_edges_source_info %>% group_by(ID_vertex_dest) %>% summarise(across(all_of(compartment_list), list(sum))) # add across transports for cases when a destination receives multiple transports
    source_removals <- active_edges_source_info %>% group_by(ID_vertex_source) %>% summarise(across(all_of(compartment_list), list(sum))) # add across transports for cases when a source sends multiple transports
    names(destination_additions) <- c("ID_vertex",compartment_list) # this is needed to match the column names of vertex_variables after the group by. It needed for the flexible compartment list feature
    names(source_removals) <- c("ID_vertex",compartment_list) # this is needed to match the column names of vertex_variables after the group by. It needed for the flexible compartment list feature
    
    # removing and adding the transports from the source and destination vertices
    vertex_variables[vertex_variables$ID_vertex %in% destination_additions$ID_vertex,compartment_list] <- vertex_variables[vertex_variables$ID_vertex %in% destination_additions$ID_vertex,compartment_list] + destination_additions[compartment_list]
    vertex_variables[vertex_variables$ID_vertex %in% source_removals$ID_vertex,compartment_list] <- vertex_variables[vertex_variables$ID_vertex %in% source_removals$ID_vertex,compartment_list] + source_removals[compartment_list]
    
    
  } # end of if condition for checking that transports occur
  return(vertex_variables)
}



f_detection <-function(vertex_variables,surveillance_schedule_t){  
  # surveillance_schedule_t is a list of tests to be performed at current time step, variables are ID_vertex and test_type
  # for TESTING : surveillance_schedule_t <- surveillance_schedule[surveillance_schedule$t_step == t,]
  # NOTE TO DO: test that this works for multiple tests on the same farm
  
  output_detection <- c(0)
  
  test_infected_vertex <- left_join(surveillance_schedule_t, vertex_variables, by = c("ID_vertex")) # get the state data for the vertices tested at this time step
  test_infected_vertex <- left_join(test_infected_vertex,vertex_parameters, by = c("ID_vertex")) # get the vertex type
  
  # for slaughterhouses, the status_infected_pigs needs to be updated after transport as all pigs are cleared at population stage and therefore is status_infected_pigs at beginning of time step
  # for other vertices, the status_infected_pigs reflects pre-transport numbers and does not correspond to what the compartments counts are after transport. This gets reconciled in first step of loop.
  # slaugtherhouses not used.  test_infected_vertex$status_infected_pigs[test_infected_vertex$type == "slaughter"] <- rowSums(test_infected_vertex[test_infected_vertex$type == "slaughter",c(compartment_list[2:(compartment_num-1)])])
  
  test_infected_vertex <- filter(test_infected_vertex, status_infected_pigs > 0 ) # take only the infected ones. To be able to track false-positives, one would need to keep all tested vertices, not implemented here

  if(nrow(test_infected_vertex)>0){
    # each test is assumed to be an individual pig test but the share_animal_tested parameter 
    # means that the share of pigs tested in the holding can be changed, the number of animals tested is always at least one (rounding up)
    # tests_sensitivities = list("1" = 0.80, "2" = 0.95)
    # share_animal_tested = list("1" = 0.10, "2" = 0.5)
    # Note that the sensitivity is the same for all pigs across compartments....TO FIX?
    
    test_infected_vertex$total_pigs <- rowSums(test_infected_vertex[,compartment_list[1:(compartment_num-1)]])
    test_infected_vertex$tested_pigs <- min(ceiling(unlist(share_animal_tested[test_infected_vertex$test_type])*test_infected_vertex$status_infected_pigs), test_infected_vertex$total_pigs)
   
    test_infected_vertex$infected_tested_pigs = with(test_infected_vertex,  ## drawing the pigs tested among all the pigs present and only count the ones that are infected
                    sapply(1:nrow(test_infected_vertex), 
                           function(i) sum(sample(test_infected_vertex$total_pigs[i], 
                                                  test_infected_vertex$tested_pigs[i], 
                                                  replace = FALSE) <= test_infected_vertex$status_infected_pigs[i])))

    
    test_infected_vertex <- filter(test_infected_vertex, infected_tested_pigs > 0 )
    
    if(nrow(test_infected_vertex)>0){ # draw the tests only if there are any infected animals selected for testing
    test_infected_vertex$detections <-  rbinom(nrow(test_infected_vertex),test_infected_vertex$infected_tested_pigs, unlist(tests_sensitivities[test_infected_vertex$test_type]))
  
    output_detection <- sum(test_infected_vertex$detections)
    }
  }


  
  
  return(output_detection) # this would need to be changed in order for the detection function to be allowed to change surveillance_schedule for dynamic surveillance strategies
}



f_population <-function(vertex_variables,vertex_pop_t){
  ## for testing vertex_pop_t <- vertex_pop[t,]
  ##   vertex_pop_t_check = vertex_variables
  ## vertex_pop_t_check$pop = vertex_pop_t
  
  # for slaughterhouses only, remove all live animals to removed
  vertex_variables$removed[vertex_parameters$type == "slaughter"] <-  vertex_variables$removed[vertex_parameters$type == "slaughter"] + 
                    rowSums(vertex_variables[vertex_parameters$type == "slaughter",c(compartment_list[-compartment_num])]) #moving all pigs to "removed" in slaughterhouses
  vertex_variables[vertex_parameters$type == "slaughter",c(compartment_list[-compartment_num])] <- 0 # setting all other compartments of pigs in slaughterhouses back to zero
  
  ## for all vertices (slaughterhouse will not be affected as they have no population change)
  vertex_variables$susceptible <- vertex_variables$susceptible + vertex_pop_t #adding/subtracting the net births/deaths to susceptible
  vertex_variables$susceptible[vertex_variables$susceptible<0] <- 0
  vertex_variables$removed[vertex_pop_t<0] <- vertex_variables$removed[vertex_pop_t<0] - vertex_pop_t[vertex_pop_t<0] #adding the net births/deaths to removed when negative (when they are a net death)
  
  # rest the susceptible population to the base in wildboar units)
  vertex_variables$susceptible[vertex_parameters$type == "wildboar"] <-  init_vertex_variable$susceptible[vertex_parameters$type == "wildboar"] 
  
  
  ## advance disease stages (backwards from removed to latent)
  vertex_variables[,compartment_list[compartment_num]] <- vertex_variables[,compartment_list[compartment_num]] + vertex_variables[,compartment_list[compartment_num-1]] # start by adding to removed (from last clinical stage)
  vertex_variables[,compartment_list[-c(1,2,compartment_num)]] <- vertex_variables[,compartment_list[-c(1,compartment_num-1,compartment_num)]] # move all compartments (except susceptible, latent and removed) one step to the right
  vertex_variables[,compartment_list[2]] <- 0 # set latent to zero
  
  return(vertex_variables)
}



f_infection <-function(vertex_variables, contact_edgelist){
    ## for TESTING purpose: contact_edgelist <- contact_network_edges[contact_network_edges$t_step == t | contact_network_edges$t_step == Inf ,]
    #"ID_vertex_source"       "ID_vertex_dest"         "t_step"                 "n_susceptibles"         "infectioussness_factor" "contact_type"    
    
    
    infected_vertex <- vertex_variables[(vertex_variables$infectiousness>0),] # find the vertices that are currently infected 


    active_infected_edge <- inner_join(contact_edgelist, infected_vertex, by = c("ID_vertex_source"="ID_vertex")) # add the compartments from source vertex 
    active_infected_edge$source_npigs <- rowSums(active_infected_edge[,compartment_list[-compartment_num]]) # calculate the total number of pigs (infected or not) at source
    active_infected_edge <- active_infected_edge[,-which(names(active_infected_edge) %in% compartment_list)] # remove useless compartments from source vertex
    
    
    active_infected_edge <- left_join(active_infected_edge,vertex_variables[,c("ID_vertex","susceptible" )], by = c("ID_vertex_dest"="ID_vertex")) # add the susceptible count of the destination
    active_infected_edge$n_susceptibles[is.na(active_infected_edge$n_susceptibles)] <- active_infected_edge$susceptible[is.na(active_infected_edge$n_susceptibles)] # when number of susceptible has not been set  (from transport size) take susceptible in destination vertex
    active_infected_edge <- active_infected_edge[active_infected_edge$n_susceptibles > 0,-which(names(active_infected_edge) %in% c("susceptible","status_infected_pigs","ID_vertex_source","t_step"))]# remove vertex dest with no susceptibles as well as useless compartments from source vertex active_infected_edge

    active_infected_edge$infect_proba[active_infected_edge$contact_type == "p2p"] <- 
      probas_infections_param["p2p"]*
      tanh(active_infected_edge$infectiousness_factor[active_infected_edge$contact_type == "p2p"]*
             active_infected_edge$infectiousness[active_infected_edge$contact_type == "p2p"]/
             active_infected_edge$source_npigs[active_infected_edge$contact_type == "p2p"])
    active_infected_edge$infect_proba[active_infected_edge$contact_type == "f"] <- 
                            probas_infections_param["f"]*
                            tanh(active_infected_edge$infectiousness_factor[active_infected_edge$contact_type == "f"]*
                                 active_infected_edge$infectiousness[active_infected_edge$contact_type == "f"]/
                                 active_infected_edge$source_npigs[active_infected_edge$contact_type == "f"])
    
    active_infected_edge$infect_proba[active_infected_edge$contact_type == "s"] <- probas_infections_param["s"]*
                                                                                   tanh(active_infected_edge$infectiousness[active_infected_edge$contact_type == "s"])/
                                                                                   (1-active_infected_edge$infectiousness_factor[active_infected_edge$contact_type == "s"])^2
    active_infected_edge$infect_proba[active_infected_edge$contact_type == "v"] <- probas_infections_param["v"]*tanh(active_infected_edge$infectiousness[active_infected_edge$contact_type == "v"])
    active_infected_edge$infect_proba[active_infected_edge$contact_type == "t"] <- probas_infections_param["t"]*tanh(active_infected_edge$infectiousness[active_infected_edge$contact_type == "t"])
   
    self_contacts <- data.frame(infected_vertex$ID_vertex,infected_vertex$susceptible, probas_infections_param["self"])
    names(self_contacts) = c("ID_vertex_dest","n_susceptibles","infect_proba") ## add the self infections with contact intensity of 1.
 
    if(nrow(active_infected_edge>0)){
    active_infected_edge <- active_infected_edge[,c("ID_vertex_dest","n_susceptibles","infect_proba")]
    active_infected_edge <- rbind(active_infected_edge, self_contacts)} else {
    active_infected_edge <- self_contacts
    }

    
    active_infected_edge$contact_draw <- rbinom(nrow(active_infected_edge),size=floor(active_infected_edge$n_susceptibles),active_infected_edge$infect_proba) # draw the occurrence of contact based on infection probability
    
    infection_edge <- active_infected_edge[active_infected_edge$contact_draw > 0,c("ID_vertex_dest","contact_draw")] # extract the edges where infections occur
    infection_edge <- infection_edge %>% group_by(ID_vertex_dest) %>% summarise(sum(contact_draw))
    
    infection_edge <- left_join(data.frame(ID_vertex=vertex_variables$ID_vertex), infection_edge, by = c("ID_vertex"= "ID_vertex_dest"))
    names(infection_edge)<- c("ID_vertex","contact_draw")
    infection_edge[is.na(infection_edge$contact_draw),"contact_draw"]<-0

    
    return(infection_edge)
}

  

