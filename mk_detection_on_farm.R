#################################################################################################
#' @title detection for on farm surveillance
#' @author : Antoine Champetier
#' @date : 05.07.2022
#' @description: runs de detection on farms based on mortality with dynamic surveillance (update vet visits)
#################################################################################################



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
