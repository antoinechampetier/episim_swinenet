#######################################
#' @title Make surveillance and detection for farm based surveillance
#' @author : Antoine Champetier
#' @date : 15.08.2022
#' @description:  Makes a function for a schedule surveillance, daily based on morbidity (clinical signs)
#######################################




mk_surveillance<- function(vertex_key, surveillance_parameter){
  
  surveillance_schedule <- matrix(0,nrow = nrow(vertex_key), ncol = simulation_steps) 
  
  targeted_farms = vertex_parameters$ID_vertex[ vertex_parameters$type == "farm" ]
  
  surveillance_schedule[ vertex_key$ID_vertex %in% targeted_farms,] <- 2
  
  #turning the matrix into a list of tests since it is much more efficient (lots of zeros)
  rownames(surveillance_schedule) <- vertex_key$ID_vertex
  colnames(surveillance_schedule) <- c(1:simulation_steps)
  surveillance_schedule<- as.data.frame(as.table(surveillance_schedule))
  names(surveillance_schedule) <- c("ID_vertex","t_step","test_type")
  surveillance_schedule <-  surveillance_schedule[surveillance_schedule$test > 0 ,] 
  
  surveillance_schedule$ID_vertex <- as.integer(as.character(surveillance_schedule$ID_vertex))
  surveillance_schedule$t_step <- as.integer(as.character(surveillance_schedule$t_step))
  return(surveillance_schedule)
}




f_detection <-function(vertex_variables,surveillance_schedule_t){  
  # surveillance_schedule_t is a list of tests to be performed at current time step, variables are ID_vertex and test_type
  # surveillance_schedule_t = surveillance_schedule[surveillance_schedule$t_step == t,]
  # NOTE: test needed to check that this works for multiple tests on the same farm
  
  output_detection <-  data.frame("ID_vertex" = as.character(),
                                  "test_type" = as.numeric())
  
  
  
  test_infected_vertex <- left_join(surveillance_schedule_t, vertex_variables, by = c("ID_vertex")) # get the state data for the vertices tested at this time step
  test_infected_vertex <- left_join(test_infected_vertex,vertex_parameters, by = c("ID_vertex")) # get the vertex type
  test_infected_vertex <- filter(test_infected_vertex, status_infected_pigs > 0 ) # take only the infected ones. To be able to track false-positives, one would need to keep all tested vertices, not implemented here
  
  test_infected_vertex_test_1  <- test_infected_vertex[test_infected_vertex$test_type == 1,  ]
  test_infected_vertex_test_2  <- test_infected_vertex[test_infected_vertex$test_type == 2,  ]
  test_infected_vertex_test_3  <- test_infected_vertex[test_infected_vertex$test_type == 3,  ]
  
  if(nrow(test_infected_vertex_test_1)>0){

    output_detection_1 <-  data.frame("ID_vertex" = as.character(),
                                    "test_type" = as.numeric())
    
    
    test_infected_vertex_test_1$pigs_morbid <- rowSums(test_infected_vertex_test_1[,c("clinical_0","clinical_1", "carcass")])
    test_infected_vertex_test_1$total_pigs  <- rowSums(test_infected_vertex_test_1[,c(compartment_list[1:compartment_num-1])])
    test_infected_vertex_test_1$detections  <- 0
    test_infected_vertex_test_1$detections[test_infected_vertex_test_1$pigs_morbid >=  tests_parameters$other_parameter_1[1] |
                                           test_infected_vertex_test_1$pigs_morbid/test_infected_vertex_test_1$total_pigs >  tests_parameters$other_parameter_2[1] ] <- 1
    
    
    if(nrow(test_infected_vertex_test_1)>0){ # draw the tests only if there are any infected animals selected for testing
      output_detection_1 <- test_infected_vertex_test_1$detections
      output_detection  <- rbind(output_detection,output_detection_1)
      }
  }
  
  if(nrow(test_infected_vertex_test_2)>0){
    
    output_detection_1 <-  data.frame("ID_vertex" = as.character(),
                                      "test_type" = as.numeric())
    
    
    test_infected_vertex_test_2$pigs_dead <- test_infected_vertex_test_2[,c("carcass")]
    test_infected_vertex_test_2$total_pigs  <- rowSums(test_infected_vertex_test_2[,c(compartment_list[1:compartment_num-1])])
    test_infected_vertex_test_2$detections  <- 0
    test_infected_vertex_test_2$detections[test_infected_vertex_test_2$pigs_dead >=  tests_parameters$other_parameter_1[2 ] |
                                             test_infected_vertex_test_2$pigs_morbid/test_infected_vertex_test_2$total_pigs >  tests_parameters$other_parameter_2[2] ] <- 2
    
    
    if(nrow(test_infected_vertex_test_2)>0){ # draw the tests only if there are any infected animals selected for testing
      output_detection_2 <- test_infected_vertex_test_2$detections
      output_detection  <- rbind(output_detection,output_detection_2)
    }
  }
  
  
  
  return(output_detection) # this would need to be changed in order for the detection function to be allowed to change surveillance_schedule for dynamic surveillance strategies
}






