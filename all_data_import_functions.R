#######################################
#' @title Makes a vertex key and other variable and parameter dataframes from the data provided
#' @author : Antoine Champetier
#' @date : 21.12.2021
#' @description: data extraction
#######################################



mk_vertex_key <-function(date_start, simulation_steps, data_active){
  load(data_active)
  end_date = ymd(date_start) + simulation_steps
  vertex_key  <- activity_dates[activity_dates$best_start_date <= end_date &
                                  activity_dates$best_end_date >= end_date, c("swinenet_id")]
  names(vertex_key) = c("ID_vertex")
  return(vertex_key)
}


mk_vertex_pop <- function(vertex_key,date_start,simulation_steps,pop_data_active){
  
  load(pop_data_active)
  reference_data_population  <- "2014-01-01"
  start_day_count = as.numeric((ymd(date_start) - ymd(reference_data_population)), units="days")+1
  end_day_count = start_day_count + simulation_steps
  
  # initialiye an empty matrix with a row per simulation step and a column per vertex
  vertex_pop  <- matrix(0,nrow = simulation_steps+1, ncol = nrow(vertex_key)  )
  
  # getting the population changes only for the farms and not for all other types of vertices or wild boar (ID less than 10000)
  vertex_pop[,vertex_key$ID_vertex < 10000 ] <- net_pop_change_vertex[start_day_count: end_day_count, vertex_key$ID_vertex[vertex_key$ID_vertex < 10000 ]]
  
  vertex_pop[is.na(vertex_pop)]  = 0 
  
  return(vertex_pop)
}






mk_vertex_param <- function(vertex_key, param_data, wildboar_param){
  #param_data = paste(relative_path_to_processed_data,"parameters_all_final.RData",sep ="" )
  #wildboar_param = paste(relative_path_to_processed_data,"wildboar_units.Rdata",sep ="" )
  load(param_data)
  farm_parameters   <-  parameters_all_final[parameters_all_final$swinenet_id %in% vertex_key$ID_vertex &
                                               parameters_all_final$enterprisetype != "SlaughterEnterprise" , ]
  farm_parameters$type = "farm"
  names(farm_parameters) = c("ID_vertex",  "enterprisetype", "gemeinde","gkode" ,  "gkodn" ,"farm_type_ML" , "haltungsform",  "type")
  
  load(wildboar_param)
  wb_num = nrow(wildboar_units)
  wildboar_parameters   <-  data.frame("ID_vertex" = wildboar_units$gemeinde_BFS + rep(90000, wb_num),
                                       "gemeinde" = wildboar_units$gemeinde_BFS,
                                       "type" = rep("wildboar", wb_num),
                                       "haltungsform" = rep(NA, wb_num),
                                       "farm_type_ML" = rep(NA, wb_num),
                                       "gkodn" = rep(NA, wb_num),
                                       "gkode" = rep(NA, wb_num),
                                       "enterprisetype" = rep(NA, wb_num) )
  wildboar_parameters <-  wildboar_parameters[wildboar_parameters$ID_vertex %in% vertex_key$ID_vertex, ]
  
  slaughter_parameters = parameters_all_final[parameters_all_final$swinenet_id %in% vertex_key$ID_vertex &
                                          parameters_all_final$enterprisetype == "SlaughterEnterprise" , ]
  slaughter_parameters$type = "slaugther" 
  names(slaughter_parameters) = c("ID_vertex",  "enterprisetype", "gemeinde","gkode" ,  "gkodn" ,"farm_type_ML" , "haltungsform",  "type")
  

  vertex_parameters = farm_parameters
  vertex_parameters = rbind(vertex_parameters, wildboar_parameters)
  vertex_parameters = rbind(vertex_parameters, slaughter_parameters)
  
  return(vertex_parameters)
}





mk_vertex_variables <- function(vertex_key, date_start,  compartment_list, population_file, population_wildboar){
  load(population_file)
  load(population_wildboar)
  reference_data_population  <- "2014-01-01"
  start_day_count = as.numeric((ymd(date_start) - ymd(reference_data_population)), units="days")+1

  startpop = matrix(0,nrow = nrow(vertex_key), ncol = 1  )
    # getting the starting population  only for the farms and not for all other types of vertices or wild boar (swinenet_id less than 10000)
  startpop[vertex_key$ID_vertex < 10000 ] <- pop_vertex[start_day_count, vertex_key$ID_vertex[vertex_key$ID_vertex < 10000 ]]
  startpop[is.na(startpop)]=0
  
  # now setting the wild boar populations
  wildboar_units$swinenet_id        <-   90000 + wildboar_units$gemeinde_BFS
  active_wildboar = vertex_key[vertex_key$ID_vertex >= 90000, ]
  active_wildboar = left_join(active_wildboar, wildboar_units, by = c("ID_vertex" = "swinenet_id") )
  startpop[vertex_key$ID_vertex >= 90000 ] <- active_wildboar$wildboar_pop
  
  
  # now building the full variable with all the compartments. The initial population is all put in susceptible state 
  init_vertex_variable <- data.frame(ID_vertex = vertex_key$ID_vertex, 
                                     susceptible = startpop)
  for(comp in 1:(compartment_num+1)){
    init_vertex_variable <- cbind(init_vertex_variable, rep(0,nrow(vertex_key)))
  }
  names(init_vertex_variable)<-c("ID_vertex",compartment_list, "status_infected_pigs","infectiousness")
  init_vertex_variable$susceptible[vertex_parameters$type == "slaughter"]<- 0
  
  return(init_vertex_variable)
}



mk_transport<- function(data_tvd,vertex_key,date_start_transport,sim_steps){
  load(data_tvd)
  date_end_transport = as.character(ymd(date_start_transport) + sim_steps)
  transport_network_edges  <- TVD_edge_swinenet[ TVD_edge_swinenet$date  >= date_start_transport &
                                        TVD_edge_swinenet$date  <= date_end_transport &
                                        TVD_edge_swinenet$swinenet_id_source %in% as.integer (vertex_key$ID_vertex) &
                                        TVD_edge_swinenet$swinenet_id_dest %in% as.integer (vertex_key$ID_vertex), ]

  transport_network_edges$date <-    floor(as.numeric(difftime( transport_network_edges$date, ymd(date_start) ),units = "days"))+2
  transport_network_edges$swinenet_id_dest = as.numeric(transport_network_edges$swinenet_id_dest)
  transport_network_edges$swinenet_id_source = as.numeric(transport_network_edges$swinenet_id_source)
  names(transport_network_edges) <- c("t_step","n_pigs","ID_vertex_source", "ID_vertex_dest")
  return(transport_network_edges)
}




 
mk_contact_network <- function(vertex_key, date_start,  simulation_steps, space_cutoff, distance_param_haltung, distance_param_wb_to_wb, space, tours, vets, resimul_tours){
  
  date_end = as.character(ymd(date_start) + simulation_steps)
  contact_network_edges <- data.frame("ID_vertex_source" = numeric(),
                                      "ID_vertex_dest" = numeric(),
                                      "t_step" = integer(),
                                      "n_susceptibles" = numeric(),
                                      "infectiousness_factor" = numeric(),
                                      "contact_type" = character())
  
  
  load(space)# space =paste(relative_path_to_processed_data,"spatial_data_final.RData",sep="")
  spatial_data  <- spatial_data[spatial_data$ID_vertex_source %in% vertex_key$ID_vertex & 
                                spatial_data$ID_vertex_dest %in% vertex_key$ID_vertex &
                                spatial_data$dist <= space_cutoff  ,]
  space_edges  <- data.frame(spatial_data$ID_vertex_source,
                             spatial_data$ID_vertex_dest ,
                             rep(Inf, nrow(spatial_data)), 
                             rep(NA, nrow(spatial_data)),
                             spatial_data$dist,
                             rep("s", nrow(spatial_data)))
  names(space_edges) = names(contact_network_edges)
  # remove the self contact
 
  # here we clean up and adjust the distance values to account for re-parametrization of distance = 0 for wild'boar to wild boar and haltungs form.

  space_edges   <- left_join(space_edges, vertex_parameters[, c("ID_vertex", "haltungsform","type") ], by = c("ID_vertex_source" = "ID_vertex"))
  space_edges   <- left_join(space_edges, vertex_parameters[, c("ID_vertex", "haltungsform","type") ], by = c("ID_vertex_dest" = "ID_vertex"))
  names(space_edges) = c("ID_vertex_source","ID_vertex_dest","t_step","n_susceptibles","infectiousness_factor", "contact_type", "haltungsform_source","type_source","haltungsform_dest","type_dest")
  
  
  space_edges   <- space_edges[space_edges$ID_vertex_source != space_edges$ID_vertex_dest,]
  space_edges   <- space_edges[space_edges$type_source != "slaugther" & space_edges$type_dest != "slaugther", ]
  space_edges$infectiousness_factor[space_edges$type_source == "wildboar" & space_edges$type_dest == "wildboar"]    <-  distance_param_wb_to_wb
  
  # this is setting the haltungsform of the wild boards to 4 to streamline the calculation of haltungsform adjustments.
  space_edges$haltungsform_source[space_edges$type_source == "wildboar"] = 4 
  space_edges$haltungsform_dest[space_edges$type_dest == "wildboar"] = 4

  # this is setting the NAs of hlatungsform to 2 as default 
  space_edges$haltungsform_source[is.na(space_edges$haltungsform_source)] = 2 
  space_edges$haltungsform_dest[is.na(space_edges$haltungsform_dest)] = 2
  
  
  temp_farm_wildboar_distance = (space_edges$type_source == "farm" & space_edges$type_dest == "wildboar") | 
                                (space_edges$type_source == "wildboar" & space_edges$type_dest == "farm")
  
  space_edges$infectiousness_factor[temp_farm_wildboar_distance]    <-  
    space_edges$infectiousness_factor[temp_farm_wildboar_distance] + 
    distance_param_haltung * (9 - space_edges$haltungsform_source[temp_farm_wildboar_distance ]- space_edges$haltungsform_dest[temp_farm_wildboar_distance ])
    
  temp_farm_farm_distance = (space_edges$type_source == "farm" & space_edges$type_dest == "farm") 
  
  space_edges$infectiousness_factor[temp_farm_farm_distance]   <- 
    space_edges$infectiousness_factor[temp_farm_farm_distance] +  
    distance_param_haltung * (9 - space_edges$haltungsform_source[temp_farm_farm_distance ]- space_edges$haltungsform_dest[temp_farm_farm_distance ])
    


  contact_network_edges   <- rbind(contact_network_edges, space_edges[c("ID_vertex_source","ID_vertex_dest","t_step","n_susceptibles","infectiousness_factor", "contact_type")])


  
  if(resimul_tours == TRUE){
    source("mk_tour_contacts.R")

    load(tours)
    tour_TVD_final  <- tour_TVD_final[tour_TVD_final$ID_vertex_dest %in% vertex_key$ID_vertex & 
                                        tour_TVD_final$ID_vertex_source  %in% vertex_key$ID_vertex &
                                        tour_TVD_final$date  >=  date_start &
                                        tour_TVD_final$date  <=  date_end,]
    tour_TVD_final  <-  mk_contact_network_tour(tour_TVD_final)
    tour_TVD_final$date  <-    floor(as.numeric(difftime( tour_TVD_final$date, ymd(date_start) ),units = "days"))+2 
    tour_TVD_final$n_susceptibles  <-   10 # add n_pigs here need to fix mk_tour_contacts
    tour_TVD_final$infectiousness_factor   <-   NA
    names(tour_TVD_final)[names(tour_TVD_final) == 'date'] <- 't_step'
    contact_network_edges   <- rbind(contact_network_edges, tour_TVD_final)
    
    
    load(vets)
    tour_vet_final  <- tour_vet_final[tour_vet_final$ID_vertex_dest %in% vertex_key$ID_vertex & 
                                        tour_vet_final$ID_vertex_source  %in% vertex_key$ID_vertex &
                                        tour_vet_final$date  >=  date_start &
                                        tour_vet_final$date  <=  date_end,]
    tour_vet_final  <-  mk_contact_network_vet(tour_vet_final)
    tour_vet_final$date  <-    floor(as.numeric(difftime( tour_vet_final$date, ymd(date_start) ),units = "days"))+2 
    tour_vet_final$n_susceptibles  <-   NA # add n_pigs here need to fix mk_tour_contacts
    tour_vet_final$infectiousness_factor   <-   NA
    names(tour_vet_final)[names(tour_vet_final) == 'date'] <- 't_step'
    contact_network_edges   <- rbind(contact_network_edges, tour_vet_final)
  
  } else {
    load(paste(relative_path_to_processed_data,"tours_TVD_contact_made.RData",sep=""))
    tours_TVD_contact_made  <- tours_TVD_contact_made[tours_TVD_contact_made$ID_vertex_dest %in% vertex_key$ID_vertex & 
                                                        tours_TVD_contact_made$ID_vertex_source  %in% vertex_key$ID_vertex &
                                                        tours_TVD_contact_made$date  >=  date_start &
                                                        tours_TVD_contact_made$date  <=  date_end,]
    
   
    tours_TVD_contact_made$date  <-    floor(as.numeric(difftime( tours_TVD_contact_made$date, ymd(date_start) ),units = "days"))+2 
    tours_TVD_contact_made$n_susceptibles  <-   10 # add n_pigs here need to fix mk_tour_contacts
    tours_TVD_contact_made$infectiousness_factor   <-   NA
    names(tours_TVD_contact_made)[names(tours_TVD_contact_made) == 'date'] <- 't_step'
    contact_network_edges   <- rbind(contact_network_edges, tours_TVD_contact_made)
    
    
    load(paste(relative_path_to_processed_data,"tours_vet_contact_made.RData",sep=""))
    tours_vet_contact_made  <- tours_vet_contact_made[tours_vet_contact_made$ID_vertex_dest %in% vertex_key$ID_vertex & 
                                                        tours_vet_contact_made$ID_vertex_source  %in% vertex_key$ID_vertex &
                                                        tours_vet_contact_made$date  >=  date_start &
                                                        tours_vet_contact_made$date  <=  date_end,]

    tours_vet_contact_made$date  <-    floor(as.numeric(difftime( tours_vet_contact_made$date, ymd(date_start) ),units = "days"))+2 
    
    tours_vet_contact_made$n_susceptibles  <-   NA # add n_pigs here need to fix mk_tour_contacts
    tours_vet_contact_made$infectiousness_factor   <-   NA
    names(tours_vet_contact_made)[names(tours_vet_contact_made) == 'date'] <- 't_step'
    contact_network_edges   <- rbind(contact_network_edges, tours_vet_contact_made)
  }
  


  return(contact_network_edges)
}



mk_surveillance<- function(vertex_key, surveillance_parameter, surveillance_policy){
  
  surveillance_schedule <- matrix(0,nrow = nrow(vertex_key), ncol = simulation_steps) 
  surveillance_density = surveillance_parameter # share of nodes tested each day
  for (t_step in 1:simulation_steps){
    surveillance_schedule[sample.int(nrow(vertex_key),round(nrow(vertex_key)*surveillance_density/2,0),replace = FALSE) ,t_step] <- 1
  }
  for (t_step in 1:simulation_steps){
    surveillance_schedule[sample.int(nrow(vertex_key),round(nrow(vertex_key)*surveillance_density/2,0),replace = FALSE) ,t_step] <- 2
  }
  
  #turning the matrix into a list of tests since it is much more efficient (lots of zeros)
  rownames(surveillance_schedule) <- vertex_key$ID_vertex
  colnames(surveillance_schedule) <- c(1:simulation_steps)
  surveillance_schedule<- as.data.frame(as.table(surveillance_schedule))
  names(surveillance_schedule) <- c("ID_vertex","t_step","test_type")
  surveillance_schedule <-  surveillance_schedule[surveillance_schedule$test_type > 0 ,] 
  
  surveillance_schedule$ID_vertex <- as.integer(as.character(surveillance_schedule$ID_vertex))
  surveillance_schedule$t_step <- as.integer(as.character(surveillance_schedule$t_step))
  return(surveillance_schedule)
}





mk_index_case<- function(vertex_key, index_case_parameter){
  index_case_probabilities <- data.frame(ID_vertex = vertex_key$ID_vertex,
                                         probability = rep(0,nrow(vertex_key)))
  index_case_probabilities$probability[ sample(nrow(vertex_key),index_case_parameter)]=.9                                       
  index_case_probabilities$probability[vertex_key$type=="slaughter"] <- 0
  return(index_case_probabilities)
}








