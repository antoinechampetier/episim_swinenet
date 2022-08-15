#######################################
#' @title INdex case probabilities based on distance of wild boar from rest areas
#' @author : Antoine Champetier
#' @date : 21.12.2021
#' @description: generate the probabilities of index cased based on wild boar distance to rest area
#######################################







f_population_transition <-function(vertex_variables,vertex_pop_t, transition_proba){
  ## for testing vertex_pop_t <- vertex_pop[t,]
  
  # for slaughterhouses only, remove all live animals to removed
  vertex_variables$removed[vertex_parameters$type == "slaughter"] <-  vertex_variables$removed[vertex_parameters$type == "slaughter"] + 
    rowSums(vertex_variables[vertex_parameters$type == "slaughter",c(compartment_list[-compartment_num])]) #moving all pigs to "removed" in slaughterhouses
  vertex_variables[vertex_parameters$type == "slaughter",c(compartment_list[-compartment_num])] <- 0 # setting all other compartments of pigs in slaughterhouses back to zero
  
  ## for all vertices (slaughterhouse will not be affected as they have no population change)
  vertex_variables$susceptible <- vertex_variables$susceptible + vertex_pop_t #adding/subtracting the net births/deaths to susceptible
  vertex_variables$susceptible[vertex_variables$susceptible<0] <- 0
  vertex_variables$removed[vertex_pop_t<0] <- vertex_variables$removed[vertex_pop_t<0] + vertex_pop_t[vertex_pop_t<0] #adding the net births/deaths to removed when negative (when they are a net death)
  
  transition_proba$farm
  ## advance disease stages (backwards from removed to latent)
  comp=1
  for(comp in (compartment_num-1):-1:2){
    advance = rbinom( vertex_variables[vertex_parameters$type == "farm", comp] , 
                      floor(vertex_variables[vertex_parameters$type == "farm",compartment_list[comp]]), 
                      transition_proba$farm[comp])
    vertex_variables[vertex_parameters$type == "farm",comp] = vertex_variables[vertex_parameters$type == "farm",comp] - advance
    vertex_variables[vertex_parameters$type == "farm",comp+1] = vertex_variables[vertex_parameters$type == "farm",comp+1] + advance
    
  }
  vertex_variables[vertex_parameters$type == "farm",compartment_list[compartment_num]] <- 
    vertex_variables[,compartment_list[compartment_num]] + 
    vertex_variables[,compartment_list[compartment_num-1]] # start by adding to removed (from last clinical stage)
  vertex_variables[,compartment_list[-c(1,2,compartment_num)]] <- vertex_variables[,compartment_list[-c(1,compartment_num-1,compartment_num)]] # move all compartments (except susceptible, latent and removed) one step to the right
  vertex_variables[,compartment_list[2]] <- 0 # set latent to zero
  
  
  
  ## advance disease stages (backwards from removed to latent)
  vertex_variables[,compartment_list[compartment_num]] <- vertex_variables[,compartment_list[compartment_num]] + vertex_variables[,compartment_list[compartment_num-1]] # start by adding to removed (from last clinical stage)
  vertex_variables[,compartment_list[-c(1,2,compartment_num)]] <- vertex_variables[,compartment_list[-c(1,compartment_num-1,compartment_num)]] # move all compartments (except susceptible, latent and removed) one step to the right
  vertex_variables[,compartment_list[2]] <- 0 # set latent to zero
  
  return(vertex_variables)
}
