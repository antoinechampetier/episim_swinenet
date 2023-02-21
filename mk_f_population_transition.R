#######################################
#' @title INdex case probabilities based on distance of wild boar from rest areas
#' @author : Antoine Champetier
#' @date : 21.12.2021
#' @description: generate the probabilities of index cased based on wild boar distance to rest area
#######################################







f_population <-function(vertex_variables,vertex_pop_t){
  ## for testing vertex_pop_t <- vertex_pop[t,]
  
  # for slaughterhouses only, remove all live animals to removed
  
  vertex_variables$removed[vertex_parameters$type == "slaughter"] <-  vertex_variables$removed[vertex_parameters$type == "slaughter"] + 
    rowSums(vertex_variables[vertex_parameters$type == "slaughter",c(compartment_list[-compartment_num])]) #moving all pigs to "removed" in slaughterhouses
  vertex_variables[vertex_parameters$type == "slaughter",c(compartment_list[-compartment_num])] <- 0 # setting all other compartments of pigs in slaughterhouses back to zero
  
  ## for all vertices (slaughterhouse will not be affected as they have no population change)
  vertex_variables$susceptible <- vertex_variables$susceptible + vertex_pop_t #adding/subtracting the net births/deaths to susceptible
  vertex_variables$susceptible[vertex_variables$susceptible<0] <- 0
  vertex_variables$removed[vertex_pop_t<0] <- vertex_variables$removed[vertex_pop_t<0] + vertex_pop_t[vertex_pop_t<0] #adding the net births/deaths to removed when negative (when they are a net death)
  
  ## advance disease stages (backwards from removed to latent)


  for(comp in (compartment_num-1):2){
    advance = rbinom( nrow(vertex_variables[vertex_parameters$type == "farm",]) , 
                      floor(vertex_variables[vertex_parameters$type == "farm", comp+1]), 
                      transition_proba$farm[comp])
    if(length(advance) >0){
    vertex_variables[vertex_parameters$type == "farm",comp+1] = vertex_variables[vertex_parameters$type == "farm",comp+1] - advance
    vertex_variables[vertex_parameters$type == "farm",comp+2] = vertex_variables[vertex_parameters$type == "farm",comp+2] + advance # note that there is +1 in vertex_variable coming from the first column being the Vertex_ID
    }
  }
  vertex_variables[vertex_parameters$type == "farm","removed"] = vertex_variables[vertex_parameters$type == "farm","removed"] + vertex_variables[vertex_parameters$type == "farm","carcass"]
  vertex_variables[vertex_parameters$type == "farm","carcass"] = 0
  
  for(comp in (compartment_num-1):2){
    advance = rbinom( nrow(vertex_variables[vertex_parameters$type == "wildboar",]) , 
                      floor(vertex_variables[vertex_parameters$type == "wildboar", comp+1]), 
                      transition_proba$wildboar[comp])
    if(length(advance) >0){
      vertex_variables[vertex_parameters$type == "wildboar",comp+1] = vertex_variables[vertex_parameters$type == "wildboar",comp+1] - advance
      vertex_variables[vertex_parameters$type == "wildboar",comp+2] = vertex_variables[vertex_parameters$type == "wildboar",comp+2] + advance # note that there is +1 in vertex_variable coming from the first column being the Vertex_ID
    }
  }
  # restore the susceptible population to the base in wildboar units)
  vertex_variables$susceptible[vertex_parameters$type == "wildboar"] <-  regen_wild_boar *init_vertex_variable$susceptible[vertex_parameters$type == "wildboar"] 
  
  
  #vertex_variables[vertex_parameters$type == "wildboar","removed"] = vertex_variables[vertex_parameters$type == "wildboar","removed"] + vertex_variables[vertex_parameters$type == "wildboar","carcass"]
  #vertex_variables[vertex_parameters$type == "wildboar","carcass"] = 0
  
  
  return(vertex_variables)
}


