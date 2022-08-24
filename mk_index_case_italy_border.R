#######################################
#' @title INdex case probabilities based on distance of wild boar italy border
#' @author : Antoine Champetier
#' @date : 21.12.2021
#' @description: generate the probabilities of index cased based on wild boar distance to rest area
#######################################







mk_index_case<- function(vertex_key, index_case_parameter){
  relative_path_to_processed_data = "../epi_data/processed/"
  load(paste(relative_path_to_processed_data,"list_italian_border_gemeinde_final.Rdata",sep =""))
  load(paste(relative_path_to_processed_data,"wildboar_units.Rdata",sep =""))


  wildboar_units = left_join(wildboar_units,list_italian_border_gemeinde, by = c("gemeinde_BFS"= "gemeinde_BFS") )
  
  wildboar_units$ID_vertex =   wildboar_units$gemeinde_BFS + 90000   
  
  wildboar_units$border_italy = 0
  wildboar_units$border_italy[!is.na(wildboar_units$gemiende_name) ] = 1
  
  wildboar_units=unique(wildboar_units[,c("ID_vertex","wildboar_density","border_italy"  )])
  
  index_case_probabilities = left_join(vertex_key, wildboar_units,  by = c("ID_vertex"= "ID_vertex"))
  

  index_case_probabilities$wildboar_density[is.na(index_case_probabilities$wildboar_density)]=0
  
  index_case_probabilities$probability=index_case_probabilities$border_italy*index_case_probabilities$wildboar_density
  index_case_probabilities$probability[is.na(index_case_probabilities$probability)]=0

  index_case_probabilities  <-  index_case_probabilities[, c("ID_vertex", "probability") ]
  
  
  return(index_case_probabilities)
}
