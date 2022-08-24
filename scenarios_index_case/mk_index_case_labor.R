#######################################
#' @title INdex case probabilities based on presence of foreign labor on farms
#' @author : Antoine Champetier
#' @date : 5.7.2022
#' @description: generate the probabilities of index cased based on presence of foreign workers on farm
#######################################



mk_index_case<- function(vertex_key, index_case_parameter){
  relative_path_to_processed_data = "../epi_data/processed/"
  load(paste(relative_path_to_processed_data,"labor_data_final.RData",sep ="" ))
  labor_indicator  <-  unique(labor_AGIS[labor_AGIS$year == year(ymd(date_start)),c("swinenet_id","foreign")])


  index_case_probabilities = left_join(vertex_key, labor_indicator ,  by = c("ID_vertex"= "swinenet_id"))
  index_case_probabilities  <-  index_case_probabilities[!duplicated(index_case_probabilities$ID_vertex),] 
  
  index_case_probabilities$probability= index_case_probabilities$foreign * index_case_parameter
  
  index_case_probabilities$probability[is.na(index_case_probabilities$probability)]=0
  
  index_case_probabilities  <-  index_case_probabilities[, c("ID_vertex", "probability") ]
  
  
  return(index_case_probabilities)
}

