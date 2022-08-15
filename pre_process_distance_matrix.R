#######################################
#' @title Preprocessing sub_script to build distance data between farms
#' @author : Antoine Champetier
#' @date : 15.06.2022
#' @description: This is an intermediate step in the distance calculations and is run from preprocess_contact_networks.R
#######################################





rm(list = ls())
Sys.setenv(LANG = "en")

load("parameters_all_predistance.RData")

## Building distances between all vertices with cut off at 10km ####
cutoff_dist = 10000 # distances of more than 10km are ignored



# building the distances one pair of vertices at a time. this is run on Ubelix as it is time consuming

spatial_data  <- data.frame("ID_vertex_source" = as.numeric(),
                            "ID_vertex_dest" = as.numeric(),
                            "dist" = as.numeric())




for (i in 1:nrow(parameters_all_final) ){
  for (j in 1:(nrow(parameters_all_final)) ){
    ID_vertex_source = parameters_all_final$swinenet_id[i]
    ID_vertex_dest = parameters_all_final$swinenet_id[j]
    dist_temp = sqrt( (parameters_all_final$gkode[i]-parameters_all_final$gkode[j])^2 +
                        (parameters_all_final$gkodn[i]-parameters_all_final$gkodn[j])^2)
    if (dist_temp <= cutoff_dist){
      spatial_data = rbind(spatial_data,
                           data.frame("ID_vertex_source" = ID_vertex_source,
                                      "ID_vertex_dest" = ID_vertex_dest,
                                      "dist" =dist_temp))
    }
  }
  
}



save("parameters_all_postdistance.RData")


# this section of script gathers the ubelix outputs in one file

spatial_data_gather  <- data.frame("ID_vertex_source" = as.numeric(),
                            "ID_vertex_dest" = as.numeric(),
                            "dist" = as.numeric())


for (i in 1:32){
  load(paste0("parameters_all_postdistance_",i, ".RData", sep =""))
  spatial_data_gather = rbind(spatial_data_gather,spatial_data)
  
}

spatial_data = spatial_data_gather

save(spatial_data, file = "parameters_all_postdistance.RData")
