#######################################
#' @title Preprocessing of tour, vet and spatial data to build contact network edgelists
#' @author : Antoine Champetier
#' @date : 29.05.2022
#' @description: Build a data frame covering all period of data with contact network links (farm to farm)
#######################################



## Basic set up libraries and paths to data ####
getwd()
rm(list = ls())
Sys.setenv(LANG = "en")
#setwd("./episim_swinenet")
# setwd("~/swinenet_episim/episim_swinenet")

suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(readxl))
suppressMessages(library(lubridate))
suppressMessages(library(ggplot2))

relative_path_to_processed_data = "../epi_data/processed/"
load(paste(relative_path_to_processed_data,"TVD_traders.RData",sep=""))
load(paste(relative_path_to_processed_data,"id_reference_flat_TVD.RData",sep ="" ))


## Extracting the tour ids from data or extrapolation when available ####
tour_TVD_temp  <-  tvd_traders_2014_2019[, c( "date","tourid_original","tvd_source","tvd_dest","n_pigs","c_plannedarrivaltime" )]

tour_TVD_temp$tvd_source  <-    as.character(tour_TVD_temp$tvd_source)
tour_TVD_temp$tvd_dest  <-    as.character(tour_TVD_temp$tvd_dest)

tour_TVD_temp <-    left_join(tour_TVD_temp, id_reference_flat_TVD, by = c("tvd_source" = "id_TVD"))
names(tour_TVD_temp)[names(tour_TVD_temp) == "swinenet_id"] <- "ID_vertex_source"

tour_TVD_temp <-    left_join(tour_TVD_temp, id_reference_flat_TVD, by = c("tvd_dest" = "id_TVD"))
names(tour_TVD_temp)[names(tour_TVD_temp) == "swinenet_id"] <- "ID_vertex_dest"

tour_TVD_final <-  tour_TVD_temp[, c( "date","ID_vertex_dest","ID_vertex_source","n_pigs","tourid_original","c_plannedarrivaltime" )]


temp_time <- substr(tour_TVD_final$c_plannedarrivaltime, nchar(tour_TVD_final$c_plannedarrivaltime) - 8 + 1, nchar(tour_TVD_final$c_plannedarrivaltime))
temp_minutes <- as.numeric(substr(temp_time, 1,2))*60 + as.numeric(substr(temp_time, 4,5))

tour_TVD_final$time_at_dest <-  temp_minutes
tour_TVD_final$time_at_source <-  temp_minutes-30

tour_TVD_final <-  tour_TVD_final[, c( "ID_vertex_dest","ID_vertex_source","n_pigs", "date", "tourid_original","time_at_dest", "time_at_source" )]

names(tour_TVD_final)[names(tour_TVD_final) == 'tourid_original'] <- 'tour_id'

tour_TVD_final <-   na.omit(tour_TVD_final)
save(tour_TVD_final, file =paste(relative_path_to_processed_data,"tour_TVD_final.RData",sep ="" ))


source("mk_tour_contacts.R")
load(paste(relative_path_to_processed_data,"tour_TVD_final.RData",sep ="" ))

tours_TVD_contact_made  <-  mk_contact_network_tour(tour_TVD_final)

save(tours_TVD_contact_made, file =paste(relative_path_to_processed_data,"tours_TVD_contact_made.RData",sep ="" ))
rm(tour_TVD_final,tour_TVD_temp,tvd_traders_2014_2019,temp_minutes, temp_time,tours_TVD_contact_made)



## Extracting the vet from data or ext◙rapolation when available ####
# AT THE MOMENT JUST SIMULATING TO CHECK IMPORT AND MODEL RUN

density_contacts = 10
vertex_num = nrow(id_reference_flat_TVD)
tour_vet_final <- data.frame("ID_vertex_source" = sample.int(vertex_num,vertex_num*density_contacts,replace = TRUE),
                             "ID_vertex_dest" = sample.int(vertex_num,vertex_num*density_contacts,replace = TRUE),
                             "tour_id" = sample.int(5,vertex_num*density_contacts,replace = TRUE),
                             "date" = sample(seq(as.Date("2014/01/01"), as.Date("2020/12/01"), by="day"), vertex_num*density_contacts, replace = TRUE),
                             "time_at_source" = sample.int(24*60,vertex_num*density_contacts,replace = TRUE),
                             "time_at_dest" = sample.int(24*60,vertex_num*density_contacts,replace = TRUE))

tour_vet_final<- tour_vet_final[tour_vet_final$ID_vertex_source != tour_vet_final$ID_vertex_dest,] # removing edges with same source and dest
tour_vet_final<- tour_vet_final[tour_vet_final$time_at_dest != tour_vet_final$time_at_source,] # removing edges with same time at source and dest

save(tour_vet_final, file =paste(relative_path_to_processed_data,"tour_vet_final.RData",sep ="" ))


tours_vet_contact_made  <-  mk_contact_network_vet(tour_vet_final)
save(tours_vet_contact_made, file =paste(relative_path_to_processed_data,"tours_vet_contact_made.RData",sep ="" ))
rm(tour_vet_final,density_contacts,vertex_num,tours_vet_contact_made)



## Building distances between all vertices with cut off at 10km ####
cutoff_dist = 10000 # distances of more than 10km are ignored



load(paste(relative_path_to_processed_data,"parameters_all_final.RData",sep ="" ))
load(paste(relative_path_to_processed_data,"wildboar_units.Rdata",sep ="" ))

wildboar_units$swinenet_id        <-   90000 + wildboar_units$gemeinde_BFS
wildboar_units$gemeinde        <-   wildboar_units$gemeinde_BFS



library(raster)
library(sf)
library(terra)

str_name<-'R8_AI_1118_lv03_20210302_37737.tif' 

# Note here that the read_sf function only seems to accept absolute path. this needs a fix
shapedirectory_swissbound <- "/Users/ac20l170/Documents/swinenet_episim/epi_data/swiss_geom/SHAPEFILE_LV95_LN02"
#shapedirectory_swissbound<- "/Users/antoinechampetier/Dropbox/Work/Research/SwineNet/Code/swinenet_episim/epi_data/swiss_geom/SHAPEFILE_LV95_LN02"
data.shape_gemeinde2  <-  read_sf(dsn=shapedirectory_swissbound,layer="swissBOUNDARIES3D_1_3_TLM_HOHEITSGEBIET")
touching_gemiende  <-  st_touches(data.shape_gemeinde2)



#shapedirectory_swissbound<- "/Users/antoinechampetier/Dropbox/Work/Research/SwineNet/Code/swinenet_episim/epi_data/swiss_geom/SHAPEFILE_LV95_LN02"
data.shape_land  <-  read_sf(dsn=shapedirectory_swissbound,layer="swissBOUNDARIES3D_1_3_TLM_LANDESGEBIET")

gemeinde_boundaries = data.shape_gemeinde2$geometry



shapedirectory_italy<- "/Users/ac20l170/Documents/swinenet_episim/epi_data/Italy_shapefile"
data.shape_italy = read_sf(dsn=shapedirectory_italy,layer="it_1km")
italy_boundaries = data.shape_italy$geometry


gemeinde_boundaries <- st_transform(data.shape_gemeinde2, st_crs(data.shape_italy))
gemeinde_boundaries <- gemeinde_boundaries$geometry



list_italian_border_gemeinde = data.frame("BFS_NUMMER" = as.numeric(),
                                          "gemeinde_name" = as.character())

for(i in 1:length(data.shape_gemeinde2$NAME)){
  AA = st_intersects(gemeinde_boundaries[i],italy_boundaries)
  
  if(length(unlist(AA)) != 0){
    new_gemeinde = c("BGS_NUMMER" = data.shape_gemeinde2$BFS_NUMMER[i],
                     "gemeinde_name" = data.shape_gemeinde2$NAME[i])
    
    list_italian_border_gemeinde =rbind(list_italian_border_gemeinde, new_gemeinde)
    print(new_gemeinde)
  }

}
names(list_italian_border_gemeinde) = c("gemeinde_BFS", "gemiende_name")
list_italian_border_gemeinde$gemeinde_BFS = as.numeric(list_italian_border_gemeinde$gemeinde_BFS)

save(list_italian_border_gemeinde, file =paste(relative_path_to_processed_data,"list_italian_border_gemeinde_final.RData",sep ="" ))
rm(italy_boundaries)

# replacing the missing coordinates of vertices by centroid of gemiende
gemeinde_centroids  <- st_centroid(data.shape_gemeinde2$geometry)
gemeinde_centroids = unlist(gemeinde_centroids)

row_odd <- seq_len(length(gemeinde_centroids)) %% 2             
gkode = gemeinde_centroids[row_odd == 1]    
gkodn = gemeinde_centroids[row_odd == 0] 

gemeinde_centroids    <- data.frame("gemeinde_BFS" = data.shape_gemeinde2$BFS_NUMMER,
                                    "gkode" = gkode,
                                    "gkodn"  =gkodn)

gemeinde_centroids   <- gemeinde_centroids [!duplicated(gemeinde_centroids$gemeinde_BFS),]
 
parameters_all_final_temp = left_join(parameters_all_final, gemeinde_centroids, by = c("gemeinde"="gemeinde_BFS"))

parameters_all_final[is.na(parameters_all_final$gkode), c("gkode","gkodn" )] = 
  parameters_all_final_temp[is.na(parameters_all_final$gkode), c("gkode.y","gkodn.y" )]


parameters_all_final = parameters_all_final[!is.na(parameters_all_final$gkode),] 
rm(gemeinde_boundaries)





save(parameters_all_final, file =paste(relative_path_to_processed_data,"parameters_all_predistance.RData",sep ="" ))

rm(parameters_all_final)
## Here one needs to rerun the distance calculation that uses parameters_all_predistance and returns 
## parameters_all_postdistance. That is a very time consuming process run on cluster
## it runs the code below with cutoff 10km

# 
# spatial_data  <- data.frame("ID_vertex_source" = as.numeric(),
#                             "ID_vertex_dest" = as.numeric(),
#                             "dist" = as.numeric())
# 
# for (i in 1:nrow(parameters_all_final) ){
#   for (j in 1:(nrow(parameters_all_final)) ){
#     ID_vertex_source = parameters_all_final$swinenet_id[i]
#     ID_vertex_dest = parameters_all_final$swinenet_id[j]
#     dist_temp = sqrt( (parameters_all_final$gkode[i]-parameters_all_final$gkode[j])^2 +
#                         (parameters_all_final$gkodn[i]-parameters_all_final$gkodn[j])^2)
#     if (dist_temp <= cutoff_dist){
#       spatial_data = rbind(spatial_data,
#                            data.frame("ID_vertex_source" = ID_vertex_source,
#                                       "ID_vertex_dest" = ID_vertex_dest,
#                                       "dist" =dist_temp))
#     }
#   }
#   
# }

load(paste(relative_path_to_processed_data,"parameters_all_predistance.RData",sep ="" ))
backup_parameters_all_final = parameters_all_final

load(paste(relative_path_to_processed_data,"parameters_all_postdistance.RData",sep ="" ))





# adding wildboar to vertex contacts if vertex falls in same gemeinde 
spatial_data_vertex_WB  <- data.frame("ID_vertex_source" = as.numeric(),
                               "ID_vertex_dest" = as.numeric(),
                               "dist" = as.numeric())


for (i in 1 :nrow(parameters_all_final) ){
  for (j in 1:(nrow(wildboar_units)) ){
    if(wildboar_units$gemeinde[j] == parameters_all_final$gemeinde[i]){
      spatial_data_vertex_WB = rbind(spatial_data_vertex_WB, 
                           data.frame("ID_vertex_source" = wildboar_units$swinenet_id[j],
                             "ID_vertex_dest" = parameters_all_final$swinenet_id[i],
                             "dist" = 0))
      spatial_data_vertex_WB = rbind(spatial_data_vertex_WB, 
                           data.frame("ID_vertex_source" = parameters_all_final$swinenet_id[i],
                             "ID_vertex_dest" = wildboar_units$swinenet_id[j],
                             "dist" = 0))
    }
  } 
  
} 

# adding wildboar to vertex contacts where gemeinde are touching 


spatial_data_WB  <- data.frame("ID_vertex_source" = as.numeric(),
                            "ID_vertex_dest" = as.numeric(),
                            "dist" = as.numeric())

for (i in 1:nrow(data.shape_gemeinde2) ){
  list_touch = unlist(touching_gemiende[i])
  for (j in list_touch ){
    spatial_data_WB = rbind(spatial_data_WB, 
                           c("ID_vertex_source" = data.shape_gemeinde2$BFS_NUMMER[j]+90000,
                             "ID_vertex_dest" = data.shape_gemeinde2$BFS_NUMMER[i]+90000,
                             "dist" = 0))
  }
  }
names(spatial_data_WB) = c("ID_vertex_source","ID_vertex_dest","dist")

spatial_data_WB   <- spatial_data_WB[spatial_data_WB$ID_vertex_source %in% wildboar_units$swinenet_id,]
spatial_data_WB   <- spatial_data_WB[spatial_data_WB$ID_vertex_dest %in% wildboar_units$swinenet_id,]


# adding wildboar to vertex contacts where gemeinde are touching 
spatial_data_WB = rbind(spatial_data_vertex_WB, spatial_data_WB)
spatial_data = rbind(spatial_data, spatial_data_WB)

spatial_data = spatial_data[ !is.na(spatial_data),]
spatial_data = unique(spatial_data)

save(spatial_data, file =paste(relative_path_to_processed_data,"spatial_data_final.RData",sep ="" ))
#load(paste(relative_path_to_processed_data,"spatial_data_final.RData",sep ="" ))




## Pieces related to the ML procedure on tour predictions ####

#links <- readRDS("G:/VPHI/Epi/Projects/100_PigNetworkModeling_SNF (Duerr)/DatasetsAnalysis/Tour_prediction/fg/links.rds")


#links$date_format <- as.Date(links$date, origin="1970-01-01")
#links$year <- year(links$date_format)
#max(links$year)
