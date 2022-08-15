#######################################
#' @title Building list of vertices for wild boards from WSL boar data and Gemeinde shapefile
#' @author : Antoine Champetier
#' @date : 01.04.2022
#' @description: Builds one wild boar unit per Gemeinde except where densities are 
#' zero fo for all the raster cells covered by the Gemeinde
#######################################



rm(list = ls())

getwd() 
#setwd("./episim_swinenet")

relative_path_to_output = "../simulation_output/"
relative_path_to_scenarios = "../simulation_inputs/"
relative_path_to_data = "../epi_data/"
relative_path_to_wild_boar_data = "../epi_data/wild_boar/wildboar_wsl/"
relative_path_to_swiss_geom_data = "../epi_data/wild_boar/swiss_geom/"
relative_path_to_processed = "../epi_data/processed/"

suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(tidyverse))
suppressMessages(library(ggplot2))
suppressMessages(library(matrixStats))
suppressMessages(library(lubridate))


library(raster)
library(sf)
library(terra)

str_name<-'R8_AI_1118_lv03_20210302_37737.tif' 
#imported_raster=raster(paste(relative_path_to_wild_boar_data,str_name,sep=""))
imported_rast =  rast(paste(relative_path_to_wild_boar_data,str_name,sep=""))



# Note here that the read_sf function only seems to accept absolute path. this needs a fix
shapedirectory_swissbound<- "/Users/ac20l170/Documents/swinenet_episim/epi_data/swiss_geom/SHAPEFILE_LV95_LN02"
#shapedirectory_swissbound<- "/Users/antoinechampetier/Dropbox/Work/Research/SwineNet/Code/swinenet_episim/epi_data/swiss_geom/SHAPEFILE_LV95_LN02"

data.shape_gemeinde2 = read_sf(dsn=shapedirectory_swissbound,layer="swissBOUNDARIES3D_1_3_TLM_HOHEITSGEBIET")
data.proj_gemeinde2 = st_transform(data.shape_gemeinde2, crs(imported_rast))
data.proj_gemeinde2 <- data.proj_gemeinde2[,c(16,20,22)]





gem_num = nrow(data.proj_gemeinde2)
gemeinde_densities <- data.frame(c(1:gem_num))

for (i in 1:gem_num){
boar_masked = mask(imported_rast, vect(data.proj_gemeinde2[i,]))
gemeinde_density <- values(boar_masked)
gemeinde_density <- gemeinde_density[!is.na(gemeinde_density)]

gemeinde_densities$gemeinde[i] <- as.numeric(data.proj_gemeinde2$BFS_NUMMER[i])
gemeinde_densities$sum_density[i] <- sum(gemeinde_density[gemeinde_density >= 0 ])
gemeinde_densities$name[i] <- data.proj_gemeinde2$NAME[i]
gemeinde_densities$surface[i] <- data.proj_gemeinde2$GEM_FLAECH[i]
#if(sum(gemeinde_density)<0){
#plot(boar_masked, main=paste("Sum of density: ",gemeinde_density," in Gemeinde ",data.proj_gemeinde2$NAME[i],sep=""))
#}
}

gemeinde_densities$average_dens <- gemeinde_densities$sum_density / gemeinde_densities$surface
wildboar_units <- gemeinde_densities[(gemeinde_densities$sum_density > 0 )&(!is.na(gemeinde_densities$surface)),c(2,3,5,6)]
names(wildboar_units) <- c("gemeinde_BFS","wildboar_pop","gemeinde_area_m2","wildboar_density")
save(wildboar_units,file= paste(relative_path_to_processed ,"wildboar_units.Rdata",sep =""))

#load(paste(relative_path_to_processed,"wildboar_units.Rdata",sep =""))



str_name__rest_areas<-'restareastarg2_values_86_eudis_20210302_lv03_cf2_4cls.tif'
imported_rast_rest_areas =  rast(paste(relative_path_to_wild_boar_data,str_name__rest_areas,sep=""))


data.shape_gemeinde2 = read_sf(dsn=shapedirectory_swissbound,layer="swissBOUNDARIES3D_1_3_TLM_HOHEITSGEBIET")


data.proj_gemeinde2 = st_transform(data.shape_gemeinde2, crs(imported_rast_rest_areas))
data.proj_gemeinde2 <- data.proj_gemeinde2[,c(16,20,22)]


gem_num = nrow(data.proj_gemeinde2)
gemeinde_distances_restarea <- data.frame(c(1:gem_num))

i=1
for (i in 1:gem_num){
  area_masked = mask(imported_rast_rest_areas, vect(data.proj_gemeinde2[i,]))
  min_distances_rest_area <- max(values(area_masked),na.rm = TRUE)  
  # From WSL explanation 
  # From 0 to 2,000 m: assigned value 4.  This is consistent with the seasonal home range of females.
  #From 2,001 to 4,000 m: assigned value 3. This is consistent with seasonal movements of males.
  #From 4,001 to 20,000 m: assigned value 2
  # Distances longer than 20,000 m. This is consistent with some individuals that disperse farther


  gemeinde_distances_restarea$gemeinde[i] <- as.numeric(data.proj_gemeinde2$BFS_NUMMER[i])
  gemeinde_distances_restarea$distance_rest[i] <- min_distances_rest_area

}
gemeinde_distances_restarea <-  gemeinde_distances_restarea %>% group_by(gemeinde) %>% summarise(distance_rest_min = max(distance_rest))
wildboar_units  <- left_join(wildboar_units, gemeinde_distances_restarea , by = c("gemeinde_BFS" = "gemeinde"))
save(wildboar_units,file= paste(relative_path_to_processed ,"wildboar_units.Rdata",sep =""))

