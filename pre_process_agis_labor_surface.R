#######################################
#' @title Getting labor and surface data for farms to set up index case farm workers
#' @author : Antoine Champetier
#' @date : 28.06.2022
#' @description: creates an intermediary file to be added to parameters 
#######################################



rm(list = ls())

getwd() 
#setwd("./episim_swinenet")

relative_path_to_output = "../simulation_output/"
relative_path_to_scenarios = "../simulation_inputs/"
relative_path_to_data = "../epi_data/"
relative_path_to_wild_boar_data = "../epi_data/wild_boar/wildboar_wsl/"
relative_path_to_swiss_geom_data = "../epi_data/wild_boar/swiss_geom/"
relative_path_to_processed_data = "../epi_data/processed/"

suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(tidyverse))
suppressMessages(library(ggplot2))
suppressMessages(library(matrixStats))
suppressMessages(library(lubridate))




library(readstata13)
# this will be replaced with extrapolated tour data from trader data and ML
name_file  <-  paste(relative_path_to_data,"/agis/labor/AGIS_2014_2019_durchtiere_otheryears.dta",sep="")
laboragis_raw <- read.dta13(name_file)

detach("package:readstata13")


laboragis_raw <- laboragis_raw[, c("ln_ha", "tvd_nr", "year", "personnel_count0",  "personnel_type0" ,  "personnel_count1" , "personnel_type1" , 
 "personnel_count2" , "personnel_type2" ,  "personnel_count3" , "personnel_type3"  ,
 "personnel_count4" , "personnel_type4" ,  "personnel_count5" , "personnel_type5"  ,
 "personnel_count6" , "personnel_type6" ,  "personnel_count7" , "personnel_type7"  ,
 "personnel_count8" , "personnel_type8" ,  "personnel_count9" , "personnel_type9"  ,
 "personnel_count10", "personnel_type10",  "personnel_count11", "personnel_type11" ,
 "personnel_count12", "personnel_type12" , "personnel_count13", "personnel_type13" ,
 "personnel_count14", "personnel_type14",  "personnel_count15", "personnel_type15" ,
 "personnel_count16", "personnel_type16",  "personnel_count17", "personnel_type17" ,
 "personnel_count18", "personnel_type18",  "personnel_count19", "personnel_type19" ,
 "personnel_count20" ,"personnel_type20",  "personnel_count21", "personnel_type21" ,
 "personnel_count22" ,"personnel_type22",  "personnel_count23", "personnel_type23" ,
 "personnel_count24" ,"personnel_type24" )]


list_labor_categories  <-  c()

for (v in names(laboragis_raw)){
  a  <- laboragis_raw[,v]
  a  <- a[(!is.na(a))]
  a  <- a[(a!="")]
  list_labor_categories  <-  append(list_labor_categories, a[[1]]) 
}

list_labor_categories  <-  list_labor_categories[seq(5, length(laboragis_raw), by=2)]

  
labor_AGIS <- laboragis_raw[, c("tvd_nr", "year", "ln_ha", 
                               "personnel_count0",   "personnel_count1" , 
                               "personnel_count2" ,  "personnel_count3" , 
                               "personnel_count4" ,  "personnel_count5" , 
                               "personnel_count6" ,  "personnel_count7" , 
                               "personnel_count8" ,  "personnel_count9" , 
                               "personnel_count10",  "personnel_count11", 
                               "personnel_count12",  "personnel_count13", 
                               "personnel_count14",  "personnel_count15", 
                               "personnel_count16",  "personnel_count17", 
                               "personnel_count18",  "personnel_count19", 
                               "personnel_count20",  "personnel_count21", 
                               "personnel_count22",  "personnel_count23", 
                               "personnel_count24"  )]
 
names(labor_AGIS)   <-  append(c("tvd_nr", "year", "ln_ha"),list_labor_categories)
rm(laboragis_raw)

labor_AGIS  <- labor_AGIS[ ! labor_AGIS$tvd_nr %in% c(8888,9999),]


load(paste(relative_path_to_processed_data,"id_reference_flat_TVD.RData",sep ="" ))


labor_AGIS$id_TVD <- as.character(labor_AGIS$tvd_nr)
labor_AGIS <- left_join(labor_AGIS, id_reference_flat_TVD, by = c("id_TVD" = "id_TVD" ))

labor_AGIS <- labor_AGIS[!is.na(labor_AGIS$swinenet_id), append(c("swinenet_id", "year", "ln_ha"),list_labor_categories)]


foreign_list <- c("Familienfremde Ausländer, Männer, über 74% der Arbeitszeit", 
                  "Familienfremde Ausländer, Frauen, über 74% der Arbeitszeit",
                  "Familienfremde Ausländer, Männer, 50-74% der Arbeitszeit", 
                  "Familienfremde Ausländer, Frauen, 50-74% der Arbeitszeit",
                  "Familienfremde Ausländer, Männer, unter 50% der Arbeitszeit",              
                  "Familienfremde Ausländer, Frauen, unter 50% der Arbeitszeit")

all_labor_list = names(labor_AGIS)
all_labor_list = all_labor_list[c(4:28)]





labor_AGIS$labor_count = rowSums(labor_AGIS[,c(4:24)], na.rm = TRUE)

labor_AGIS$foreign_count =   rowSums(is.na(labor_AGIS[ ,foreign_list]))
labor_AGIS$foreign = 0
labor_AGIS$foreign[labor_AGIS$foreign_count < 6] = 1



labor_AGIS <- labor_AGIS[,c("swinenet_id","year","ln_ha","foreign","labor_count" )]
names(labor_AGIS)

save(labor_AGIS, file =paste(relative_path_to_processed_data,"labor_data_final.RData",sep ="" ))

#load(paste(relative_path_to_processed_data,"labor_data_final.RData",sep ="" ))



  