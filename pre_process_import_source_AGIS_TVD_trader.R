#######################################
#' @title Extraction of data from all source data into Rdata files ready for pre process or env. make
#' @author : Antoine Champetier
#' @date : 25.02.2022
#' @description: 
#######################################



rm(list = ls())

suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(readxl))



relative_path_to_processed_data = "../epi_data/processed/"
# paths to update in order to use local data not in G drive
relative_path_to_AGIS_data = "../epi_data/agis/"
relative_path_to_AGIS_coordinate_data = "../epi_data/agis/coordinates/"
relative_path_to_AGIS_inventories_data = "../epi_data/agis/inventories/"
relative_path_to_AGIS_tvd_ids_data = "../epi_data/agis/tvdinfo/"
relative_path_to_TVD_data = "../../epi_data/tvd/"


# tier codes to remove non pig AGIS entries
pigtypes = c("1621"="Zuchteber", # Breeding boars
             "1615"="Nicht säugende Zuchtsauen über 6 Monate alt (ca 3 Umtriebe pro Platz)",# Non-lactating breeding sows over 6 months old
             "1611"="Säugende Zuchtsauen", # Suckling breeding sows 
             "1631"="Abgesetzte Ferkel", # Weaned piglets
             "1639"="Remonten und Mastschweine (ca 3 Umtriebe pro Platz)", # Replacement and fattening pigs
             "1635"="Saugferkel (im Faktor der Mutter eingerechnet)") # Suckling pig
pigcodes = names(pigtypes)

years = c("2014","2015","2016","2017","2018","2019","2020")
          
for(y in years){
  assign(paste("AGIS_invent_",y,sep=""),
         data.frame(read_excel(paste("G:/VPHI/Epi/Projects/100_PigNetworkModeling_SNF (Duerr)/Datasets/AGIS/20211108/20211108_pigs_",y,".xlsx",sep=""))))
}

AGIS_inventory <- rbind(AGIS_invent_2014,AGIS_invent_2015,AGIS_invent_2016,AGIS_invent_2017, AGIS_invent_2018,AGIS_invent_2019,AGIS_invent_2020)
rm(AGIS_invent_2014,AGIS_invent_2015,AGIS_invent_2016,AGIS_invent_2017, AGIS_invent_2018,AGIS_invent_2019,AGIS_invent_2020)

AGIS_inventory  <- AGIS_inventory[AGIS_inventory$TIERCODE %in% pigcodes,]

inventory_average <- AGIS_inventory %>% group_by(ID,JAHR) %>% summarise(sum(DURCH_TIERE))
inventory_jan1 <- AGIS_inventory %>% group_by(ID,JAHR) %>% summarise(sum(STICHTAG_TIERE))
haltungsform_max <- AGIS_inventory %>% group_by(ID,JAHR) %>% summarise(max(HALTUNGSFORM))


AGIS_inventory <- left_join(inventory_average, inventory_jan1, by =c("JAHR","ID"))
AGIS_inventory <- left_join(AGIS_inventory, haltungsform_max, by =c("JAHR","ID"))

names(AGIS_inventory)  <- c("ID","JAHR","DURCH_TIERE","STICHTAG_TIERE","haltungsform_max" )
  
  
# Importing the AGIS to TVD matches for all years (including fl/Liechtenstein)
for(y in years[1:6]){
  assign(paste("AGIS_TVD_",y,sep=""), 
         data.frame(read_excel(paste("G:/VPHI/Epi/Projects/100_PigNetworkModeling_SNF (Duerr)/Datasets/AGIS/AGIS_FINAL/200206_pigs_",y,".xlsx",sep=""), 
                               sheet = paste("TVD ",y,sep =""))))
  assign(paste("fl_AGIS_TVD_",y,sep=""), 
         data.frame(read_excel(paste("G:/VPHI/Epi/Projects/100_PigNetworkModeling_SNF (Duerr)/Datasets/AGIS/AGIS_FINAL/200302_pigs_fl_",y,".xlsx",sep=""), 
                               sheet = "TVD")))

}
AGIS_TVD_2020 <- read_excel("G:/VPHI/Epi/Projects/100_PigNetworkModeling_SNF (Duerr)/Datasets/AGIS/20211108/20211112_pigs_TVD_2020.xlsx")
AGIS_TVD_id_match <- rbind(AGIS_TVD_2014,AGIS_TVD_2015,AGIS_TVD_2016,AGIS_TVD_2017,AGIS_TVD_2018,AGIS_TVD_2019,AGIS_TVD_2020,
                  fl_AGIS_TVD_2014,fl_AGIS_TVD_2015,fl_AGIS_TVD_2016,fl_AGIS_TVD_2017,fl_AGIS_TVD_2018,fl_AGIS_TVD_2019)
rm(y,AGIS_TVD_2014,AGIS_TVD_2015,AGIS_TVD_2016,AGIS_TVD_2017,AGIS_TVD_2018,AGIS_TVD_2019,AGIS_TVD_2020, 
                  fl_AGIS_TVD_2014,fl_AGIS_TVD_2015,fl_AGIS_TVD_2016,fl_AGIS_TVD_2017,fl_AGIS_TVD_2018,fl_AGIS_TVD_2019)





AGIS_matched <-  left_join(AGIS_inventory[c("JAHR","ID","DURCH_TIERE","STICHTAG_TIERE","haltungsform_max" )],AGIS_TVD_id_match[c("ID","TVD_NR")],by = "ID")
AGIS_matched <- unique(AGIS_matched)

AGIS_matched <-  AGIS_matched %>% ungroup()
save(AGIS_matched, file =paste(relative_path_to_processed_data,"AGIS_matched.RData",sep ="" ))


# getting variables from Francesco's merged dataset


library(readstata13)
# this will be replaced with extrapolated tour data from trader data and ML
name_file = "G:/VPHI/Epi/Projects/100_PigNetworkModeling_SNF (Duerr)/Datasets/TVD/Stata/tvd_data_gps_pigcat_suisagtransports_agis_traders_farmtype_suisagtraders.dta"
merged_tvd_agis_raw <- read.dta13(name_file)


name_file = "G:/VPHI/Epi/Projects/100_PigNetworkModeling_SNF (Duerr)/DatasetsAnalysis/Tour_prediction/fg/data_exploration/tvd_traders_2014_2019.dta"
tvd_traders_2014_2019 <- read.dta13(name_file)
detach("package:readstata13")
save(tvd_traders_2014_2019, file =paste(relative_path_to_processed_data,"TVD_traders.RData",sep ="" ))



TVD_merged <- merged_tvd_agis_raw[,c( "id_source",
                             "id_dest",
                             "tvd_source",
                             "tvd_dest",
                             "enterprisetype_source",
                             "enterprisetype_dest",
                             "date",
                             "n_pigs",
                             "pigcat",
                             "tourid",
                             "seq_nr",
                             "gkode_source",             
                             "gkodn_source",
                             "gkode_dest",
                             "gkodn_dest",
                             "type_to_type",
                             "farm_type_dest",
                             "farm_type_source")] 

rm(merged_tvd_agis_raw,name_file)

save(TVD_merged, file =paste(relative_path_to_processed_data,"TVD_merged_tour.RData",sep ="" ))

# PigNet_Data_2020 <- read.csv("~/swinenet_episim/epi_data/tvd/PigNet_Data_2020.csv")
PigNet_Data_2020 <- read.csv("G:/VPHI/Epi/Projects/100_PigNetworkModeling_SNF (Duerr)/Datasets/TVD/Nov2021/PigNet_Data_2020.csv")

# PigNet_Data_2014_19 <- read.csv("~/swinenet_episim/epi_data/tvd/PigNet_Data_v3-20200820.csv")
PigNet_Data_2014_19 <- read.csv("G:/VPHI/Epi/Projects/100_PigNetworkModeling_SNF (Duerr)/Datasets/TVD/Aug2020/SwineNet_Data_20200820/PigNet_Data_v3-20200820.csv")
TVD_source <-  rbind(PigNet_Data_2014_19,PigNet_Data_2020)


TVD_source <- TVD_source[!(TVD_source$TVD_Source == TVD_source$TVD_Dest),
                          c( "TVD_Source",
                             "TVD_Dest",
                             "EnterpriseType_Source",
                             "EnterpriseType_Dest",
                             "Event_Date",
                             "N_Pigs",
                             "PigCategory",
                             "Municipality_Dest",
                             "Municipality_Source",
                             "GKODE_Source",             
                             "GKODE_Dest",
                             "GKODE.y",
                             "GKODN.y")] 

names(TVD_source) <- c("tvd_source",
                      "tvd_dest",
                      "enterprisetype_source",
                      "enterprisetype_dest",
                      "date",
                      "n_pigs",
                      "pigcat",
                      "gemeinde_dest",
                      "gemeinde_source",
                      "gkode_source",             
                      "gkodn_source",
                      "gkode_dest",
                      "gkodn_dest")

save(TVD_source, file =paste(relative_path_to_processed_data,"TVD_source.RData",sep ="" ))

