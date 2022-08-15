#######################################
#' @title Preprocessing of AGIS and TVD datafor matching
#' @author : Antoine Champetier
#' @date : 29.11.2021
#' @description: 
#######################################


## Library and basic set up  ####
suppressMessages(library(networkDynamic))
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))

suppressMessages(library(ggplot2))
suppressMessages(library(matrixStats))
suppressMessages(library(lubridate))


rm(list = ls())

getwd()
#setwd("./episim_swinenet")
relative_path_to_data = "../epi_data" ## Note that the epi_data folder should contain three RData files prepared with import_source_AGIS_TVD_merged_trader.R

relative_path_to_processed_data = "../epi_data/processed/"



## Load and extraction of data from TVD and AGIS pre-extracted files ####
#load(paste(relative_path_to_processed_data,"TVD_merged_tour.RData",sep=""))  
load(paste(relative_path_to_processed_data,"TVD_source.RData",sep=""))
load(paste(relative_path_to_processed_data,"AGIS_matched.RData",sep=""))


AGIS_matched   <-  data.frame(AGIS_matched) # remove the group attributes that are passed from data extraction

TVD_source$year  <-  year(TVD_source$date)
TVD_source   <-  TVD_source[TVD_source$year>2013,]
# checking replicates and removing them (6062 entries)
TVD_source   <-  unique(TVD_source) 
TVD_data  <- TVD_source # renaming the TVD data taken from source files to avoid confusion with source in transport
rm(TVD_source)

# checking cases of slaughter house out (89 entries) left in data
# slaughter_out = TVD_data[TVD_data$enterprisetype_source=="SlaughterEnterprise",]


names(AGIS_matched) <-  c("year","id_AGIS","pop_avg","pop_jan1","haltungsform_max", "id_TVD" )
names(TVD_data) <-  c("id_tvd_source","id_tvd_dest","enterprisetype_source","enterprisetype_dest","date","n_pigs","pig_cat","gemeinde_dest",
                      "gemeinde_source","gkode_source","gkodn_source", "gkode_dest", "gkodn_dest" , "year" )

# Removing two errors in TVD data where size of transport is much too large.
TVD_data = TVD_data[TVD_data$id_tvd_source != "2060958",]
TVD_data = TVD_data[TVD_data$id_tvd_source != "1213836",]



TVD_sources  <-  TVD_data[,c("id_tvd_source","enterprisetype_source","date","n_pigs","pig_cat","gemeinde_source","gkode_source","gkodn_source", "year")]
TVD_destinations  <-  TVD_data[,c("id_tvd_dest","enterprisetype_dest","date","n_pigs","pig_cat","gemeinde_dest","gkode_dest","gkodn_dest", "year")]

TVD_sources$source_dest   <-  "source"
TVD_destinations$source_dest   <-  "dest"

names(TVD_sources) <- c("id_TVD","enterprisetype","date","n_pigs","pig_cat","gemeinde","gkode","gkodn", "year", "source_dest")
names(TVD_destinations) <- c("id_TVD","enterprisetype","date","n_pigs","pig_cat","gemeinde","gkode","gkodn", "year", "source_dest")

TVD_all   <-  rbind(TVD_sources,TVD_destinations)
save(TVD_all, file =paste(relative_path_to_processed_data,"TVD_all_transports.RData",sep ="" ))
rm(TVD_sources,TVD_destinations)

## Extracting all the slaugtherhouses and their start and end date as well as geo info  ####
slaugtherhouse_all   <- TVD_all[TVD_all$enterprisetype=="SlaughterEnterprise",]

slaugtherhouse_start_active <- slaugtherhouse_all %>% group_by(id_TVD) %>% summarise(start_date =min(date))
slaugtherhouse_end_active <- slaugtherhouse_all %>% group_by(id_TVD) %>% summarise(end_date = max(date))
slaugtherhouse_geo  <- distinct(slaugtherhouse_all[,c("id_TVD","gemeinde","gkode","gkodn")],id_TVD, .keep_all= TRUE)


slaugtherhouse_list_active  <- merge(slaugtherhouse_start_active,slaugtherhouse_end_active)
slaugtherhouse_list_active  <- merge(slaugtherhouse_list_active,slaugtherhouse_geo)

save(slaugtherhouse_list_active, file =paste(relative_path_to_processed_data,"slaugtherhouse_list_active.RData",sep ="" ))
rm(slaugtherhouse_start_active,slaugtherhouse_end_active,slaugtherhouse_geo,slaugtherhouse_list_active,slaugtherhouse_all)



## Extracting and processing population for all the pig holdings  ####
non_standard_holding_types   <- c("NonComercial", "LiveStockDealerEnterprise" ,  "MedicalCenter","SlaughterEnterprise",
                                  "Unknown", "MarketAuctionExhibition")

holdings_all   <- TVD_all[!(TVD_all$enterprisetype %in% non_standard_holding_types),]
holdings_non_standard   <- TVD_all[(TVD_all$enterprisetype %in% non_standard_holding_types)&
                                     (TVD_all$enterprisetype != "SlaughterEnterprise"),]

holdings_non_standard  <- distinct(holdings_non_standard[,c("id_TVD","gemeinde","gkode","gkodn","enterprisetype")],id_TVD, .keep_all= TRUE)
save(holdings_non_standard, file =paste(relative_path_to_processed_data,"non_standard_holdings_list_active_TVD.RData",sep ="" ))


holdings_start_active <- holdings_all %>% group_by(id_TVD) %>% summarise(start_date =min(date))
holdings_end_active <- holdings_all %>% group_by(id_TVD) %>% summarise(end_date = max(date))
holdings_geo  <- distinct(holdings_all[,c("id_TVD","gemeinde","gkode","gkodn")],id_TVD, .keep_all= TRUE)
holdings_list_active  <- merge(holdings_start_active,holdings_end_active)
holdings_list_active  <- merge(holdings_list_active,holdings_geo)
holdings_list_active_TVD  <- holdings_list_active
save(holdings_list_active_TVD, file =paste(relative_path_to_processed_data,"holdings_list_active_TVD.RData",sep ="" ))
rm(holdings_start_active,holdings_end_active,holdings_geo,holdings_list_active)

## Cleaning AGIS data including duplicate entries and TVD-AGIS id matches#### 

## some TVD codes are not valid codes. here they are replaced by NAs ## 
false_TVD_ID = c("8888", "8888813","8888821" ,"8888829", "8888839", "8888880", "8888882", "8888887", "9999" , "9999822", "9999921" ,"9999991" ,"9999992", "9999998" ,"9999999")
AGIS_matched$id_TVD[(AGIS_matched$id_TVD %in%  false_TVD_ID)] = NA



AGIS_valid_TVD  <- AGIS_matched[!is.na(AGIS_matched$id_TVD),]
AGIS_no_valid_TVD  <- AGIS_matched[is.na(AGIS_matched$id_TVD),]

## Some of the AGIS without TVD on one year may have tvds in other years so we re_match them 
AGIS_no_valid_TVD_rematched  <- left_join(AGIS_no_valid_TVD,distinct(AGIS_valid_TVD[,c("id_AGIS","id_TVD")],id_AGIS, .keep_all= TRUE), by="id_AGIS")
AGIS_no_valid_TVD_rematched  <- AGIS_no_valid_TVD_rematched[,c(1,2,3,4,5,7)]
names(AGIS_no_valid_TVD_rematched) = c("year","id_AGIS","pop_avg","pop_jan1","haltungsform_max", "id_TVD" )
## the ones for which a match has been found are added back to the list of AGIS entries with valid TVD
AGIS_valid_TVD  <-  rbind(AGIS_valid_TVD,AGIS_no_valid_TVD_rematched[!is.na(AGIS_no_valid_TVD_rematched$id_TVD),])
AGIS_no_valid_TVD  <- AGIS_no_valid_TVD_rematched[is.na(AGIS_no_valid_TVD_rematched$id_TVD),]


list_AGIS_no_valid_TVD = distinct(AGIS_no_valid_TVD,id_AGIS, .keep_all= TRUE)
list_AGIS_valid_TVD = distinct(AGIS_valid_TVD,id_AGIS, .keep_all= TRUE)

## checking how many agis have multiple tvd and vice versa, all within the AGIS data
valid_agis_tvd_pairs= unique(AGIS_valid_TVD[,c("id_AGIS","id_TVD" )])

AGIS_tally = valid_agis_tvd_pairs %>% group_by(id_AGIS) %>% tally()
TVD_tally = valid_agis_tvd_pairs %>% group_by(id_TVD) %>% tally()

pair_counts = left_join(valid_agis_tvd_pairs,AGIS_tally)
names(pair_counts) = c("id_AGIS","id_TVD" ,"tvds_matched_for_agis" )
pair_counts = left_join(pair_counts,TVD_tally)
names(pair_counts) = c("id_AGIS","id_TVD" ,"tvds_matched_for_agis","agis_matched_for_tvd" )


pair_matrix  =   population_mat <-  matrix(0, nrow = max(AGIS_tally$n), ncol = max(TVD_tally$n))
for(i in 1:max(AGIS_tally$n)){
  for(j in 1:max(TVD_tally$n)){
    pair_matrix[i,j] = nrow(pair_counts[(pair_counts$agis_matched_for_tvd == j)& (pair_counts$tvds_matched_for_agis == i),])
  }
}
print("count of TVD and AGIS Ids with numbers of duplications" )
pair_matrix
sum(sum(pair_matrix))


## we separate the farms that have a unique TVD and AGIS pair
clean_pairs  <- pair_counts %>% filter((agis_matched_for_tvd == 1)&(tvds_matched_for_agis== 1))
num_pairs= nrow(clean_pairs)

## reformat the AGIS inventory data. the slow loop is only to use the same procedure with the multiple pair cases below.
inventory_clean_data = data.frame(swinenet_id=integer(),
                                  year=numeric(),
                                  match_type=character(),
                                  pop_avg=double(),
                                  pop_jan1=double(),
                                  haltungsform_max=integer())

id_reference_list = data.frame(swinenet_id=integer(),
                               id_AGIS=integer(),
                               id_TVD=integer())

for (j in 1:num_pairs){
  check_match = AGIS_matched[(AGIS_matched$id_AGIS  %in% clean_pairs$id_AGIS[j])|
                               (AGIS_matched$id_TVD  %in% clean_pairs$id_TVD[j]), ]
  
  current_inventory_data = data.frame(swinenet_id=rep(j,nrow(check_match)),
                                      year=check_match$year,
                                      match_type=rep("single_pair",nrow(check_match)),
                                      pop_avg=check_match$pop_avg,
                                      pop_jan1=check_match$pop_jan1,
                                      haltungsform_max=check_match$haltungsform_max)
  
  inventory_clean_data = rbind(inventory_clean_data,current_inventory_data)
  
  current_id_reference =  na.omit(data.frame( swinenet_id = j,
                                      id_AGIS=unique(check_match$id_AGIS),
                                      id_TVD=unique(check_match$id_TVD)  ))
  id_reference_list = rbind(id_reference_list,current_id_reference)
}



## extract match and process the farms that have multiple TVD or AGIS pair
pair_counts <- pair_counts %>% filter(!(agis_matched_for_tvd == 1)|!(tvds_matched_for_agis== 1))


##  Here we use a network analysis package igraph to connect all the ids of a single holding.
library(igraph)
AA = as.matrix(pair_counts[,c(1,2)])
id_graph = graph_from_edgelist(AA, directed = FALSE)
BB = components(id_graph)

pairs = data.frame(id = rep(0,length(BB$membership)), holding= rep(0,length(BB$membership)))
for (i in 1 : length(BB$membership)){
  pairs$id[i] = names(BB$membership[i])
  pairs$holding[i] = BB$membership[[i]]
}

temp_tvd_ids = pairs[ nchar(pairs$id) == 7,]
pair_counts = left_join(pair_counts,temp_tvd_ids, by = c("id_TVD"="id"))
rm(AA,BB,temp_tvd_ids,pairs)





##  the pair_counts dataframe has in the last column a holding ID that groups all the pairs that share IDs
##  Now we do some cleaning to remove the duplicates and create a new record of inventories and haltungsform
##  where the AGIS and TVD ids are lists of the ids that apply to that holding


multi_holdings = max(pair_counts$holding)
start_siwnenet_id = max(inventory_clean_data$swinenet_id)


for(j in 1:multi_holdings){
  AGIS_node_list = pair_counts[pair_counts$holding==j,]
  check_match = AGIS_matched[(AGIS_matched$id_AGIS  %in% AGIS_node_list$id_AGIS)|
                               (AGIS_matched$id_TVD  %in% AGIS_node_list$id_TVD), ]
  
  year_tally = check_match %>% group_by(year) %>% tally()
  tvd_tally = check_match %>% group_by(id_TVD) %>% tally()
  agis_tally = check_match %>% group_by(id_AGIS) %>% tally()
  if(nrow(check_match)==agis_tally$n & max(tvd_tally$n)*max(year_tally$n)==nrow(check_match)){
    
    check_match_grouped <- unique(check_match[,-c(2)])
    check_match_grouped <- check_match_grouped %>% group_by(year) %>% summarise(pop_avg =sum(pop_avg),
                                                                                pop_jan1 =sum(pop_jan1),
                                                                                haltungsform_max =max(haltungsform_max))    

    n_entries = nrow(check_match_grouped)
    current_inventory_data = data.frame(swinenet_id=rep(j+start_siwnenet_id,n_entries),
                                        year=check_match_grouped$year,
                                        match_type=rep("Duplicated tvds and inventory",n_entries),
                                        pop_avg=check_match_grouped$pop_avg,
                                        pop_jan1=check_match_grouped$pop_jan1,
                                        haltungsform_max=check_match_grouped$haltungsform_max)
    
    current_id_reference =  data.frame( swinenet_id = j+num_pairs,
                                        id_AGIS=0,
                                        id_TVD=0)
    current_id_reference$id_AGIS = list(na.omit(unique(check_match$id_AGIS)))
    current_id_reference$id_TVD =  list(na.omit(unique(check_match$id_TVD)))
    
    
  } else if(nrow(check_match)==tvd_tally$n & nrow(check_match)==nrow(year_tally)){
    check_match_grouped <- unique(check_match[,-c(2)])
    check_match_grouped <- check_match_grouped %>% group_by(year) %>% summarise(pop_avg =sum(pop_avg),
                                                                                pop_jan1 =sum(pop_jan1),
                                                                                haltungsform_max =max(haltungsform_max))
    n_entries = nrow(check_match_grouped)
    current_inventory_data = data.frame(swinenet_id=rep(j+start_siwnenet_id,n_entries),
                                        year=check_match_grouped$year,
                                        match_type=rep("Change in AGIS but one constant tvd",n_entries),
                                        pop_avg=check_match_grouped$pop_avg,
                                        pop_jan1=check_match_grouped$pop_jan1,
                                        haltungsform_max=check_match_grouped$haltungsform_max)
    
    current_id_reference =  data.frame( swinenet_id = j+num_pairs,
                                        id_AGIS=0,
                                        id_TVD=0)
    current_id_reference$id_AGIS = list(na.omit(unique(check_match$id_AGIS)))
    current_id_reference$id_TVD =  list(na.omit(unique(check_match$id_TVD)))
    
  } else {
    check_match_notvd <- unique(check_match[,-c(6)])
    check_match_notagis <- check_match_notvd %>% group_by(year) %>% summarise(pop_avg =sum(pop_avg),
                                                                              pop_jan1 =sum(pop_jan1),
                                                                              haltungsform_max =max(haltungsform_max))
    current_inventory_data = data.frame(swinenet_id=rep(j+start_siwnenet_id,nrow(check_match_notagis)),
                                        year=check_match_notagis$year,
                                        match_type=rep("else",nrow(check_match_notagis)),
                                        pop_avg=check_match_notagis$pop_avg,
                                        pop_jan1=check_match_notagis$pop_jan1,
                                        haltungsform_max=check_match_notagis$haltungsform_max)
    
    current_id_reference =  data.frame( swinenet_id = j+num_pairs,
                                        id_AGIS=0,
                                        id_TVD=0)
    current_id_reference$id_AGIS = list(na.omit(unique(check_match$id_AGIS)))
    current_id_reference$id_TVD =  list(na.omit(unique(check_match$id_TVD)))
    
    ## print(paste(j," out of ",multi_holdings, sep = "" ))
    ## print(arrange(check_match,year))
    ## print(list(unique(check_match$id_AGIS)))
    ## print(list(unique(check_match$id_TVD)))
    ## print(arrange(check_match_notagis,year))
    ## multi_holdings_data$match_type[j] = readline(prompt = "mixed matching y or n ")
    
    
  }
  inventory_clean_data = rbind(inventory_clean_data,current_inventory_data)
  id_reference_list = rbind(id_reference_list,current_id_reference)
  
  
}

inventory_clean_data = unique(inventory_clean_data) # making sure that there are no duplicates 



## Checking some basic matches between AGIS and TVD data####

TVDs_shared_AGIS_standard_TVD = intersect(unique(holdings_list_active_TVD$id_TVD),
                                          as.integer(unique(unlist(id_reference_list$id_TVD))))

TVD_only = setdiff(unique(holdings_list_active_TVD$id_TVD),
                   as.integer(unique(unlist(id_reference_list$id_TVD))))

AGIS_only= setdiff(as.integer(unique(unlist(id_reference_list$id_TVD))),
                   unique(holdings_list_active_TVD$id_TVD))

TVDs_shared_AGIS_non_standard_TVD = intersect(unique(holdings_non_standard$id_TVD),
                                              as.integer(unique(unlist(id_reference_list$id_TVD))))

TVDs_shared_AGIS_all_TVD = intersect(unique(TVD_all$id_TVD),
                                     as.integer(unique(unlist(id_reference_list$id_TVD))))

load(paste(relative_path_to_processed_data,"slaugtherhouse_list_active.RData",sep ="" ))
TVDs_shared_AGIS_slaughter_TVD = intersect(unique(slaugtherhouse_list_active$id_TVD),
                                           as.integer(unique(unlist(id_reference_list$id_TVD))))


print("Number of matches between all TVD transports and AGIS cleaned")
print(length(TVDs_shared_AGIS_all_TVD))
print("Number of matches between standard holding TVD transports and AGIS cleaned")
print(length(TVDs_shared_AGIS_standard_TVD))
print("Number of matches between non-standard holding TVD transports and AGIS cleaned")
print(length(TVDs_shared_AGIS_non_standard_TVD))
print("Number of matches between slaughterhouses TVD transports and AGIS cleaned")
print(length(TVDs_shared_AGIS_slaughter_TVD))
print("Number of holdings in AGIS not found in TVD")
print(length(AGIS_only))
print("Number of holdings in TVD not found in AGIS")
print(length(TVD_only))




## The two cleaned outputs are saved for reference
save(inventory_clean_data,file = paste(relative_path_to_processed_data,"inventory_swinenet_id.RData",sep=""))


## Build two reference lists for IDs that are easier to look up for conversion to and from swinenet_id to AGIS or TVD ####

id_reference_flat_TVD = data.frame(swinenet_id = integer(),
                                   id_TVD = character())
id_reference_flat_AGIS = data.frame(swinenet_id = integer(),
                                    id_AGIS = integer())
for(i in 1:nrow(id_reference_list)) {
  temp_holding = id_reference_list[i,]
  temp_TVD = unlist(temp_holding$id_TVD)
  temp_TVD = data.frame(swinenet_id =rep(id_reference_list$swinenet_id[i],length(temp_TVD)),
                        id_TVD =temp_TVD)
  id_reference_flat_TVD = rbind(id_reference_flat_TVD,temp_TVD)
  
  temp_AGIS = unlist(temp_holding$id_AGIS)
  temp_AGIS = data.frame(swinenet_id =rep(id_reference_list$swinenet_id[i],length(temp_AGIS)),
                         id_AGIS =temp_AGIS)
  id_reference_flat_AGIS = rbind(id_reference_flat_AGIS, temp_AGIS)
  
}

save(id_reference_list,file = paste(relative_path_to_processed_data,"reference_list_swinenet_AGIS_TVD_ids.RData",sep=""))
save(id_reference_flat_AGIS, file =paste(relative_path_to_processed_data,"id_reference_flat_AGIS.RData",sep ="" ))
save(id_reference_flat_TVD, file =paste(relative_path_to_processed_data,"id_reference_flat_TVD.RData",sep ="" ))


## getting some of the attributes-parameters from TVD and trader datasets and matche into swinenet ID ####


load(paste(relative_path_to_processed_data,"TVD_all.RData",sep ="" ))
load(paste(relative_path_to_processed_data,"TVD_traders.RData",sep ="" ))


TVD_param = unique(data.frame(TVD_all[, c("swinenet_id", "enterprisetype","gemeinde","gkode" ,"gkodn"  )]))

load(paste(relative_path_to_processed_data,"TVD_merged_tour.RData",sep ="" ))



trader_param_source = data.frame(tvd_traders_2014_2019 [, c("tvd_source", "farm_type_source" )])
trader_param_dest = data.frame(tvd_traders_2014_2019 [, c( "tvd_dest", "farm_type_dest" )])
names(trader_param_source) = c("id_TVD", "farm_type_ML" )
names(trader_param_dest) = c("id_TVD", "farm_type_ML" )
trader_param = unique(rbind(trader_param_source,trader_param_dest ))
trader_param$id_TVD = as.character(trader_param$id_TVD)

trader_swinenet =  left_join(trader_param,id_reference_flat_TVD, by = c("id_TVD" ="id_TVD"  ) )


TVD_merged$tvd_source <- as.character(TVD_merged$tvd_source)
TVD_merged$tvd_dest <- as.character(TVD_merged$tvd_dest)
TVD_merged  <- left_join(TVD_merged,id_reference_flat_TVD,by = c("tvd_source" ="id_TVD"  ))
names(TVD_merged)[names(TVD_merged)=="swinenet_id"]="swinenet_id_source"
TVD_merged  <- left_join(TVD_merged,id_reference_flat_TVD,by = c("tvd_dest" ="id_TVD"  ))
names(TVD_merged)[names(TVD_merged)=="swinenet_id"]="swinenet_id_dest"

parameters_tvd_ML_source = TVD_merged[,c("swinenet_id_source", "farm_type_source") ]
parameters_tvd_ML_dest = TVD_merged[,c("swinenet_id_dest", "farm_type_dest") ]
names(parameters_tvd_ML_source) = c("swinenet_id", "farm_type_ML")
names(parameters_tvd_ML_dest) = c("swinenet_id", "farm_type_ML")

parameters_tvd_ML = unique(rbind(parameters_tvd_ML_source,parameters_tvd_ML_dest ))

parameters_tvd_ML = left_join(TVD_param,parameters_tvd_ML, by = c("swinenet_id"="swinenet_id") )

parameters_tvd_ML = parameters_tvd_ML[parameters_tvd_ML$gemeinde <= 7011 & !is.na(parameters_tvd_ML$gemeinde)  , ]


parameters_AGIS = left_join(AGIS_matched[,c("id_TVD", "haltungsform_max") ], id_reference_flat_TVD, by = c("id_TVD"="id_TVD"))
parameters_AGIS <- parameters_AGIS %>% group_by(swinenet_id) %>% summarise(haltungsform = max(haltungsform_max))
  
parameters_all_final  <- left_join(parameters_tvd_ML, parameters_AGIS, by = c("swinenet_id"="swinenet_id"))



##  removing possible duplicates
parameters_all_final = parameters_all_final[duplicated(parameters_all_final[, c("swinenet_id")]) == FALSE,]

save(parameters_all_final, file =paste(relative_path_to_processed_data,"parameters_all_final.RData",sep ="" ))



load(paste(relative_path_to_processed_data ,"TVD_source.RData",sep =""))
TVD_source$tvd_source = as.character(TVD_source$tvd_source)
TVD_source$tvd_dest = as.character(TVD_source$tvd_dest)

TVD_edge_swinenet  <- left_join(TVD_source, id_reference_flat_TVD, by = c("tvd_source" ="id_TVD"  )   )
TVD_edge_swinenet  <- left_join(TVD_edge_swinenet, id_reference_flat_TVD, by = c("tvd_dest" ="id_TVD"  )   )
TVD_edge_swinenet   <-  TVD_edge_swinenet[, c( "date", "n_pigs","swinenet_id.x","swinenet_id.y"   )]
names(TVD_edge_swinenet) = c( "date", "n_pigs","swinenet_id_source","swinenet_id_dest"   )
save(TVD_edge_swinenet, file =paste(relative_path_to_processed_data,"TVD_edge_swinenet.RData",sep ="" ))


