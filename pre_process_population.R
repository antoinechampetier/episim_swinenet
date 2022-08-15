#######################################
#' @title Preprocessing of AGIS and TVD data to build population change rates for simulations
#' @author : Antoine Champetier
#' @date : 29.11.2021
#' @description: Build a population net rate and extrapolations based on AGIS inventories and TVD flows to track population and density-dependent transmission
#######################################


## Basic set up libraries and paths to data ####
getwd()
#setwd("./episim_swinenet")
# setwd("~/swinenet_episim/episim_swinenet")

suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(readxl))
suppressMessages(library(lubridate))
suppressMessages(library(ggplot2))

rm(list = ls())
relative_path_to_data = "../epi_data" ## Note that the epi_data folder should contain three RData files prepared with import_source_AGIS_TVD_merged_trader.R
relative_path_to_processed_data = "../epi_data/processed/"
relative_path_to_trajectory_figures = "../trajectory_figures/"
relative_path_to_trajectory_figures_pass = "../trajectories_pass/"
relative_path_to_trajectory_figures_fail = "../trajectories_fail/"
relative_path_to_trajectory_figures_no_criteria = "../trajectories_no_criteria/"

load(paste(relative_path_to_processed_data,"slaugtherhouse_list_active.RData",sep ="" ))
load(paste(relative_path_to_processed_data,"holdings_list_active_TVD.RData",sep ="" ))
load(paste(relative_path_to_processed_data,"non_standard_holdings_list_active_TVD.RData",sep ="" ))
load(paste(relative_path_to_processed_data,"TVD_all_transports.RData",sep ="" ))
load(paste(relative_path_to_processed_data,"inventory_swinenet_id.RData",sep=""))
load(paste(relative_path_to_processed_data,"reference_list_swinenet_AGIS_TVD_ids.RData",sep=""))
load(paste(relative_path_to_processed_data,"AGIS_matched.RData",sep=""))

load(paste(relative_path_to_processed_data,"id_reference_flat_AGIS.RData",sep ="" ))
load(paste(relative_path_to_processed_data,"id_reference_flat_TVD.RData",sep ="" ))




## load the vertex types from the ML clustering output
load(paste(relative_path_to_processed_data,"TVD_merged_tour.RData",sep=""))

TVD_merged$type_to_type[TVD_merged$type_to_type == ""] <- "none -> none"
TVD_merged <- cbind(TVD_merged, data.frame(do.call("rbind",strsplit(TVD_merged$type_to_type, split = " -> ", fixed = TRUE))))

holding_cluster_type_source <- TVD_merged[,c("tvd_source", "X1")]
holding_cluster_type_dest <- TVD_merged[,c("tvd_dest", "X2")]
names(holding_cluster_type_source) = c("id_TVD", "holding_type_cluster")
names(holding_cluster_type_dest) = c("id_TVD", "holding_type_cluster")
holding_cluster_type <- unique( rbind(holding_cluster_type_source,holding_cluster_type_dest ))
holding_cluster_type$id_TVD <- as.character(holding_cluster_type$id_TVD)
holding_cluster_type <- left_join(holding_cluster_type, id_reference_flat_TVD, by = c("id_TVD" = "id_TVD"))

holding_cluster_type <- na.omit(holding_cluster_type[holding_cluster_type$holding_type_cluster!="none", ])

rm(holding_cluster_type_source,TVD_merged, holding_cluster_type_dest )





## Converting TVD data to Swinenet id ####
TVD_all$id_TVD   <- as.character(TVD_all$id_TVD)


## building swinenet id for the slaughterhouses
slaugtherhouse_list_active$swinenet_id = 10000 +  c(1:nrow(slaugtherhouse_list_active))
full_key_ids = rbind(id_reference_flat_TVD,slaugtherhouse_list_active[,c("swinenet_id","id_TVD")])

TVD_all_t  <- left_join(TVD_all,full_key_ids,by = "id_TVD")

## building swinenet id for the Summering Husbandry and the holdings that have no agis record

summer_husbandry = TVD_all_t[(TVD_all_t$enterprisetype == "SummeringHusbandry") & is.na(TVD_all_t$swinenet_id),]
summer_husbandry_key = unique(summer_husbandry$id_TVD)
summer_husbandry_key = data.frame(swinenet_id = 20000 + c(1:length(summer_husbandry_key)), id_TVD = summer_husbandry_key)
full_key_ids = rbind(full_key_ids, summer_husbandry_key)

TVD_all_t  <- left_join(TVD_all,full_key_ids,by = "id_TVD")


no_agis_other_type = TVD_all_t[is.na(TVD_all_t$swinenet_id),]
no_agis_other_type = unique(no_agis_other_type$id_TVD)
no_agis_other_type_key = data.frame(swinenet_id = 30000 + c(1:length(no_agis_other_type)), id_TVD = no_agis_other_type)
full_key_ids = rbind(full_key_ids, no_agis_other_type_key)


TVD_all  <- left_join(TVD_all,full_key_ids,by = "id_TVD")
rm(TVD_all_t,temp_AGIS,temp_TVD,temp_holding,summer_husbandry,no_agis_other_type,i)

save(TVD_all, file =paste(relative_path_to_processed_data,"TVD_all.RData",sep ="" ))



## PLots and summaries for flows for each group of holdings: ####
## standard :                       swinenet_id   0 to 10 000 
## slaughterhouses :                swinenet_id   10 001 to 20 000 
## summer husbandry :               swinenet_id   20 001 to 30 000 
## non_AGIS non slaughter summer :  swinenet_id   30 001 and on  

library(gridExtra)  
library(DAAG)
library(ggpubr)
# setting up day counts from start of period of data to track inventories
startdate <- dmy("01/01/2014")

TVD_all$day_start_period <-    floor(as.numeric(difftime(TVD_all$date, startdate ),units = "days"))+2


duration = max(TVD_all$day_start_period)
date_key = startdate + days(c(0:(duration-1)))
date_key_year = year(date_key)


inventory_dates <-  c("01/01/2014","01/01/2015","01/01/2016","01/01/2017","01/01/2018","01/01/2019","01/01/2020")
inventory_dates <-  data.frame(dates = dmy(inventory_dates), year = year(dmy(inventory_dates)))
inventory_dates$day_start_period <-  floor(as.numeric(difftime(inventory_dates$date,startdate ),units = "days"))+1

# for each holding in standard list, go through inventory estimation procedure
standard_ids = full_key_ids$swinenet_id[full_key_ids$swinenet_id <= 10000]

# variables to record the inventories and pop changes (net birth deaths) for all vertices. it is always the inventory at the start of the day, before transport of population change
pop_vertex  <- matrix(0,nrow = duration+1, ncol = nrow(id_reference_list))  
net_pop_change_vertex  <- matrix(0,nrow = duration+1, ncol = nrow(id_reference_list)) 
delta_estim_data_inventory  <- matrix(0,nrow = duration, ncol = nrow(id_reference_list)) 

library(forecast)
signatures = data.frame("swinenet_id" = integer(), 
                        "frequency_all"= integer(),
                        "frequency_in"= integer(),
                        "frequency_out"= integer(),
                        "size_out" = double(),
                        "size_in" = double())


for (i in standard_ids ){  

  hold_inventory <- inventory_clean_data[inventory_clean_data$swinenet_id == i,]
  
  
  if(nrow(hold_inventory) > 1 & (nrow(hold_inventory) == max(hold_inventory$year)-min(hold_inventory$year) + 1)){
  trajectory = matrix(0, nrow = duration+1, ncol = 6) 
  
  # First column is the inventories on January 1
  trajectory[inventory_dates$day_start_period[inventory_dates$year %in% hold_inventory$year],1] = 
    hold_inventory$pop_jan1[hold_inventory$year %in% inventory_dates$year]
  # Second column is the inventories on average for each year
  trajectory[inventory_dates$day_start_period[inventory_dates$year %in% hold_inventory$year],2] = 
    hold_inventory$pop_avg[hold_inventory$year %in% inventory_dates$year]


  hold_flows <- TVD_all[TVD_all$swinenet_id == i,]
  hold_flows_track_tvd <- hold_flows
    
  hold_flows$n_pigs[hold_flows$source_dest == "source"]= - hold_flows$n_pigs[hold_flows$source_dest == "source"]
  hold_flows_track_tvd <- hold_flows
  
  year_flows  <- hold_flows %>% group_by(year) %>% summarise(net_flow = sum(n_pigs))
  tvd_active <- c(min(hold_flows$date),max(hold_flows$date))
  hold_flows <- hold_flows %>% group_by(day_start_period) %>% summarise(net_flow = sum(n_pigs))
  
  inventory_active_start <- inventory_dates$day_start_period[inventory_dates$year == min(hold_inventory$year)]
  inventory_active_end <- inventory_dates$day_start_period[inventory_dates$year == max(hold_inventory$year)]
  
  tvd_active_start_t = which(date_key == tvd_active[1])
  tvd_active_end_t = which(date_key == tvd_active[2])
  start_trajectory_t = max(inventory_active_start,tvd_active_start_t )
  end_trajectory_t = min(inventory_active_end,tvd_active_end_t )
  
  
  # third column is the flows on the day they occur
  trajectory[hold_flows$day_start_period,3] = hold_flows$net_flow

  # forth column tracks the cummulative flows 
  trajectory[1,4]=  trajectory[1,3]
  for (t in 2:duration) {
    trajectory[t,4]=  trajectory[t-1,4]+trajectory[t,3]
  }
  
  for (y in 1:(length(unique(hold_inventory$year))-1) ){
    hold_inventory$delta_pop[y] = year_flows$net_flow[ y] - (hold_inventory$pop_jan1[y+1]-hold_inventory$pop_jan1[y])
  }

  # fifth column tracks the net birth-date adjustment
  # sixth column tracks the population with the net birth-date adjustment

  trajectory[1,6]=  trajectory[1,1]+trajectory[1,3]
  t=2

  while (t <= duration & t <= end_trajectory_t){
    if(length( hold_inventory$delta_pop[hold_inventory$year == year(date_key[t])]) > 0){
      trajectory[t,5] = -hold_inventory$delta_pop[hold_inventory$year == year(date_key[t])]/365
      trajectory[t,6] =  trajectory[t-1,6]+trajectory[t,5] +trajectory[t,3]
    
    } else {
      trajectory[t,6]=   trajectory[t-1,6] +trajectory[t,3]
    }
 
    t = t+1

  } 
  
  
  if (length(tvd_active_end_t)>0){
  if (tvd_active_end_t >= t){
    trajectory[t,5] = mean(trajectory[2:(t-1),5])
    while (t <= duration+1 ){
      trajectory[t,6]=  trajectory[t-1,6]+trajectory[t,5]+trajectory[t,3]
      t = t+1
    }
  }
    
  }
 
 
  
## Preparing report with tables and figures ##### 
  
  trajectory_df = data.frame(date_key,trajectory[1:duration,])
  trajectory_df$year = year(trajectory_df$date_key)
  trajectory_avg <- trajectory_df %>% group_by(year) %>% summarise(estim_average = mean(X6))
  average_diff <- left_join(hold_inventory,trajectory_avg, by = "year" )
  trajectory_df  <- left_join(trajectory_df,average_diff, by = "year" )
  
  
  hold_inventory$net_birth_death = hold_inventory$delta_pop
  net_transport  <- trajectory_df %>% group_by(year) %>% summarise(net_transport = sum(X3))
  hold_inventory   <- left_join(hold_inventory,net_transport, by = "year" )
  estim_average  <- trajectory_df %>% group_by(year) %>% summarise(estim_average = mean(estim_average))
  hold_inventory   <- left_join(hold_inventory,estim_average, by = "year" )
  hold_inventory$rate_birth_death =   - round( hold_inventory$net_birth_death /  hold_inventory$estim_average, digits = 2)
  hold_inventory$net_birth_death =   - round(hold_inventory$net_birth_death, digits = 2)
  

  plot_figure = FALSE
  if (plot_figure){
  
  
  trajectory_plot <- ggplot(data=trajectory_df, aes(x=date_key, y=X6, group = year(date_key), colour = year(date_key))) + 
    scale_color_gradient(low="green", high="blue")+
    geom_line() + labs(y= "Pigs", x=element_blank()) + theme(legend.position="none") +
    ggtitle(paste("Inventory holding n# ",i,". Red line: estimated avg, black line: data avg, dotted: data Jan 1 ",sep = "")) +
    geom_line(aes(x=date_key, y=estim_average), color = "red") + 
    geom_line(aes(x=date_key, y=pop_avg), color = "black") +
    geom_line(aes(x=date_key, y=pop_jan1), color = "blue",linetype="dotted" )

  
 
  hold_flows_track_tvd$id_TVD  <- as.integer(hold_flows_track_tvd$id_TVD)
  tvd_plot  <- trajectory_plot
  if(nrow(hold_flows_track_tvd)>0){
    tvd_plot <- ggplot(data=hold_flows_track_tvd, aes(x= day_start_period, ymax=n_pigs, ymin=0, fill = id_TVD,group = id_TVD, colour = id_TVD )) + 
      scale_color_gradient(low="red", high="blue")+
      geom_linerange() + labs(y= "Pigs", x=element_blank()) + theme(legend.position="none") +
      ggtitle(paste("Transport events holding n# ",i,sep = "")) +
      xlim(1, duration)
  }

## signature of TVD flows 
  holding_tvds = TVD_all[TVD_all$swinenet_id == i ,]
  if(sum(holding_tvds$n_pigs)!=0){
    dat1 =  holding_tvds %>% group_by(day_start_period) %>% summarise(flow = sum(n_pigs))
    dat2 =  left_join(data.frame("day_start_period" = c(1:duration)),dat1,by = "day_start_period")
    dat2$flow[is.na(dat2$flow)] = 0
    timeseries = ts(dat2$flow, start = 1, frequency = 365) 
    frequ_all = findfrequency(timeseries)
    
    dat1 =  holding_tvds[holding_tvds$source_dest == "source",] %>% group_by(day_start_period) %>% summarise(flow = sum(n_pigs))
    dat2 =  left_join(data.frame("day_start_period" = c(1:duration)),dat1,by = "day_start_period")
    dat2$flow[is.na(dat2$flow)] = 0
    timeseries = ts(dat2$flow, start = 1, frequency = 365) 
    if(sum(dat2$flow)!=0){
      frequ_out = findfrequency(timeseries)
    } else {frequ_out = NA}
    
    dat1 =  holding_tvds[holding_tvds$source_dest == "dest",] %>% group_by(day_start_period) %>% summarise(flow = sum(n_pigs))
    dat2 =  left_join(data.frame("day_start_period" = c(1:duration)),dat1,by = "day_start_period")
    dat2$flow[is.na(dat2$flow)] = 0
    timeseries = ts(dat2$flow, start = 1, frequency = 365)
    if(sum(dat2$flow)!=0){
      frequ_in = findfrequency(timeseries)
    } else {frequ_in = NA}
    
    
    size_out = mean(holding_tvds$n_pigs[holding_tvds$source_dest == "source"]) 
    size_in = mean(holding_tvds$n_pigs[holding_tvds$source_dest == "dest"]) 
    
    signatures_holding =  data.frame("swinenet_id" = i,
                                              "frequency_all"=frequ_all,
                                              "frequency_out"= frequ_out,
                                              "frequency_in"=frequ_in,
                                              "size_in"=size_in,
                                              "size_out"=size_out)

    signatures = rbind(signatures,signatures_holding)
  }else{
    signatures_holding =  data.frame("swinenet_id" = i,
                                         "frequency_all"=0,
                                         "frequency_out"= 0,
                                         "frequency_in"=0,
                                         "size_in"=0,
                                         "size_out"=0)
    }
    

  inventory_table <- ggplot()+ annotation_custom(tableGrob(hold_inventory[,c("year","pop_jan1",  "net_birth_death",  "net_transport",   "rate_birth_death" )]))
  

  
  average_error = round( mean(abs((trajectory_df$estim_average[inventory_dates$day_start_period]- trajectory_df$pop_avg[inventory_dates$day_start_period]) / 
         trajectory_df$estim_average[inventory_dates$day_start_period])*100, na.omit = TRUE), digits = 2)

  holding_table <- data.frame("Match type" = hold_inventory$match_type[1],
                              "Haltungs  form" = hold_inventory$haltungsform_max[1],
                              "Low point" = round(min(trajectory[,6]),digits = 0),
                              "High point" = round(max(trajectory[,6]),digits = 0),
                              "Start TVD" =   tvd_active[1],
                              "End TVD" =   tvd_active[2],
                              "Perct. diff invent." = average_error,
                              "cluster type" = holding_cluster_type$holding_type_cluster[holding_cluster_type$swinenet_id == 10]) 

  
  holding_table <- ggplot() + 
    annotation_custom(tableGrob(holding_table)) 
  
  
  agis_id = id_reference_flat_AGIS$id_AGIS[id_reference_flat_AGIS$swinenet_id == i]
  matching_table <- AGIS_matched[AGIS_matched$ID %in% agis_id,]
  signatures_holding <- ggplot() + annotation_custom(tableGrob(signatures_holding))  
  matching_table <- ggplot() + annotation_custom(tableGrob(matching_table))  
  
  lay <- rbind(c(1,1,1,1,1,1),
               c(1,1,1,1,1,1),
               c(1,1,1,1,1,1),
               c(2,2,2,2,2,2),
               c(2,2,2,2,2,2),
               c(2,2,2,2,2,2),
               c(3,3,3,3,3,3),
               c(6,6,6,6,6,6),
               c(4,4,4,5,5,5),
               c(4,4,4,5,5,5),
               c(4,4,4,5,5,5))

  g_all <-grid.arrange(trajectory_plot, tvd_plot, holding_table ,inventory_table, matching_table, signatures_holding, layout_matrix = lay)
  
  
  if(!is.na(average_error)){
  if( average_error < 30 & min(trajectory[,5],na.omit = TRUE)>= -5 ){
    png(paste(relative_path_to_trajectory_figures_pass,"traj",i,".png"),height=900,width=800)
    print(g_all)
    dev.off() 
  } else{
    png(paste(relative_path_to_trajectory_figures_fail,"traj",i,".png"),height=900,width=800)
    print(g_all)
    dev.off() 
  }
  }else{
    png(paste(relative_path_to_trajectory_figures_no_criteria,"traj",i,".png"),height=900,width=800)
    print(g_all)
    dev.off() 
  }
  
  
  }
  
  
  pop_vertex[,i]  <- trajectory[,6]  
  net_pop_change_vertex[,i]   <- trajectory[,5]
  delta_estim_data_inventory[,i]   <- (trajectory_df$estim_average-trajectory_df$pop_avg) / trajectory_df$pop_avg
  }
}


delta_estim_data_inventory_year   <-  delta_estim_data_inventory[inventory_dates$day_start_period+1,]
net_pop_change_vertex_year   <-  net_pop_change_vertex[inventory_dates$day_start_period+1,]



# intermediary save used for dev. can be removed.
#save(pop_vertex, file =paste(relative_path_to_processed_data,"population_intermed.RData",sep ="" ))
#save(net_pop_change_vertex_year, file =paste(relative_path_to_processed_data,"delta_pop_intermed_year.RData",sep ="" ))
#save(delta_estim_data_inventory_year, file =paste(relative_path_to_processed_data,"pop_avg_error_intermed.RData",sep ="" ))

#load(paste(relative_path_to_processed_data,"population_intermed.RData",sep ="" ))
#load(paste(relative_path_to_processed_data,"delta_pop_intermed.RData",sep ="" ))
#load(paste(relative_path_to_processed_data,"pop_avg_error_intermed.RData",sep ="" ))


#  fixing temporary of inventories and pop_increases to be able to run episim with them
pop_vertex[pop_vertex < 0] = 0 #  remove negative populations

max_pop    <- inventory_clean_data  %>% group_by(swinenet_id) %>% summarise(max_jan1 = max(pop_jan1))

pop_vertex[,max_pop$max_jan1 < 5 ] = 5  #  for very small holdings, fix population to 5 since they are always quite messy
net_pop_change_vertex_year[,max_pop$max_jan1 < 5 ] = 0 #  for very small holdings, fix population to 5 since they are always quite messy
# note that this will need to be accounted for in the simulation to avoid transport changing the population.


# setting the start and end dates of activity of each vertex based on transport and inventory
start_transport  <-  TVD_all  %>% group_by(swinenet_id) %>% summarise(start = min(date))
end_transport  <-  TVD_all  %>% group_by(swinenet_id) %>% summarise(end = max(date))    
start_inventory   <-   inventory_clean_data  %>% group_by(swinenet_id) %>% summarise(start = min(year))
end_inventory   <-   inventory_clean_data  %>% group_by(swinenet_id) %>% summarise(end = max(year))

start_transport$start      <-    as_date(start_transport$start)
end_transport$end      <-    as_date(end_transport$end )

start_inventory$start     <-  make_datetime(start_inventory$start, 1, 1)
end_inventory$end     <-    make_datetime(end_inventory$end, 1, 1)

start_inventory$start     <-    ymd(start_inventory$start )
end_inventory$end     <-    ymd(end_inventory$end )

start_activity_dates      <-     left_join(start_transport,start_inventory , by = c("swinenet_id"))
end_activity_dates      <-     left_join(end_transport,end_inventory,  by = c("swinenet_id"))

start_activity_dates$best_start_date   <-    start_activity_dates$start.x  
end_activity_dates$best_end_date   <-    end_activity_dates$end.x
# Some issue in the date format needs to be fixed so that the earlier start and later dates are picked
#start_activity_dates$best_date[start_activity_dates$start.y < start_activity_dates$start.x] <- start_activity_dates$start.y


activity_dates       <-    left_join(start_activity_dates[,c("swinenet_id","best_start_date")], end_activity_dates[,c("swinenet_id","best_end_date")], by = c("swinenet_id"))


#  adding the wild boar units as always active
load(paste(relative_path_to_processed_data ,"wildboar_units.Rdata",sep =""))

wildboar_units$swinenet_id        <-   90000 + wildboar_units$gemeinde_BFS
wildboar_units$best_start_date         <-     min(activity_dates$best_start_date)
wildboar_units$best_end_date         <-     max(activity_dates$best_end_date)

activity_dates          <-     rbind(activity_dates, wildboar_units[, c("swinenet_id","best_start_date" , "best_end_date")  ])


save(activity_dates, file =paste(relative_path_to_processed_data,"activity_dates_final.RData",sep ="" ))
save(pop_vertex, file =paste(relative_path_to_processed_data,"pop_vertex_final.RData",sep ="" ))
save(net_pop_change_vertex_year, file =paste(relative_path_to_processed_data,"pop_change_vertex_year_final.RData",sep ="" ))
save(net_pop_change_vertex, file =paste(relative_path_to_processed_data,"pop_change_vertex_final.RData",sep ="" ))





