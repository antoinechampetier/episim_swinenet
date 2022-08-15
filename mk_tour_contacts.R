#######################################
#title: "Tour Preprocessing"
#author : Kathleen Moriarty modified by Antoine Champetier
#date : 28.10.2021
#output: functions to be applie on tour lists with tour_id and timing of arrivals at source and dest for sequencing.
#######################################


mk_contact_network_tour<- function(tour_edges){

  tour_edges <- tour_edges %>%   group_by(date, tour_id) %>%   mutate(seq_nr = order(time_at_source))
  tour_edges <-  as_tibble(tour_edges)
  tour_edges$date   <-   as.character(tour_edges$date)
  tmp_tour_sorted <- tour_edges %>%     arrange(tour_id, seq_nr)
    
  tmp_edge_list <- tibble(ID_vertex_source=as.integer(), 
                          ID_vertex_dest=as.integer(),
                          date=as.character(),
                          contact_type=as.character())

     
#loop through the tour_ids
for(i in 1:(nrow(tmp_tour_sorted)-1)){
  #loop through the records within each tour_id
  for(j in 1:(nrow(tmp_tour_sorted))){
    k=i+j
    if(tmp_tour_sorted$tour_id[i] == tmp_tour_sorted$tour_id[k]){ #if they are in the same tour
      if(tmp_tour_sorted$ID_vertex_source[i] != tmp_tour_sorted$ID_vertex_source[(k)]){ #if there are multiple pick-ups for the same farm - ignore those
        if(tmp_tour_sorted$time_at_source[i] > tmp_tour_sorted$time_at_source[k]){ 
          tmp_edge_list <- tmp_edge_list %>% add_row(ID_vertex_source=tmp_tour_sorted$ID_vertex_source[i],  
                                                     ID_vertex_dest=tmp_tour_sorted$ID_vertex_source[k], 
                                                     date=tmp_tour_sorted$date[k],
                                                     contact_type="t") %>%
            add_row(ID_vertex_source=tmp_tour_sorted$ID_vertex_source[i], 
                    ID_vertex_dest=tmp_tour_sorted$ID_vertex_dest[k], 
                    date=tmp_tour_sorted$date[k],
                    contact_type="p2p")
        }else if(tmp_tour_sorted$time_at_source[i] == tmp_tour_sorted$time_at_source[k]){ 
          tmp_edge_list <- tmp_edge_list %>% add_row(ID_vertex_source=tmp_tour_sorted$ID_vertex_source[i],  
                                                     ID_vertex_dest=tmp_tour_sorted$ID_vertex_source[k], 
                                                     date=tmp_tour_sorted$date[k],
                                                     contact_type="t")
        }else if(tmp_tour_sorted$time_at_source[i] > tmp_tour_sorted$time_at_source[k]){ 
          tmp_edge_list <- tmp_edge_list %>% add_row(ID_vertex_source=tmp_tour_sorted$ID_vertex_source[i], 
                                                     ID_vertex_dest=tmp_tour_sorted$ID_vertex_source[k], 
                                                     date=tmp_tour_sorted$date[k],
                                                     contact_type="t") %>%
            add_row(ID_vertex_source=tmp_tour_sorted$ID_vertex_source[i], 
                    ID_vertex_dest=tmp_tour_sorted$ID_vertex_dest[k], 
                    date=tmp_tour_sorted$date[k],
                    contact_type="p2p") %>%
            add_row(ID_vertex_source=tmp_tour_sorted$ID_vertex_source[k],
                    ID_vertex_dest=tmp_tour_sorted$ID_vertex_dest[i], 
                    date=tmp_tour_sorted$date[k],
                    contact_type="p2p") %>%
            add_row(ID_vertex_source=tmp_tour_sorted$ID_vertex_dest[i], 
                    ID_vertex_dest=tmp_tour_sorted$ID_vertex_dest[k], 
                    date=tmp_tour_sorted$date[k],
                    contact_type="t")
        }else if(tmp_tour_sorted$time_at_source[i] < tmp_tour_sorted$time_at_source[k]){
          tmp_edge_list <- tmp_edge_list %>% add_row(ID_vertex_source=tmp_tour_sorted$ID_vertex_source[i], 
                                                     ID_vertex_dest=tmp_tour_sorted$ID_vertex_source[k], 
                                                     date=tmp_tour_sorted$date[k],
                                                     contact_type="t") %>%
            add_row(ID_vertex_source=tmp_tour_sorted$ID_vertex_source[i], 
                    ID_vertex_dest=tmp_tour_sorted$ID_vertex_dest[k], 
                    date=tmp_tour_sorted$date[k],
                    contact_type="f") %>%
            add_row(ID_vertex_source=tmp_tour_sorted$ID_vertex_dest[i], 
                    ID_vertex_dest=tmp_tour_sorted$ID_vertex_source[k], 
                    date=tmp_tour_sorted$date[k],
                    contact_type="t") %>%
            add_row(ID_vertex_source=tmp_tour_sorted$ID_vertex_dest[i], 
                    ID_vertex_dest=tmp_tour_sorted$ID_vertex_dest[k], 
                    date=tmp_tour_sorted$date[k],
                    contact_type="t")
        }
      }
    }else{break}#break from sequence for loop back to tour_id for loop
  }
}

  tmp_edge_list = unique(tmp_edge_list)
  tmp_edge_list = tmp_edge_list[tmp_edge_list$ID_vertex_source != tmp_edge_list$ID_vertex_dest, ]


return(tmp_edge_list)
}


mk_contact_network_vet<- function(tour_edges){
  tour_edges$tour_id  <- paste(as.character(tour_edges$tour_id),as.character(tour_edges$date), sep = "")
  
  tour_edges <- tour_edges %>%   group_by(date, tour_id) %>%   mutate(seq_nr = order(time_at_source))
  tour_edges <-  as_tibble(tour_edges)
  tour_edges$date   <-   as.character(tour_edges$date)
  tmp_tour_sorted <- tour_edges %>%     arrange(tour_id, seq_nr)
  
  
  tmp_edge_list <- tibble(ID_vertex_source=as.integer(), 
                          ID_vertex_dest=as.integer(),
                          date=as.character(),
                          contact_type=as.character())
  i=1
  j=1  
  #loop through the tour_ids
  for(i in 1:(nrow(tmp_tour_sorted)-1)){
    #loop through the records within each tour_id
    for(j in 1:(nrow(tmp_tour_sorted)-i)){
      k=i+j
      if(tmp_tour_sorted$tour_id[i] == tmp_tour_sorted$tour_id[k]){ #if they are in the same tour
        if(tmp_tour_sorted$ID_vertex_source[i] != tmp_tour_sorted$ID_vertex_source[(k)]){ #if there are multiple pick-ups for the same farm - ignore those
          if(tmp_tour_sorted$time_at_source[i] > tmp_tour_sorted$time_at_source[k]){ 
            tmp_edge_list <- tmp_edge_list %>% add_row(ID_vertex_source=tmp_tour_sorted$ID_vertex_source[i],  
                                                       ID_vertex_dest=tmp_tour_sorted$ID_vertex_source[k], 
                                                       date=tmp_tour_sorted$date[k],
                                                       contact_type="v") 
          }else if(tmp_tour_sorted$time_at_source[i] == tmp_tour_sorted$time_at_source[k]){ 
            tmp_edge_list <- tmp_edge_list %>% add_row(ID_vertex_source=tmp_tour_sorted$ID_vertex_source[i],  
                                                       ID_vertex_dest=tmp_tour_sorted$ID_vertex_source[k], 
                                                       date=tmp_tour_sorted$date[k],
                                                       contact_type="v")
          }else if(tmp_tour_sorted$time_at_source[i] > tmp_tour_sorted$time_at_source[k]){ 
            tmp_edge_list <- tmp_edge_list %>% add_row(ID_vertex_source=tmp_tour_sorted$ID_vertex_source[i], 
                                                       ID_vertex_dest=tmp_tour_sorted$ID_vertex_source[k], 
                                                       date=tmp_tour_sorted$date[k],
                                                       contact_type="v") %>%
              add_row(ID_vertex_source=tmp_tour_sorted$ID_vertex_dest[i], 
                      ID_vertex_dest=tmp_tour_sorted$ID_vertex_dest[k], 
                      date=tmp_tour_sorted$date[k],
                      contact_type="v")
          }else if(tmp_tour_sorted$time_at_source[i] < tmp_tour_sorted$time_at_source[k]){
            tmp_edge_list <- tmp_edge_list %>% add_row(ID_vertex_source=tmp_tour_sorted$ID_vertex_source[i], 
                                                       ID_vertex_dest=tmp_tour_sorted$ID_vertex_source[k], 
                                                       date=tmp_tour_sorted$date[k],
                                                       contact_type="v") %>%
              add_row(ID_vertex_source=tmp_tour_sorted$ID_vertex_dest[i], 
                      ID_vertex_dest=tmp_tour_sorted$ID_vertex_source[k], 
                      date=tmp_tour_sorted$date[k],
                      contact_type="v") %>%
              add_row(ID_vertex_source=tmp_tour_sorted$ID_vertex_dest[i], 
                      ID_vertex_dest=tmp_tour_sorted$ID_vertex_dest[k], 
                      date=tmp_tour_sorted$date[k],
                      contact_type="v")
          }
        }
      }else{break}#break from sequence for loop back to tour_id for loop
    }
  }
  
  tmp_edge_list = unique(tmp_edge_list)
  tmp_edge_list = tmp_edge_list[tmp_edge_list$ID_vertex_source != tmp_edge_list$ID_vertex_dest, ]
  
  
  return(tmp_edge_list)
}






