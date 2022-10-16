## Some rough plotting of the simulations to give idea of variation across simulations

par(mfrow=c(1,1))






#* fix to make cummulative instead of instantaneous prevalence graphs ####
cummutalive_graphs = 1
if(cummutalive_graphs == 1){
  
  sim_output$cummul_farm <- 0
  sim_output$cummul_pig <- 0
  sim_output$cummul_wbunit<- 0
  sim_output$cummul_wb<- 0
  
  for(rep in 1:max(sim_output$replication)){
    sim_output$cummul_farm[sim_output$t_step == 1 & sim_output$replication == rep ] <-  sim_output$prevalence_farm[sim_output$t_step == 1 & sim_output$replication == rep]
    sim_output$cummul_pig[sim_output$t_step == 1 & sim_output$replication == rep ] <-  sim_output$prevalence_pigs[sim_output$t_step == 1 & sim_output$replication == rep]
    sim_output$cummul_wbunit[sim_output$t_step == 1 & sim_output$replication == rep ] <-  sim_output$prevalence_wb_units[sim_output$t_step == 1 & sim_output$replication == rep]
    sim_output$cummul_wb[sim_output$t_step == 1 & sim_output$replication == rep ] <-  sim_output$prevalence_wb[sim_output$t_step == 1 & sim_output$replication == rep]
    
    for(t in 2:max(sim_output$t_step)){
      sim_output$cummul_farm[sim_output$t_step == t & sim_output$replication == rep] <- sim_output$cummul_farm[sim_output$t_step == t-1 & sim_output$replication == rep] +
        sim_output$incidence_farm[sim_output$t_step == t & sim_output$replication == rep]
      sim_output$cummul_pig[sim_output$t_step == t & sim_output$replication == rep] <- sim_output$cummul_pig[sim_output$t_step == t-1 & sim_output$replication == rep] +
        sim_output$incidence_pigs[sim_output$t_step == t & sim_output$replication == rep]
      
      sim_output$cummul_wbunit[sim_output$t_step == t & sim_output$replication == rep] <- sim_output$cummul_wbunit[sim_output$t_step == t-1 & sim_output$replication == rep] +
        sim_output$prevalence_wb_units[sim_output$t_step == t & sim_output$replication == rep]
      sim_output$cummul_wb[sim_output$t_step == t & sim_output$replication == rep] <- sim_output$cummul_wb[sim_output$t_step == t-1 & sim_output$replication == rep] +
        sim_output$incidence_wb[sim_output$t_step == t & sim_output$replication == rep]
    }
  }
  
  check = sim_output[, c("t_step", "replication", "incidence_farm", "cummul_farm", "prevalence_farm")]
  
  sim_output$prevalence_farm <- sim_output$cummul_farm
  sim_output$prevalence_wb_units <- sim_output$cummul_wbunit
  sim_output$prevalence_pigs <- sim_output$cummul_pig
  sim_output$prevalence_wb <- sim_output$cummul_wb
  
}



## make the plot for prevalence pigs ####
output_all_plot <- sim_output[,c("t_step" ,"replication" , "prevalence_pigs", "incidence_pigs" , "prevalence_wb", "incidence_wb")]
names(output_all_plot)  <- c("t_step" ,"replication" , "Total number of infected pigs","Number of infected pigs at time step","Total number of infected wildboar","Number of infected wildboar at time step" )
df=melt(output_all_plot,id.vars = c("t_step", "replication"))

#output_dat$t_step <- as.numeric(output_dat$t_step)
tspag = ggplot(df, aes(x=t_step, y=value)) + 
  geom_line() + guides(colour = "none") + xlab("Time steps") 
spag = tspag + aes(colour = factor(replication)) + geom_smooth(se=FALSE, colour="black", size=1) 

current_plot <- spag + facet_wrap(~ variable, scales = "free")


png(paste(relative_path_to_output,"/",scenario,"/","preval_incid_pigs_wildboar.png",sep=""))
print(current_plot)
dev.off()

## make the plot for prevalence farms ####
output_all_plot <- sim_output[,c("t_step" ,"replication" , "prevalence_farm", "incidence_farm", "prevalence_wb_units" ,  "incidence_wb_units" )]
names(output_all_plot)  <- c("t_step" ,"replication" , "Total number of infected farms","Number of infected farms at time step",
                             "Total number of infected wildboar units","Number of infected wildboar units at time step" )

df=melt(output_all_plot,id.vars = c("t_step", "replication"))

#output_dat$t_step <- as.numeric(output_dat$t_step)
tspag = ggplot(df, aes(x=t_step, y=value)) + 
  geom_line() + guides(colour = "none") + xlab("Time steps") 
spag = tspag + aes(colour = factor(replication)) + geom_smooth(se=FALSE, colour="black", size=1)
current_plot<- spag + facet_wrap(~ variable, scales = "free")


png(paste(relative_path_to_output,"/",scenario,"/","preval_incid_farm_wildboar.png",sep=""))
print(current_plot)
dev.off()





## make the plot for side to side with prevalence farms and pigs####
output_all_plot <- sim_output[,c("t_step" ,"replication" , "prevalence_pigs", "prevalence_farm" , "prevalence_wb", "prevalence_wb_units")]
names(output_all_plot) <- c("t_step" ,"replication" , "Total number of infected pigs", "Total number infected of farms" , "Total number of infected wildboars", "Total number infected wildboar units")
df=melt(output_all_plot,id.vars = c("t_step", "replication"))



#output_dat$t_step <- as.numeric(output_dat$t_step)
tspag = ggplot(df, aes(x=t_step, y=value)) + 
  geom_line() + guides(colour = "none") + xlab("Time steps") 
spag = tspag + aes(colour = factor(replication)) + geom_smooth(se=FALSE, colour="black", size=1)

current_plot <- spag + facet_wrap(~ variable, scales = "free")


png(paste(relative_path_to_output,"/",scenario,"/","prevalence_farm_pigs_wildboar_.png",sep=""))
print(current_plot)
dev.off()





## make the plot for detections side to side with prevalence pigs/wb ####
output_all_plot <- sim_output[,c("t_step" ,"replication" , "detections","prevalence_farm","prevalence_pigs")]
names(output_all_plot)  <- c("t_step" ,"replication" , "Detections","Total number of infected farms","Total number of infected pigs")
df=melt(output_all_plot,id.vars = c("t_step", "replication"))

#output_dat$t_step <- as.numeric(output_dat$t_step)
tspag = ggplot(df, aes(x=t_step, y=value)) + 
  geom_line() + guides(colour = "none") + xlab("Time steps") 
spag = tspag + aes(colour = factor(replication)) + geom_smooth(se=FALSE, colour="black", size=1)

current_plot1 <- spag + facet_wrap(~ variable, scales = "free")

png(paste(relative_path_to_output,"/",scenario,"/","detections_prevalence_pigs.png",sep=""))
print(current_plot1)
dev.off()



## make the detection time summaries ####
first_time <- function(x)(first(which(x >0)))
detection_stats <- sim_output %>%  group_by(replication)  %>%  summarize(first_dedect = first_time(detections))

detection_stats  <-detection_stats[  !is.na(detection_stats$first_dedect) ,]

# detection_stats[  is.na(detection_stats$first_dedect), ]  <- max(sim_output$t_step)

detection_stats <- left_join(detection_stats,sim_output[,c("t_step","prevalence_farm","prevalence_wb_units" , "prevalence_pigs"  ,   "prevalence_wb"  , "replication" )], 
                             by = c("replication"="replication" ,"first_dedect"="t_step"))



if (!is.na(sum(detection_stats$first_dedect))){
  png(paste(relative_path_to_output,"/",scenario,"/","detection_times.png",sep=""))
  hist_plot <- ggplot(detection_stats, aes(x=first_dedect))+
    geom_histogram(color="darkblue", fill="lightblue", binwidth = 1) + xlab("Time at first detection") +
    geom_vline(aes(xintercept=mean(first_dedect)),
               color="blue", size=1) +
    geom_vline(aes(xintercept=median(first_dedect)),
               color="blue", linetype="dashed",size=1) +
    xlim(0, max(detection_stats$first_dedect)+2)
  print(hist_plot)
  dev.off()
  
  png(paste(relative_path_to_output,"/",scenario,"/","detection_sizes_pigs.png",sep=""))
  hist_plot <- ggplot(detection_stats, aes(x=prevalence_pigs))+
    geom_histogram(color="brown", fill="#E7B800") + xlab("Pigs infected at first detection (mean solid line, median dashed)") +
    geom_vline(aes(xintercept=mean(prevalence_pigs)),
               color="blue", size=1) +
    geom_vline(aes(xintercept=median(prevalence_pigs)),
               color="blue", linetype="dashed",size=1) +
    xlim(0, max(detection_stats$prevalence_pigs)+2)
  print(hist_plot)
  dev.off()
  
  png(paste(relative_path_to_output,"/",scenario,"/","detection_sizes_farms.png",sep=""))
  hist_plot <- ggplot(detection_stats, aes(x=prevalence_farm))+
    geom_histogram(color="#C4961A", fill="#FFDB6D") + xlab("Farms infected at first detection (mean solid line, median dashed)") +
    geom_vline(aes(xintercept=mean(prevalence_farm)),
               color="blue", size=1) +
    geom_vline(aes(xintercept=median(prevalence_farm)),
               color="blue", linetype="dashed",size=1) +
    xlim(0, max(detection_stats$prevalence_farm)+2)
  print(hist_plot)
  dev.off()
  
  png(paste(relative_path_to_output,"/",scenario,"/","detection_sizes_wildboars.png",sep=""))
  hist_plot <- ggplot(detection_stats, aes(x=prevalence_wb))+
    geom_histogram(color="#C4961A", fill="#FFDB6D") + xlab("Wildboars infected at first detection (mean solid line, median dashed)") +
    geom_vline(aes(xintercept=mean(prevalence_wb)),
               color="blue", size=1) +
    geom_vline(aes(xintercept=median(prevalence_wb)),
               color="blue", linetype="dashed",size=1) +
    xlim(0, max(detection_stats$prevalence_wb)+2)
  print(hist_plot)
  dev.off()
  
  png(paste(relative_path_to_output,"/",scenario,"/","time_detection_size_outbreak.png",sep=""))
  
  current_plot <-  ggplot(detection_stats, aes(x=first_dedect, y=prevalence_farm)) + 
    geom_point(aes(size=prevalence_pigs), alpha=0.5)+
    xlab("Time at first detection") +
    ylab("Number of infected farms at first detection")+
    labs(size="Number of infected pigs")
  
  print(current_plot)
  dev.off()
  
  
  
  
  png(paste(relative_path_to_output,"/",scenario,"/","pigs_farm_detection_size_outbreak.png",sep=""))
  
  current_plot <-  ggplot(detection_stats, aes(x=prevalence_pigs, y=prevalence_farm)) + 
    geom_point(aes(size=first_dedect), alpha=0.5)+
    xlab("Number of infected pigs") +
    ylab("Number of infected farms at first detection")+
    labs(size="Time at first detection")+
    ylim(0, max(detection_stats$prevalence_farm)+2)
  
  print(current_plot)
  dev.off()
  
}





# making plot of number of additional farms infected if delay between detection and response
first_time <- function(x)(first(which(x >0)))
detection_stats <- sim_output %>%  group_by(replication)  %>%  summarize(first_dedect = first_time(detections))

detection_stats <- detection_stats[!is.na(detection_stats$first_dedect),]

sim_output_augment <- left_join(sim_output, detection_stats, by =c("replication"= "replication"))

sim_output_augment <- sim_output_augment[!is.na(sim_output_augment$first_dedect),]

cost_delay <- data.frame("replication" = as.numeric(),
                         "prevalence_farm"= as.numeric(),
                         "prevalence_pigs"= as.numeric(),
                         "delay" = as.integer())
for(delay in 0:20){
  size_at_response <- sim_output_augment[sim_output_augment$t_step == sim_output_augment$first_dedect+ delay ,]
  cost_delay_temp <- size_at_response[,c("replication", "prevalence_farm", "prevalence_pigs")]
  cost_delay_temp$delay <- delay
  cost_delay <- rbind(cost_delay, cost_delay_temp)
  
}


cost_delay_average <- cost_delay %>%  group_by(delay)  %>%  summarize(prevalence_farm = mean(prevalence_farm),prevalence_pigs = mean(prevalence_pigs))

png(paste(relative_path_to_output,"/",scenario,"/","cost_delay_farms.png",sep=""))
current_plot <-  ggplot(cost_delay_average, aes(x=delay, y=prevalence_farm)) + 
  geom_line() + 
  xlab("Delay between on-farm detection and response (days)") +
  ylab("Average number of infected farms at time of response")


print(current_plot)
dev.off()









## detach reshape2 to avoid conflicts with other packages ####
detach("package:reshape2")








