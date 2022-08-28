## Some rough plotting of the simulations to give idea of variation across simulations
library(reshape2)
par(mfrow=c(1,1))


# #this is to plot the trajectories of just the latest simulation
# output_run = output_run[,c("t_step",  "prevalence_farm"  ,  "prevalence_wb_units", "prevalence_pigs"  ,   "prevalence_wb"  )]
# names(output_run)= c("t_step" ,"Number of infected farms" , "Number of infected wildboar units", "Number of infected pigs" , "Number of infected wild boars")
# df=melt(output_run,id.vars = c("t_step"))
# tspag = ggplot(df, aes(x=t_step, y=value)) + 
#   geom_line() + guides(colour = "none") + xlab("Time steps") 
# 
# current_plot<- tspag + facet_wrap(~ variable, scales = "free")
# print(current_plot)


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
output_all_plot <- sim_output[,c("t_step" ,"replication" , "prevalence_farm", "prevalence_wb_units" , "incidence_farm",  "incidence_wb_units" )]
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

detection_stats <- left_join(detection_stats,sim_output[,c("t_step","prevalence_farm","prevalence_wb_units" , "prevalence_pigs"  ,   "prevalence_wb"  , "replication" )], 
                             by = c("replication"="replication" ,"first_dedect"="t_step"))

if (!is.na(sum(detection_stats$first_dedect))){
  png(paste(relative_path_to_output,"/",scenario,"/","detection_times.png",sep=""))
  hist_plot <- ggplot(detection_stats, aes(x=first_dedect))+
    geom_histogram(color="darkblue", fill="lightblue") + xlab("Time at first detection") +
    geom_vline(aes(xintercept=mean(first_dedect)),
               color="blue", linetype="dashed", size=1) +
    geom_vline(aes(xintercept=median(first_dedect)),
               color="blue", size=1) +
    xlim(0, max(detection_stats$first_dedect))
  print(hist_plot)
  dev.off()
  
  png(paste(relative_path_to_output,"/",scenario,"/","detection_sizes_pigs.png",sep=""))
  hist_plot <- ggplot(detection_stats, aes(x=prevalence_pigs))+
    geom_histogram(color="brown", fill="#E7B800") + xlab("Pigs infected at first detection (mean solid line, median dashed)") +
    geom_vline(aes(xintercept=mean(prevalence_pigs)),
               color="blue", size=1) +
    geom_vline(aes(xintercept=median(prevalence_pigs)),
               color="blue", linetype="dashed",size=1) +
    xlim(0, max(detection_stats$prevalence_pigs))
  print(hist_plot)
  dev.off()
  
  png(paste(relative_path_to_output,"/",scenario,"/","detection_sizes_farms.png",sep=""))
  hist_plot <- ggplot(detection_stats, aes(x=prevalence_farm))+
    geom_histogram(color="#C4961A", fill="#FFDB6D") + xlab("Farms infected at first detection (mean solid line, median dashed)") +
    geom_vline(aes(xintercept=mean(prevalence_farm)),
               color="blue", size=1) +
    geom_vline(aes(xintercept=median(prevalence_farm)),
               color="blue", linetype="dashed",size=1) +
    xlim(0, max(detection_stats$prevalence_farm))
  print(hist_plot)
  dev.off()
  
  png(paste(relative_path_to_output,"/",scenario,"/","detection_sizes_wildboars.png",sep=""))
  hist_plot <- ggplot(detection_stats, aes(x=prevalence_wb))+
    geom_histogram(color="#C4961A", fill="#FFDB6D") + xlab("Wildboars infected at first detection (mean solid line, median dashed)") +
    geom_vline(aes(xintercept=mean(prevalence_wb)),
               color="blue", size=1) +
    geom_vline(aes(xintercept=median(prevalence_wb)),
               color="blue", linetype="dashed",size=1) +
    xlim(0, max(detection_stats$prevalence_wb))
  print(hist_plot)
  dev.off()
  
  
  
}



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
  labs(size="Time at first detection")

print(current_plot)
dev.off()



# ensemble average
#ensemble_av <- df %>% group_by(t_step) %>% summarize(Mean = mean(value))


## detach reshape2 to avoid conflicts with other packages ####
detach("package:reshape2")





#boxplot  <- ggplot(detection_stats, aes(x=first_dedect)) + 
#  geom_boxplot(fill="slateblue", alpha=0.2, width = .5) + 
#  xlab("Time at first detection") +
#  xlim(0, max(detection_stats$first_dedect))
#library(ggpubr)

#ggarrange(hist_plot, boxplot, ncol = 1, nrow = 2)






