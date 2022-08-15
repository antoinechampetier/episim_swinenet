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
df=melt(output_all_plot,id.vars = c("t_step", "replication"))

#output_dat$t_step <- as.numeric(output_dat$t_step)
tspag = ggplot(df, aes(x=t_step, y=value)) + 
  geom_line() + guides(colour = "none") + xlab("Time steps") 
spag = tspag + aes(colour = factor(replication)) + geom_smooth(se=FALSE, colour="black", size=1) 

current_plot <- spag + facet_wrap(~ variable, scales = "free")


png(paste(relative_path_to_output,"/",scenario,"/","pigs_simulation.png",sep=""))
print(current_plot)
dev.off()

## make the plot for prevalence farms ####
output_all_plot <- sim_output[,c("t_step" ,"replication" , "prevalence_farm", "prevalence_wb_units" , "incidence_farm",  "incidence_wb_units" )]
df=melt(output_all_plot,id.vars = c("t_step", "replication"))

#output_dat$t_step <- as.numeric(output_dat$t_step)
tspag = ggplot(df, aes(x=t_step, y=value)) + 
  geom_line() + guides(colour = "none") + xlab("Time steps") 
spag = tspag + aes(colour = factor(replication)) + geom_smooth(se=FALSE, colour="black", size=1)
current_plot<- spag + facet_wrap(~ variable, scales = "free")


png(paste(relative_path_to_output,"/",scenario,"/","farm_simulation.png",sep=""))
print(current_plot)
dev.off()


## make the plot for detections side to side with prevalence farms ####
output_all_plot <- sim_output[,c("t_step" ,"replication" , "prevalence_pigs", "incidence_pigs" , "prevalence_wb", "incidence_wb")]
df=melt(output_all_plot,id.vars = c("t_step", "replication"))

#output_dat$t_step <- as.numeric(output_dat$t_step)
tspag = ggplot(df, aes(x=t_step, y=value)) + 
  geom_line() + guides(colour = "none") + xlab("Time steps") 
spag = tspag + aes(colour = factor(replication)) + geom_smooth(se=FALSE, colour="black", size=1)

current_plot <- spag + facet_wrap(~ variable, scales = "free")


png(paste(relative_path_to_output,"/",scenario,"/","pigs_simulation.png",sep=""))
print(current_plot)
dev.off()



## make the plot for side to side with prevalence farms and pigs####
output_all_plot <- sim_output[,c("t_step" ,"replication" , "prevalence_pigs", "prevalence_farm" , "prevalence_wb", "prevalence_wb_units")]
names(output_all_plot) <- c("t_step" ,"replication" , "Number infected pigs", "Number infected farms" , "Number infected wildboars", "Number infected wb units")
df=melt(output_all_plot,id.vars = c("t_step", "replication"))



#output_dat$t_step <- as.numeric(output_dat$t_step)
tspag = ggplot(df, aes(x=t_step, y=value)) + 
  geom_line() + guides(colour = "none") + xlab("Time steps") 
spag = tspag + aes(colour = factor(replication)) + geom_smooth(se=FALSE, colour="black", size=1)

current_plot <- spag + facet_wrap(~ variable, scales = "free")


png(paste(relative_path_to_output,"/",scenario,"/","farm_pigs_prevalence.png",sep=""))
print(current_plot)
dev.off()





# ## make the plot for side to side with prevalence farms and pigs####
# output_all_plot <- sim_output[,c("t_step" ,"replication" , "prevalence_pigs", "prevalence_farm" , "prevalence_wb", "prevalence_wb_units")]
# df=melt(output_all_plot,id.vars = c("t_step", "replication"))
# 
# r=1
# df2=df[df$replication==r,]
# #output_dat$t_step <- as.numeric(output_dat$t_step)
# tspag = ggplot(df2, aes(x=t_step, y=value)) + 
#   geom_line() + guides(colour = "none") + xlab("Time steps") 
# spag = tspag + aes(colour = factor(replication))  #+geom_smooth(se=FALSE, colour="black", size=1)
# current_plot <- spag + facet_wrap(~ variable, scales = "free")
# 
# print(current_plot)
# r=r+1




## make the plot for detections side to side with prevalence pigs/wb ####
output_all_plot <- sim_output[,c("t_step" ,"replication" , "detections","prevalence_pigs","prevalence_farm")]
names(output_all_plot)  <- c("t_step" ,"replication" , "Detections","Number of infected pigs","Number of infected farms")
df=melt(output_all_plot,id.vars = c("t_step", "replication"))

#output_dat$t_step <- as.numeric(output_dat$t_step)
tspag = ggplot(df, aes(x=t_step, y=value)) + 
  geom_line() + guides(colour = "none") + xlab("Time steps") 
spag = tspag + aes(colour = factor(replication)) + geom_smooth(se=FALSE, colour="black", size=1)

current_plot1 <- spag + facet_wrap(~ variable, scales = "free")

png(paste(relative_path_to_output,"/",scenario,"/","detections_simulation.png",sep=""))
print(current_plot1)
dev.off()



## make the detection time summaries ####
first_time <- function(x)(first(which(x >0)))
detection_stats <- sim_output %>%  group_by(replication)  %>%  summarize(first_dedect = first_time(detections))

detection_stats <- left_join(detection_stats,sim_output[,c("t_step","prevalence_farm","prevalence_wb_units" , "prevalence_pigs"  ,   "prevalence_wb"  , "replication" )], 
                             by = c("replication"="replication" ,"first_dedect"="t_step"))

png(paste(relative_path_to_output,"/",scenario,"/","hist_detection_times.png",sep=""))
hist_plot <- hist(detection_stats$first_dedect,breaks = simulation_steps, xlab= "time first detection",xaxp=c(0,simulation_steps,simulation_steps))
dev.off()



png(paste(relative_path_to_output,"/",scenario,"/","box_plot_detection_times.png",sep=""))
boxplot  <- ggplot(detection_stats, aes(y=first_dedect)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  theme(axis.text.x=element_blank()) +
  ylab("time at first detection")
print(boxplot)
dev.off()



## make size at first detection summaries ####

detection_size <- data.frame(sim_output$prevalence_pigs[detection_stats$first_dedect], detection_stats$replication)
names(detection_size) = c("size_at_detection","replication")
png(paste(relative_path_to_output,"/",scenario,"/","hist_size_first_detection.png",sep=""))
hist(detection_size$size_at_detection, breaks = 100)
dev.off()


png(paste(relative_path_to_output,"/",scenario,"/","box_plot_detection_size.png",sep=""))
boxplot  <- ggplot(detection_size, aes(y=size_at_detection)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  theme(axis.text.x=element_blank()) +
  ylab("time at first detection")
print(boxplot)
dev.off()


png(paste(relative_path_to_output,"/",scenario,"/","plot_detection_size_time.png",sep=""))
plot(detection_size$size_at_detection,detection_stats$first_dedect)
dev.off()



df=melt(detection_stats,id.vars = c("replication"))
hist_plot <- ggplot(df,aes(value)) + geom_histogram() + facet_wrap(~variable, scales = "free")

png(paste(relative_path_to_output,"/",scenario,"/","hist_detection_times_details.png",sep=""))
print(hist_plot)
dev.off()






## trying better formats for trajectory plots ####
output_all_plot <- sim_output[,c("t_step" ,"replication","prevalence_pigs","prevalence_farm")]

df=melt(output_all_plot,id.vars = c("t_step", "replication"))

# ensemble average
ensemble_av <- df %>% group_by(t_step) %>% summarize(Mean = mean(value))



current_plot <- ggplot(df,aes(t_step,value,group=replication))+
  geom_line(alpha=0.2) + facet_wrap(~variable, scales = "free") +
  xlab("Days")


png(paste(relative_path_to_output,"/",scenario,"/","prevalence_pigs_farms_grey.png",sep=""))
print(current_plot)
dev.off()



## detach reshape2 to avoid conflicts with other packages ####
detach("package:reshape2")











