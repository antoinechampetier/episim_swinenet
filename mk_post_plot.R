#######################################
#' @title Plot maker from simulated sim_out
#' @author : Antoine Champetier
#' @date : 18.08.2022
#' @description: will take each scenario output and make the plots for it
#######################################




## SET DIRECTORIES AND PACKAGE-LIBRARIES####
rm(list = ls())
Sys.setenv(LANG = "en")

getwd()
#setwd("./episim_swinenet")
#setwd("~/swinenet_episim/episim_swinenet")
relative_path_to_output = "../simulation_outputs/"
relative_path_to_scenarios = "../simulation_inputs/"
relative_path_to_data = "../epi_data/"

suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(tidyverse))
suppressMessages(library(ggplot2))
suppressMessages(library(matrixStats))
suppressMessages(library(lubridate))

for(scenario in list.files(relative_path_to_output)){
  print(scenario)
  load(paste0(relative_path_to_output,scenario,"/output_simulation_set.RData"))
  simulation_steps = max(sim_output$t_step)
  source("mk_plot_simulation_set.R")
  
} 

  