# Run model and accumulate outcomes (deterministic)
# This module allows for city selection, loads list of intervention combinations, loads ordinary differential equation functions,
# analysis scenario (deterministic or sensitivity analysis), and loads input parameters and comparators.
# It also provides code for running parallel estimations.

#############################################################################
# 1. SET directory and workspace
#############################################################################

rm(list=ls())
library(rstudioapi)
library(LEMHIVpack)
library(rlist)
library(foreach)
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("01_Setup/CascadeCEA-Interventions-1-LoadBaselineWorkspace.R")

# SELECT city ##
#* CITY <- select.list(all.cities, multiple = FALSE, title = 'Select city', graphics = FALSE)

#************************* Setting city as LA for Part 1
ww <- 3; 
CITY <- all.cities[[ww]] # Otherwise you can set city by its index

## LOAD list of all combinations, interventions indexed by number in each combination
# if the list does not exist, source("CascadeCEA-Combination-0-Setup-combination.list.R")
combination.list <- readRDS("Combination/Combination.list.rds")

#************************* Storing the 10 frontier combination indices
combination.frontier.list <- as.vector(readRDS(paste0("Combination/ProductionFunction-Frontier-", CITY, ".rds"))$frontier)
combination.frontier.list

## LOAD ODE function
#source("ModelCoreModules/CascadeCEA-Model-0-Function-ode_model-Combination.R") # Function automatically loaded with LEMHIVpack

## LOAD analysis scenario
case = "DM"  # DM for deterministic, SA for sensitivity analysis

## LOAD all input parameters and comparators
source("01_Setup/CascadeCEA-Interventions-1-LoadParameterWorkspace-Combination.R")

#* total.comb <- length(combination.list)
#************************* Total combinations for just the frontier
total.comb <- length(combination.frontier.list)
total.comb

tic("Model run")

outcome.comb.mx <- matrix(0, nrow = total.comb, ncol = 44)    ##Initialize combination outcome matrix (to save results)
outcome.comb.mx <- foreach(cc=1:total.comb, .combine=rbind, .export = export.int.model.names
) %dopar% {
  comb.eva.func(input.parameters = all.params, current.int = interventions[combination.list[[combination.frontier.list[cc]]]])
}

future:::ClusterRegistry("stop")   ##Stop parallel estimations and free cores

colnames(outcome.comb.mx)        <- rep("FILL", 44)
colnames(outcome.comb.mx)[1:6]   <- c("Infections.total-20Y", "SuscPY-over20Y", "Infections.total-10Y", "SuscPY-over10Y", "Infections.total-5Y", "SuscPY-over5Y")
colnames(outcome.comb.mx)[7:32]  <- paste0("Year", c(2015:2040))
colnames(outcome.comb.mx)[33:44] <- c("QALYs.sum", "costs.total.sum", "costs.hru.sum", "costs.art.sum", "costs.art.ini.sum",
                                      "costs.oat.sum", "costs.prep.sum", "costs.prep.tests.sum", "costs.test.sum", "int.costs.sum",
                                      "int.impl.costs.sum", "int.sust.costs.sum")

saveRDS(object = outcome.comb.mx, file = paste0("Combination/Outcome-All-Combination-", CITY, "-DM.rds"))
toc()

QALYs_LA <- outcome.comb.mx[,"QALYs.sum"]
costs_total_LA <- outcome.comb.mx[,"costs.total.sum"]

#* Importing status quo values
status_quo <- unlist(readRDS(paste0("Inputs/Combination-DM-", CITY, "-refcase-outcomes.rds")))

#* Recreating table for Supplementary Table 3 Panel A 
Supp_Table_3_Panel_A <- matrix(nrow=10,ncol=4)
colnames(Supp_Table_3_Panel_A) <- c("Strategy Index","Incremental Cost: $M", "Incremental QALYs","ICER: $/ QALY")
Supp_Table_3_Panel_A[,1] <- c(combination.frontier.list)
Supp_Table_3_Panel_A[,2] <- round(c((costs_total_LA-status_quo[[2]])/1000000),digits=1)
Supp_Table_3_Panel_A[,3] <- round(c(QALYs_LA-status_quo[[1]]),digits=0)

for (i in 1:10) {
  if (Supp_Table_3_Panel_A[i,2] <= 0) {
    Supp_Table_3_Panel_A[i,4] <- c("CS")
  }
  
  else {
    cat("calculate ICER \n")
    Supp_Table_3_Panel_A[i,4] <- round((outcome.comb.mx[i,"costs.total.sum"]-outcome.comb.mx[i-1,"costs.total.sum"])/(outcome.comb.mx[i,"QALYs.sum"]-outcome.comb.mx[i-1,"QALYs.sum"]), digits=0)
  }
}

Supp_Table_3_Panel_A
saveRDS(object = Supp_Table_3_Panel_A, file = paste0("Combination/Outcome-All-Combination-", CITY, "-DM-Supp-Table-3-Panel-A.rds"))