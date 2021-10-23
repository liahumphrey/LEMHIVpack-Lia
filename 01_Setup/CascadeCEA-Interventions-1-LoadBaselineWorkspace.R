library(openxlsx)
library(abind)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(deSolve) # for the function "euler" or "ode"
library(abind)
library(zoo)
library(tictoc)
library(rmarkdown)
library(stringr)
library(LEMHIVpack)

# ## Required only when using plotly for graphs
# library(plotly)
# Sys.setenv("plotly_username"="ekheru")
# Sys.setenv("plotly_api_key"="jR1rdgfrKKtr0xnRbutx")

##packages and inputs required only when do parallel modeling
#* library(foreach)
#* library(doFuture)
#* doFuture::registerDoFuture()
#* cores <- detectCores()
#* future::plan(cluster, workers = cores[1]-2)  ##number of cores, default: (PCcores - 2)

# source("CascadeCEA-Interventions-1-LoadFunctions.R") library(LEMHIVpack) loads all model functions

## GET EXPORT VECTOR OF NAMES FOR RUNNING PARALLEL
source("01_Setup/CascadeCEA-Interventions-1-ParNamesExportIntModel.R")

## GET INTERVENTIONS AND CEA PARAMETERS
interventions <- c("Opt-out testing (ER)", "Opt-out testing (PC)",
                   "EMR testing reminder", "Nurse-initiated testing",
                   "OAT integrated testing",

                   "ART initiation", "ART retention", "ART retention, targeted",
                   "ART EMR reminder", "RAPID ART",

                   "ART re-initiation", "ART re-linkage",

                   "SSP", "OAT with BUP", "OAT with methadone", "PrEP",

                   "No interventions")

## SET time periods
int.sus        <- 10        # intervention sustainment duration = 10 years

if (technical_assessment_part == 1) {
  int.first.year <- 2020      # year of intervention starts = 2020
  lyr            <- 2040      # end of (last) projection year = 2040
}
if (technical_assessment_part == 2) {
  int.first.year <- 2021
  lyr            <- 2041
}
yr             <- 2012:lyr  # vector of year

#### SET MONTHLY PROJECTION PERIOD ####
start.proj     <- (int.first.year - yr[1]) * 12   #* (2020-2012)*12 = 96 months pre-intervention
end.proj       <- ((lyr - yr[1]) * 12)            #* (2040-2012)*12 = 336 months, duration of projection period (SHOULD BE 348 SINCE GOING TO END OF 2040?)

## Set time steps ##
nyr = lyr-2012+1            # no. of years (*29)
end_yr_ind = c(12*(1:nyr))  # indicator for year-end in month format
yr  = 2012:lyr              # year index (2012-2040 by year)
n   = nyr*12                # from 2012 to lyr(2040) by month
vt  = seq(0, n, 1)          #* monthly timestep variable, includes t=0

# Set possible scales
scale.increments <- c(seq(0.1, 1, 0.1))

# Set scale-up
scale.up        <- TRUE
scale.up.period <- 18

# Set sustainment parameters
int.start <- end_yr_ind[(which(int.first.year == yr, arr.ind = TRUE) - 1)] #* (selecting which month intervention begins, aka t = 96 aka Jan of 2020)
int.end <- (int.start + (int.sus * 12))  #* 10 yr sustainment period for scenarios -> 120 months -> finish interventions at t = 216

# Discount rate for cost and QALY
Discounting <- 0.03

# City list
all.cities <- c("ATL", "BAL", "LA", "MIA", "NYC", "SEA")
