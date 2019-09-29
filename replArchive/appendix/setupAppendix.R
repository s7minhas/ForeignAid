# Setting working directory
rm(list=ls())

# Loading libraries and functions
toLoad=c(
  'cshapes', 
  'reshape2', 'dplyr', 'devtools',
	'ggplot2', 'latex2exp', 'grid',
  'foreach', 'doParallel',
	'lme4', 'MASS'
  )

#' function to install and/or load packages
loadPkg=function(toLoad){
  for(lib in toLoad){
    if(!(lib %in% installed.packages()[,1])){ 
      install.packages(lib, repos='http://cran.rstudio.com/') }
    suppressMessages( library(lib, character.only=TRUE) )
  }
}

loadPkg(toLoad)

## gg theme
theme_set(theme_bw())
 
# require(devtools)
if(!'countrycode' %in% installed.packages()[,1]){
  install_version(
    "countrycode", 
    version = "0.16", 
    repos = "http://cran.us.r-project.org"
    ) }
library(countrycode)

# Setting seed
set.seed(6886)

# Source scripts
source('../main/intake/misc.R')
source('../main/intake/imputationHelpers.R')
source('../main/intake/tsHelpers.R')