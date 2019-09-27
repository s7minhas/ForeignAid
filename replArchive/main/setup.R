# Setting working directory
rm(list=ls())

# Loading libraries and functions
toLoad=c(
  'snow', 'mcmcplots', 'foreign', 'cshapes', 'countrycode', 'reshape2', 
	'gtools', 'ggplot2', 'doBy', 'Amelia', 'tikzDevice', 'latex2exp',
  'foreach', 'doParallel',
	'igraph', 'bipartite', 'lme4', 'glmmADMB','MASS', 'grid',
  'dplyr'
  )

#' function to install and/or load packages
#' 
#' @param toLoad character vector of packages to load
#' @author Shahryar Minhas
#' @return adds libraries to worksapce
#' @export
#' 
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

### PACKAGE VERIONS
## Must have countrycode version 0.16 installed
# require(devtools)
# install_version("countrycode", version = "0.16", repos = "http://cran.us.r-project.org")
 
# Setting seed
set.seed(6886)

# Source scripts
source('misc.R')
source('vizResults.R')
source('dyadHelpers.R')
source('adjMatHelpers.R')
source('tsHelpers.R')
source('sbgcop_l2.R')
source('imputationHelpers.R')
source('bipartite.R')
source('relDataHelpers.R')