# Setting working directory
rm(list=ls())

if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m" ){
  pathMain="~/Dropbox/Research/ForeignAid";
  pathGraphics="~/Dropbox/Research/ForeignAid/Graphics";
  pathResults='~/Dropbox/Research/ForeignAid/Results';
  pathCode='~/Research/ForeignAid/RCode/Funcs'
  pathData="~/Dropbox/Research/ForeignAid/Data"
  pathTnsr="~/Dropbox/Research/ForeignAid/Data/tnsrData/"
}

if(Sys.info()["user"]=="cindycheng") {
  pathMain="~/Dropbox/Documents/Papers/ForeignAid";
  pathGraphics="~/Dropbox/Documents/Papers/ForeignAid/graphics";
  pathData="~/Dropbox/Documents/Papers/ForeignAid/data";
  pathCode="~/Documents/Papers/ForeignAid/RCode/Funcs";
  pathFunctions="~/Dropbox/Documents/Methods/R Functions"
  pathResults = '~/Dropbox/Documents/Papers/ForeignAid/Results'
  pathTnsr="~/Dropbox/Documents/Papers/ForeignAid/Data/tnsrData/"
}
 
# Loading libraries and functions
toLoad=c('snow', 'mcmcplots', 'foreign', 'cshapes', 'countrycode', 'reshape2', 
	'gtools', 'ggplot2', 'doBy', 'Amelia', 'tikzDevice', 
  'foreach', 'doParallel',
	'igraph', 'bipartite', 'lme4', 'glmmADMB','MASS', 'grid')

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

# Helper dataset
setwd(pathData)
load('panel.rda')
panel$cnameYear<-toupper(panel$cnameYear) 
panel$cname<-toupper(panel$cname) # not for some reason German Democratic Republic is notcapitalized
 
# Setting seed
set.seed(6886)
setwd(pathMain)

# Source scripts
setwd(pathCode)
source('misc.R')
source('vizResults.R')
source('dyadHelpers.R')
source('adjMatHelpers.R')
source('tsHelpers.R')
source('sbgcop_l2.R')
source('imputationHelpers.R')
source('bipartite.R')
source('relDataHelpers.R')