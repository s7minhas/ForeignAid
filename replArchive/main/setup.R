# Setting working directory
rm(list=ls())

if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m" ){
  pathMain = '/Volumes/Samsung_X5/Dropbox/Research/'
  # pathMain = '~/Dropbox/Research/'
  pathGraphics=paste0(pathMain, "ForeignAid/Graphics");
  pathResults=paste0(pathMain, 'ForeignAid/Results');
  pathCode='~/Research/ForeignAid/RCode/Funcs'
  pathData=paste0(pathMain, "ForeignAid/Data")
  pathTnsr=paste0(pathMain, "ForeignAid/Data/tnsrData/")
}

if(Sys.info()["user"]=="cindycheng") {
  pathMain="~/Dropbox/Documents/Papers/ForeignAid";
  pathGraphics="~/Dropbox/Documents/Papers/ForeignAid/graphics";
  pathData="~/Dropbox/Documents/Papers/ForeignAid/data";
  pathCode="~/Documents/Papers/ForeignAid/RCode/Funcs";
  rPath = "~/Documents/Papers/ForeignAid/RCode";
  pathResults = '~/Dropbox/Documents/Papers/ForeignAid/Results'
  pathTnsr="~/Dropbox/Documents/Papers/ForeignAid/Data/tnsrData/"
}
 
# Loading libraries and functions
toLoad=c('snow', 'mcmcplots', 'foreign', 'cshapes', 'countrycode', 'reshape2', 
	'gtools', 'ggplot2', 'doBy', 'Amelia', 'tikzDevice', 'latex2exp',
  'foreach', 'doParallel',
	'igraph', 'bipartite', 'lme4', 'glmmADMB','MASS', 'grid',
  'dplyr')

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