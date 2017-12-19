#######################################################
rm(list=ls())

if(Sys.info()['user']=='janus829' | Sys.info()['user']=='s7m'){ 
	pathData='~/Dropbox/Research/ForeignAid/data'
	pathCode='~/Research/ForeignAid/RCode';
	pathResults='~/Dropbox/Research/ForeignAid/Results/GBME'}

setwd(pathData)
load('stratInterestMatrics.rda')
#######################################################

