rm(list = ls())
if(Sys.info()['user']=='janus829'){ 
	pathCode='~/Desktop/Research/ForeignAid/RCode' }
if(Sys.info()['user']=='s7m'){ 
	pathCode='~/Research/ForeignAid/RCode' }
if(Sys.info()['user']=='cindycheng'){ 
	pathCode='~/Documents/Papers/ForeignAid/RCode' }
if(Sys.info()['user']=='cindy'){ 
  pathCode='/home/cindy' }

 
#######################################################
source(paste0(pathCode, '/Analysis/stratVar/latStdz.R'))
source(paste0(pathCode, '/latStdz.R'))
# latToDist('ally', allyMats, divMean = T)
latToDist('ally', allyWtMats, divMean = F, symm = F, outfilename = 'allyWtDist')
 
source(paste0(pathCode, '/latStdz.R'))
latToDist('mid', midMats, divMean = F, symm = T, outfilename = 'midDist')

source(paste0(pathCode, '/Analysis/stratVar/latStdz.R'))
#latToDist('igo', igoMats, divMean = T)
latToDist('igo', igoMats, divMean = F)

source(paste0(pathCode, '/Analysis/stratVar/latStdz.R'))
# latToDist('warMsum5', warMatsMsum5, divMean = T)
latToDist('warMsum5', warMatsMsum5, divMean = F)

source(paste0(pathCode, '/Analysis/stratVar/latStdz.R'))
# latToDist('un', unMats.agree2unA, divMean = T)
latToDist('un', unMats, divMean = F, symm = T, outfilename = 'unNewDist')
 
 
#######################################################
