#######################################################
source('~/Research/ForeignAid/RCode/Analysis/nullGBME.R')
results=nullGBME(matList=allyMats, matName='ally', yrs=names(allyMats), 
	direct=FALSE,family='binomial')

source('~/Research/ForeignAid/RCode/stratVarGBME/nullGBME.R')
# results=nullGBME(matList=igoMats, matName='igo', yrs=names(igoMats), 
# 	direct=FALSE,family='poisson')
results=nullGBME(matList=igoMats, matName='igo', yrs=names(igoMats), 
	direct=FALSE,family='poisson')

source('~/Research/ForeignAid/RCode/Analysis/nullGBME.R')
results=nullGBME(matList=unMats, matName='un', yrs=names(unMats), 
	direct=FALSE,family='gaussian')

source('~/Research/ForeignAid/RCode/Analysis/nullGBME.R')
results=nullGBME(matList=warMatsMsum5, matName='warMsum5', yrs=names(warMatsMsum5), 
	direct=FALSE,family='poisson')
                 
source('~/Research/ForeignAid/RCode/Analysis/nullGBME.R')
results=nullGBME(matList=allyDirMats, matName='allyDir', yrs=names(allyDirMats), 
	direct=TRUE,family='binomial')   

source('~/Documents/Papers/ForeignAid/RCode/Analysis/stratVar/nullGBME.R')
results=nullGBME(matList=armsMats, matName='arms', yrs=names(armsMats), 
	direct=TRUE,family='gaussian')   
#######################################################