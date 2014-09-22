rm(list=ls())
if(Sys.info()['user']=='janus829'){ 
	pathCode='~/Desktop/Research/ForeignAid/RCode' }
if(Sys.info()['user']=='s7m'){ 
	pathCode='~/Research/ForeignAid/RCode' }
source(paste0(pathCode, '/setup.R')) 

#######################################################
setwd(pathData)
load('stratInterestMatrics.rda')
#######################################################

#######################################################
source(paste0(pathCode, '/Analysis/gbme.asym.R')) 
source(paste0(pathCode, '/Analysis/gbme_mixture.R')) 
source(paste0(pathCode, '/Analysis/gbme_mix_ef.R')) 

nullGBME=function(
	matList, matName, yrs=names(matList),
	direct=FALSE, family='binomial', imps=3000, ods=2){
	
	# Loop through matrices in list
	for(t in 1:length(yrs)){

		# DV
		Y = matList[[char(yrs[t])]]
		n = nrow(Y)

		# GBME
		afile=paste(matName, years[ii], 'A', sep='_')
		bfile=paste(matName, years[ii], 'B', sep='_')
		ofile=paste(matName, years[ii], 'OUT', sep='_')

		if(direct){
			ufile=paste(matName, years[ii], 'U', sep='_')
			vfile=paste(matName, years[ii], 'V', sep='_')
				gbme(Y = Y, fam=family, k=2, directed=direct,
					owrite=T, ofilename=ofile,
					efilename=ufile, ffilename = vfile, 
					awrite=T, bwrite=T, afilename=afile, bfilename=bfile,		
					NS = imps, odens = ods)
			} 
		else {
			zfile=paste(matName, years[ii], 'Z', sep='_')
				gbme(Y = Y, fam=family, k=2, directed=direct, 
					owrite=T, ofilename=ofile, zwrite=T, zfilename=zfile, 
					awrite=T, bwrite=T, afilename=afile, bfilename=bfile,
					NS = imps, odens = ods)
		}
	}
}
#######################################################

#######################################################
results=nullGBME(warMats, 'war')
#######################################################