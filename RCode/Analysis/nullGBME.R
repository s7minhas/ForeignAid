rm(list=ls())
if(Sys.info()['user']=='janus829'){ 
	pathCode='~/Desktop/Research/ForeignAid/RCode' }
if(Sys.info()['user']=='s7m'){ 
	pathCode='~/Research/ForeignAid/RCode' }
source(paste0(pathCode, '/setup.R')) 
# source('setup.R') 

#######################################################
setwd(pathData)
load('stratInterestMatrics.rda')
#######################################################

#######################################################
source(paste0(pathCode, '/Analysis/gbme.asym.R')) 
# source('gbme.asym.R')

nullGBME=function( matList, matName, yrs,
	direct=FALSE, family='binomial', imps=3000, ods=2){
  
  # 
  print(paste0('Running GBME on ', matName, ' network from ', 
  	yrs[1], ' to ', yrs[length(yrs)] ))
	
	# Loop through matrices in list
	for(t in 1:length(yrs)){

		# DV
		dv = matList[[t]]
		n = nrow(dv)

		# GBME
		afile=paste(matName, yrs[t], 'A', sep='_')
		bfile=paste(matName, yrs[t], 'B', sep='_')
		ofile=paste(matName, yrs[t], 'OUT', sep='_')

		if(direct){
			ufile=paste(matName, yrs[t], 'U', sep='_')
			vfile=paste(matName, yrs[t], 'V', sep='_')
				gbme(Y = dv, fam=family, k=2, directed=direct,
					owrite=F, ofilename=ofile,
					efilename=ufile, ffilename = vfile, 
					awrite=F, bwrite=F, afilename=afile, bfilename=bfile,		
					NS = imps, odens = ods)
			} 
		else {
			zfile=paste(matName, yrs[t], 'Z', sep='_')
			gbme(Y = dv, fam=family, k=2, directed=direct, 
				owrite=F, ofilename=ofile, zwrite=T, zfilename=zfile, 
				awrite=F, bwrite=F, afilename=afile, bfilename=bfile,
				NS = imps)
		}
    cat(paste0('\t\tYear ', yrs[t], ' finished...'))
	}
}
#######################################################

#######################################################
results=nullGBME(matList=igoMats, matName='igo', yrs=names(igoMats), 
                 direct=FALSE,family='poisson')
#######################################################