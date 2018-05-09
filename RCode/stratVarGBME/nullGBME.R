#######################################################
rm(list=ls())

if(Sys.info()['user']=='janus829'){ 
	pathData='~/Google Drive/Research/ForeignAid/Data'
	pathCode='~/Desktop/Research/ForeignAid/RCode';
	pathResults='~/Google Drive/Research/ForeignAid/Results/GBME'}
if(Sys.info()['user']=='s7m'){ 
	pathData='~/Google Drive/Research/ForeignAid/Data'	
	pathCode='~/Research/ForeignAid/RCode';
	pathResults='~/Google Drive/Research/ForeignAid/Results/GBME'}

setwd(pathData)
load('stratInterestMatrics.rda')
#######################################################

#######################################################
nullGBME=function( matList, matName, yrs,
	direct=FALSE, family='binomial', imps=6000, ods=2){
   
  print(paste0('Running GBME on ', matName, ' network from ', 
  	yrs[1], ' to ', yrs[length(yrs)] ))
	
	# Loop through matrices in list
	for(t in 1:length(yrs)){

		# dv
		Y = matList[[t]]
		n = nrow(Y)

		# GBME
		afile=paste(matName, yrs[t], 'A', sep='_')
		bfile=paste(matName, yrs[t], 'B', sep='_')
		ofile=paste(matName, yrs[t], 'OUT', sep='_')

		if(direct){
			source(paste0(pathCode, '/Analysis/stratVar/gbme.asym.R'))
			ufile=paste(matName, yrs[t], 'U', sep='_')
			vfile=paste(matName, yrs[t], 'V', sep='_')
			setwd(pathResults)
			set.seed(6886)
			gbme(Y = Y, fam=family, k=2, directed=direct,
				efilename=ufile, ffilename = vfile, 
				owrite=T, awrite=F, bwrite=F, 
				ofilename=ofile, afilename=afile, bfilename=bfile,
				NS = imps, odens = ods, N=matrix(1,n,n))
			} else {
			source(paste0(pathCode, '/Analysis/gbme.R')) 
			zfile=paste(matName, yrs[t], 'Z', sep='_')
			setwd(pathResults)
			gbme(Y = Y, fam=family, k=2, directed=direct, 
				zwrite=T, zfilename=zfile, 
				owrite=T, awrite=F, bwrite=F, 
				ofilename=ofile, afilename=afile, bfilename=bfile,
				NS = imps, odens=ods, N=matrix(1,n,n))
		}
    cat(paste0('\t\tYear ', yrs[t], ' finished...'))
	}
}
#######################################################