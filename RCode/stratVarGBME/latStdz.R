#######################################################
rm(list=ls())
if(Sys.info()['user']=='janus829'){ 
	pathCode='~/Desktop/Research/ForeignAid/RCode' }
if(Sys.info()['user']=='s7m'){ 
	pathCode='~/Research/ForeignAid/RCode' }
if(Sys.info()['user']=='cindycheng'){ 
	pathCode='~/Documents/Papers/ForeignAid/RCode' }

setwd(pathCode); source('setup.R') 
load(paste0(pathData,'/stratInterestMatrics.rda'))
#######################################################

#######################################################
# File Specific Functions

# Run lat standardization to distance
latToDist=function(yName, yList, divMean, symm, outfilename){
	yrs=names(yList)
	oNames=paste(yName, yrs, 'OUT', sep='_')

	if ( symm == T ) {
		zNames=paste(yName, yrs, 'Z', sep='_')}
	else if( symm == F ){
		uNames = paste(yName, yrs, 'U', sep = '_')
		vNames = paste(yName, yrs, 'V', sep = '_')}

	res=NULL
	for(ii in 1:length(yrs)){
		yData=yList[[ii]]
		ids=rownames(yData)

		if (symm == T){
			pzMu=getPosInSpaceZ(oNames[ii], zNames[ii], ids)
			latDist=getDyadDist(pzMu, ids)}
		else if (symm == F){
			puMu=getPosInSpace(oNames[ii], uNames[ii], ids)
			pvMu=getPosInSpace(oNames[ii], vNames[ii], ids)
			latDist=getDyadDistAsym(puMu, pvMu, ids)}}

		latDist=stdize(latDist, divMean)
		res = rbind(res, meltSymm(latDist, yrs[ii], yName), symm)
		print(paste0('Distance calculated for ',yName,':',yrs[ii]))
#  do this afterwards so that the min is the minimum across all years
	if (divMean == F & sign(min(res[,4]))==-1 ){res[,4] = res[,4] - min(res[,4]) }
	setwd(paste0(pathResults,'/gbmeLatDist'))
	save(res, file= paste0(outfilename, '.rda' ))
	print(paste0('results saved for ', yName))
}

# Procrustes transformation: rotation and reflection
proc.rr<-function(Y,X){
	k<-dim(X)[2]
	A<-t(Y)%*%(  X%*%t(X)  )%*%Y
	eA<-eigen(A,symmetric=T)
	Ahalf<-eA$vec[,1:k]%*%diag(sqrt(eA$val[1:k]),nrow=k)%*%t(eA$vec[,1:k])
	t(t(X)%*%Y%*%solve(Ahalf)%*%t(Y)) }


 
# Code from Hoff to get latent space positions
getPosInSpace=function(oname, zname, ids){
	
	# Load data
	setwd(paste0(pathResults,'/gbmeLatSpace/NewLatSpace'))
	OUT=read.table(oname, header=TRUE)
	
	Z=read.table(zname)

	#convert to an array
	nss=dim(OUT)[1]
	n=dim(Z)[1]/nss
	k=dim(Z)[2]
	PZ=array(dim=c(n,k,nss))
	for(i in 1:nss) { PZ[,,i]=as.matrix(Z[ ((i-1)*n+1):(i*n) ,])  }

	PZ=PZ[,,-(1:round(nss/2))]     #drop first half for burn in

	#find posterior mean of Z %*% t(Z)
	ZTZ=matrix(0,n,n)
	for(i in 1:dim(PZ)[3] ) { ZTZ=ZTZ+PZ[,,i]%*%t(PZ[,,i]) }
	ZTZ=ZTZ/dim(PZ)[3] 

	#a configuration that approximates posterior mean of ZTZ
	tmp=eigen(ZTZ)
	Z.pm=tmp$vec[,1:k]%*%sqrt(diag(tmp$val[1:k]))

	#now transform each sample Z to a common orientation
	for(i in 1:dim(PZ)[3] ) { PZ[,,i]=proc.rr(PZ[,,i],Z.pm) }

	# Find posterior mean of country positions
	pzMu=apply(PZ, c(1,2), mean); rownames(pzMu)=ids
	pzMu
}
 

# Euclidean distance between two points
getDyadDist=function(posMatrix, ids){
	n=nrow(posMatrix)
	distMatrix = matrix(NA, nrow=n, ncol=n, dimnames=list(ids, ids))
	for(ii in 1:length(ids)){
	  for(jj in 1:length(ids)){
	    distMatrix[ii,jj] = sqrt( (posMatrix[ids[ii],1] - posMatrix[ids[jj],1])^2 
	      + (posMatrix[ids[ii],2] - posMatrix[ids[jj],2])^2 )
	  }
	}
	distMatrix
}

getDyadDistAsym=function(posMatrixU, posMatrixV, ids){
	n=nrow(posMatrixU)
	distMatrix = matrix(NA, nrow=n, ncol=n, dimnames=list(ids, ids))
	for(ii in 1:length(ids)){
	  for(jj in 1:length(ids)){
	    distMatrix[ii,jj] = sqrt( (posMatrixU[ids[ii],1] - posMatrixV[ids[jj],1])^2 
	      + (posMatrixU[ids[ii],2] - posMatrixV[ids[jj],2])^2 )
	  }
	}
	distMatrix
}


# Standardize
stdize=function(x, divMean=TRUE){
	mu=mean(x)
	if(divMean){return(x/mu)}
	if(!divMean){sig=sd(x); return((x-mu)/sig)}
}

# Melt symmetric matrix
meltSymm=function(x, yr, vnme, symm){
	library(igraph)

	if(symm == T){
		graph=graph.adjacency(x, mode='undirected',weighted=TRUE)}
	else if (symm == F){
		graph=graph.adjacency(x, mode='directed',weighted=TRUE)}

	edgeVal=cbind(get.edgelist(graph), yr, E(graph)$weight)
	colnames(edgeVal)=c('ccode1','ccode2','year',paste0(vnme,'Dist'))
	apply(edgeVal, 2, num)
}
#######################################################