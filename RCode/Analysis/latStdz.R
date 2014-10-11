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

# Procrustes transformation: rotation and reflection
proc.rr<-function(Y,X){
	k<-dim(X)[2]
	A<-t(Y)%*%(  X%*%t(X)  )%*%Y
	eA<-eigen(A,symmetric=T)
	Ahalf<-eA$vec[,1:k]%*%diag(sqrt(eA$val[1:k]),nrow=k)%*%t(eA$vec[,1:k])
	t(t(X)%*%Y%*%solve(Ahalf)%*%t(Y)) }

fileFinder=function(x, num, stuff,info=FALSE){
	files=unlist(lapply(strsplit(stuff, '_'), 
		function(z) z[[num]]))
	stuff[which(files == x)]
}
#######################################################

#######################################################
# File objectives 

	# Takes in a test name and out name for given var-year

	# Applied procrustrean transformation : DONE

	# Need to find posterior mean for each country position: DONE

	# Want to calculate euclidean distance between each country position

	# THen we want to get a sense of the distribution of this

	# We want to standardize the distance
		
		# Using either z score

		# simon's approach of subtracting mean

	# Output of all this should be the standardized dyadic distance

		# Save output to file
#######################################################

#######################################################
# ally, un, igo, warMsum5
Y='ally'
yMat=allyMats
yrs=names(yMat)

setwd(paste0(pathResults,'/gbmeLatSpace'))
oNames=paste(Y, yrs, 'OUT', sep='_')
zNames=paste(Y, yrs, 'Z', sep='_')

# Load relev data
yData=yMat[[yrs[1]]]; ids=rownames(yData)
OUT=read.table(oNames[1], header=TRUE)
Z=read.table(zNames[1])

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

# Now calculate euclidean distance between points
latDist <- matrix(NA, nrow=n, ncol=n, dimnames=list(ids, ids))
for(ii in 1:length(ids)){
  for(jj in 1:length(ids)){
    latDist[ii,jj] <- sqrt( (pzMu[ids[ii],1] - pzMu[ids[jj],1])^2 
      + (pzMu[ids[ii],2] - pzMu[ids[jj],2])^2 )
  }
}
#######################################################