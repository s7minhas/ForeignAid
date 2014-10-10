#######################################################
rm(list=ls())

if(Sys.info()['user']=='janus829'){ 
	pathData='~/Google Drive/Research/ForeignAid/Data'
	pathCode='~/Desktop/Research/ForeignAid/RCode';
	pathResults='~/Google Drive/Research/ForeignAid/Results'}
if(Sys.info()['user']=='s7m'){ 
	pathData='~/Google Drive/Research/ForeignAid/Data'	
	pathCode='~/Research/ForeignAid/RCode';
	pathResults='~/Google Drive/Research/ForeignAid/Results/GBME'}

if(Sys.info()['user']=='cindycheng'){ 
	pathCode='~/Documents/Papers/ForeignAid/RCode' }

setwd(paste0(pathCode, '/Analysis'))
source('gbme.r')
#######################################################

# Function objectives 

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
setwd(paste0(pathResults,'/gbmeLatSpace'))
test= "un_1995_Z"
outTest="un_1995_OUT"

###analysis of latent positions
Z=read.table(test)
OUT=read.table(outTest, header=TRUE)

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
test=apply(PZ, c(1,2), mean)

# Now calculate euclidean distance between points

#######################################################