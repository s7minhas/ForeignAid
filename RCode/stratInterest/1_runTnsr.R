if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/ForeignAid/RCode/setup.R') }

############################
# Load amen data
load( paste0(pathTnsr,'amenData_all_rescaled.rda') )
load( paste0(pathTnsr,'amenData_all_stdz.rda') )
# load( paste0(pathTnsr,'amenData_all.rda') )
dimnames(amData[[1]])[[3]]


head(amData[[1]][,,8])

############################

############################
# Create directory to save latent space results
dir.create(paste0(pathTnsr, 'tnsrSpace/'), showWarnings=FALSE)

# Subset to relev vars
 inclVars = c('agree3un','totAllyCnt','igo')
# inclVars = c('agree3un','allyWt','igo')
# inclVars = c('agree3un','allyWt','igoWt')
# inclVars = c('agree3un','totAllyCnt','igoWt')
# inclVars = c('idealPtun','totAllyCnt','igo')
# inclVars = c('idealPtun','allyWt','igo')
# inclVars = c('idealPtun','allyWt','igoWt')
# inclVars = c('idealPtun','totAllyCnt','igoWt')
# inclVars = c('idealPtun','defense','igo')
# inclVars = c('idealPtun','defEntSum','igo')
# inclVars = c('idealPtun','defEnt','igo')


amDataRaw = lapply(amData, function(x){
	x = x[,,inclVars]
	return(x)
	})

#
#loadPkg('rTensor')
amDataSl = amDataRaw[[1]]
amDataSl[is.na(amDataSl)] = 0
amDataSlTnsr = as.tensor(amDataSl)
amDataSlTckr <- tucker(amDataSlTnsr, ranks=c(20,20,3))


amDataSlTckr_hosvd <- tucker(amDataSlTnsr, ranks=c(20,20,3))
hosvd_prod = ttl(amDataSlTckr_hosvd$Z, amDataSlTckr_hosvd$U, 1:3)

 <- tnsr - HOSVD_prod
table(abs(error@data) < 1e-12)
 
 

amDataSlApprox <- ttl(amDataSlTckr$Z, amDataSlTckr$U, 1:3)
fnorm(amDataSlTnsr - amDataSlApprox) / fnorm(amDataSlTnsr)

amDataSlTckr$norm_percent

# Parallelize run for every year
cl = makeCluster(6)
registerDoParallel(cl)
yrs = names(amData)
res=foreach(yr = yrs, .packages=c("rTensor")) %dopar% {
	# Run tucker decomp
	amDataSl = amDataRaw[[yr]]
	amDataSl[is.na(amDataSl)] = 0
	amDataSlTnsr = as.tensor(amDataSl)
	amDataSlTckr <- tucker(amDataSlTnsr, ranks=c(20,20,3))
}



lapply(res, function(x) x$norm_percent)
# Free my clusters
stopCluster(cl)
############################

hamming distances
if we want to get a lower respresentation, we can use hamming distances, to calaculate a euclidean distance 