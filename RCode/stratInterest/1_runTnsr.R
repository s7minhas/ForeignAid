if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/ForeignAid/RCode/setup.R') }

############################
# Load amen data
# load( paste0(pathTnsr,'amenData_all_rescaled.rda') )
load( paste0(pathTnsr,'amenData_all_stdz.rda') )
# load( paste0(pathTnsr,'amenData_all.rda') )
dimnames(amData[[1]])[[3]]
############################

############################
# Create directory to save latent space results
dir.create(paste0(pathTnsr, 'tnsrSpace/'), showWarnings=FALSE)

# Subset to relev vars
inclVars = c('agree3un','totAllyCnt','igo')
amData = lapply(amData, function(x){
	x = x[,,inclVars]
	return(x)
	})

#
loadPkg('rTensor')
amDataSl = amData[[1]]
amDataSl[is.na(amDataSl)] = 0
amDataSlTnsr = as.tensor(amDataSl)
amDataSlTckr <- tucker(amDataSlTnsr, ranks=c(20,20,3))

# amDataSlApprox <- ttl(amDataSlTckr$Z, amDataSlTckr$U, 1:3)
# fnorm(amDataSlTnsr - amDataSlApprox) / fnorm(amDataSlTnsr)

amDataSlTckr$norm_percent

# Parallelize run for every year
cl = makeCluster(6)
registerDoParallel(cl)
yrs = names(amData)
res=foreach(yr = yrs, .packages=c("rTensor")) %dopar% {
	# Run tucker decomp

}

# Free my clusters
stopCluster(cl)
############################