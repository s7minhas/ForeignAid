if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/ForeignAid/RCode/setup.R') }
loadPkg('rTensor')

############################
# Load amen data
#load( paste0(pathTnsr,'amenData_all_rescaled.rda') )
# load( paste0(pathTnsr,'amenData_all_stdz.rda') )
load( paste0(pathTnsr,'amenData_all.rda') )
dimnames(amData[[1]])[[3]]

############################

############################
# Create directory to save latent space results
dir.create(paste0(pathTnsr, 'tnsrSpace/'), showWarnings=FALSE)

# Subset to relev vars


testVars = list( c('agree3un','totAllyCnt','igo'),
	c('agree3un','allyWt','igo'),
	c('agree3un','allyWt','igoWt'),
 	c('agree3un','totAllyCnt','igoWt'),
 	c('idealPtun','totAllyCnt','igo'),
 	c('idealPtun','allyWt','igo'),
 	c('idealPtun','allyWt','igoWt'),
 	c('idealPtun','totAllyCnt','igoWt'),
 	c('idealPtun','defense','igo'),
 	c('idealPtun','defEntSum','igo'),
 	c('idealPtun','defEntSum','igoWt'),
 	c('idealPtun','defEnt','igo'),
 	c('idealPtun','defEnt','igoWt'),
 	c('agree3un','defEntSum','igo'),
 	c('agree3un','defEntSum','igoWt'),
 	c('agree3un','defEnt','igo'),
 	c('agree3un','defEnt','igoWt'))
 

results = list()
for ( ii in 1:length(testVars)){
	inclVars = testVars[[ii]]
	amDataRaw = lapply(amData, function(x){
	x = x[,,inclVars]
	return(x)
	})

	amDataSl = amDataRaw[[1]]
	amDataSl[is.na(amDataSl)] = 0
	amDataSlTnsr = as.tensor(amDataSl)
	amDataSlTckr <- tucker(amDataSlTnsr, ranks=c(143,143,1))

 	results[[ii]] = c(amDataSlTckr$norm_percent, inclVars)

}

rawTnsr = do.call(rbind, results)
stdTnsr = do.call(rbind, results)
rescaleTnsr = do.call(rbind, results)

rawTnsr 
# check performance

# amDataSlApprox <- ttl(amDataSlTckr$Z, amDataSlTckr$U, 1:3)
# fnorm(amDataSlTnsr - amDataSlApprox) / fnorm(amDataSlTnsr)


inclVars = c('agree3un','defEntSum','igo')
amDataFinal = lapply(amData, function(x){
	x = x[,,inclVars]
	return(x)
	})

# Parallelize run for every year
cl = makeCluster(6)
registerDoParallel(cl)
yrs = names(amData)
res=foreach(yr = yrs, .packages=c("rTensor")) %dopar% {
	# Run tucker decomp
	amDataSl = amDataFinal[[yr]]
	amDataSl[is.na(amDataSl)] = 0
	amDataSlTnsr = as.tensor(amDataSl)
	amDataSlTckr <- tucker(amDataSlTnsr, ranks=c(143,143,1))
}

# Free my clusters
stopCluster(cl)

# check performance
lapply(res, function(x) x$norm_percent)

# save file
save(res, file = paste0(pathTnsr, 'tnsrSpace/tnsr_Ideal_DefEnt_igoWt.rda'))
############################




hamming distances
if we want to get a lower respresentation, we can use hamming distances, to calaculate a euclidean distance 