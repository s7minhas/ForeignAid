if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/ForeignAid/RCode/setup.R') }
if(Sys.info()["user"]=="cindycheng"){
    source('~/Documents/Papers/ForeignAid/RCode/setup.R') }

loadPkg('rTensor')
 
############################
# Load amen data
load( paste0(pathTnsr,'amenData_all_rescaled.rda') )
#load( paste0(pathTnsr,'amenData_all_stdz.rda') )
#load( paste0(pathTnsr,'amenData_all.rda') )
dimnames(amData[[1]])[[3]]
 
############################

############################
# Create directory to save latent space results
dir.create(paste0(pathTnsr, 'tnsrSpace/'), showWarnings=FALSE)

# Subset to relev vars

testVars = expand.grid(
						c('agree3un', 'agree2un', 'idealPtun'),
						c('allyWt', 'defEntSum', 'defEnt', 'anyAlly', 'totAllyCnt'),
						 c('igo', 'igoWt'),
						c('hostLev', 'midCount','fatalMids'),
						c('armsTrsfrs', 'armsPop', 'armsGdp' ))
 
testVars = sapply(testVars, as.character)
 
 
results = list()
for ( ii in 1:dim(testVars)[1]){
	inclVars = unlist(testVars[ii,])
	 
	amDataRaw = lapply(amData, function(x){
	x = x[,,inclVars]
	return(x)
	})

	amDataSl = amDataRaw[[1]]
	amDataSl[is.na(amDataSl)] = 0
	amDataSlTnsr = as.tensor(amDataSl)
	amDataSlTckr <- tucker(amDataSlTnsr, ranks=c(2,2,3))

 	results[[ii]] = c(amDataSlTckr$norm_percent, inclVars)

}

 
# rawTnsr = data.frame(do.call(rbind, results))
# stdTnsr = data.frame(do.call(rbind, results))
rescaleTnsr = data.frame(do.call(rbind, results))
rescaleTnsr[order(rescaleTnsr$V1),]

# check performance
# amDataSlApprox <- ttl(amDataSlTckr$Z, amDataSlTckr$U, 1:3)
# fnorm(amDataSlTnsr - amDataSlApprox) / fnorm(amDataSlTnsr)


inclVars = c('agree2un', 'defEntSum', 'igoWt',  'hostLev', 'armsGdp')


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
	amDataSlTckr <- tucker(amDataSlTnsr, ranks=c(2, 2, 3))
}

# Free my clusters
stopCluster(cl)

# check performance
lapply(res, function(x) x$norm_percent)

# save file
save(res, file = paste0(pathTnsr, 'tnsrSpace/tnsr_idealPtun_defEntSum_igoWt_fatalMids.rda'))
############################


