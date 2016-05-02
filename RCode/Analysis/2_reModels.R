if(Sys.info()['user']=='s7m' | Sys.info()['user']=='janus829'){ source('~/Research/ForeignAid/RCode/setup.R') }
if(Sys.info()['user']=='cindycheng'){ source('~/Documents/Papers/ForeignAid/RCode/setup.R') }

################################################################
# Load reg data
setwd(pathData)
load('iData.rda')
################################################################

################################################################
# RE model

## mod formula
cntrlVars=c(
	'colony' # Colonial variable
	,'Lpolity2' # Institutions
	,'LlnGdpCap' # Macroecon controls
	,'LlifeExpect', 'Lno_disasters' # Humanitarian
	,'Lcivwar' # Civil war
	)

# model spec gen
genModelForm = function(var){
	formula(
		paste0( 
			'commitUSD13 ~ ',  # DV
			var, ' + ', # add key var
			paste(cntrlVars, collapse=' + '), # add control vars
			'+ (1|ccodeS) + (1|ccodeR)') # random sender and receiver effects
		)
}

# filename gen
genFileName = function(train, mod, zeroInf, keyVar){
	a = ifelse(train, paste0('trainSampEnd', trainEnd), 'fullSamp')
	b = mod ; c = if(zeroInf){ 'zi' } ; d = keyVar
	f = paste0(paste(a,b,c,d, sep='_'), '.rda')
	return( gsub('__','_',f) )
}

# Run models in parallel across imputed datasets
runModelParallel = function(cores=5, dataList, trainLogic, trainEnd=2002, modType, zeroInfLogic, keyRegVar, modForm, modName){
	modForm=genModelForm(keyRegVar)
	modName = genFileName(trainLogic, modType, zeroInfLogic, keyRegVar)
	print(paste0('Running model: ', Reduce(paste, deparse(modForm))))
	print(paste0('Saving to: ', modName))
	cl=makeCluster(cores) ; registerDoParallel(cl)
	mods = foreach(ii=1:length(dataList), .packages=c('glmmADMB')) %dopar% {
		regData = dataList[[ii]] # Subset to relevant data
		if(trainLogic){regData$year=num(regData$year); regData=regData[regData$year<trainEnd,] ; regData$year=factor(regData$year, levels=sort(unique(regData$year))) }	
		m=glmmadmb(modForm, data=regData, zeroInflation=zeroInfLogic, family=modType, extra.args="-ndi 90000") # Run
	} ; stopCluster(cl)
	save(mods, file=paste0(pathResults, '/', modName)) # Save output	
}

# Model 1
# data = full sample # model = zi pois # key var = lat space strat
runModelParallel(dataList=iData, trainLogic=FALSE, modType = 'poisson', zeroInfLogic=TRUE, keyRegVar='LstratMu')

# Model 2
# data = full sample # model = zi nbinom # key var = lat space strat
runModelParallel(dataList=iData, trainLogic=FALSE, modType = 'nbinom', zeroInfLogic=TRUE, keyRegVar='LstratMu')

# Model 3
# data = training # model = zi pois # key var = lat space strat
runModelParallel(dataList=iData, trainLogic=TRUE, modType = 'poisson', zeroInfLogic=TRUE, keyRegVar='LstratMu')

# Model 4
# data = training # model = zi nbinom # key var = lat space strat
runModelParallel(dataList=iData, trainLogic=TRUE, modType = 'nbinom', zeroInfLogic=TRUE, keyRegVar='LstratMu')

# Model 5
# data = training (1975-2000) # model = zi pois # key var = ally binary
runModelParallel(dataList=iData, trainLogic=TRUE, modType = 'poisson', zeroInfLogic=TRUE, keyRegVar='LallyWt')

# Model 6
# data = training (1975-2000) # model = zi pois # key var = igo count
runModelParallel(dataList=iData, trainLogic=TRUE, modType = 'poisson', zeroInfLogic=TRUE, keyRegVar='Ligo')

# Model 7
# data = training (1975-2000) # model = zi pois # key var = un ideal point score
runModelParallel(dataList=iData, trainLogic=TRUE, modType = 'poisson', zeroInfLogic=TRUE, keyRegVar='LunIdPt')

# Model 8
# data = training (1975-2000) # model = zi nbinom # key var = ally binary
runModelParallel(dataList=iData, trainLogic=TRUE, modType = 'nbinom', zeroInfLogic=TRUE, keyRegVar='LallyWt')

# Model 9
# data = training (1975-2000) # model = zi nbinom # key var = igo count
runModelParallel(dataList=iData, trainLogic=TRUE, modType = 'nbinom', zeroInfLogic=TRUE, keyRegVar='Ligo')

# Model 10
# data = training (1975-2000) # model = zi nbinom # key var = un ideal point score
runModelParallel(dataList=iData, trainLogic=TRUE, modType = 'nbinom', zeroInfLogic=TRUE, keyRegVar='LallyWt')
