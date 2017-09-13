if(Sys.info()['user']=='s7m' | Sys.info()['user']=='janus829'){ source('~/Research/ForeignAid/RCode/setup.R') }
if(Sys.info()['user']=='cindycheng'){ source('~/Dropbox/Documents/Papers/ForeignAid1/RCode/setup.R') }

################################################################
# Load reg data
setwd(pathData)
load('iData_v2.rda')
# Add dyad random effect
iData = lapply(iData, function(x){
	# add dyadic id
	x$id = paste(x$ccodeS, x$ccodeR, sep='_')
	x$id = factor(x$id)
	# log aid flow
	x$commitUSD13 = log(x$commitUSD13 + 1)
	return(x)
	})
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
genModelForm = function(var, type, struc, interaction = FALSE){
	if(type=='re'){ strucChar=paste0(' + ', paste('(1|',struc, ')', collapse=' + ')) }
	if(type=='fe'){ strucChar=paste0(' + ', paste('factor(',struc,')',collapse=' + '), ' - 1') }
	if(type=='none'){ strucChar=NULL }

	if ( interaction == FALSE){
			form = formula(
			paste0(  'commitUSD13 ~ ',  # DV
			paste(var, collapse=' + '), ' + ', # add key var
			paste(cntrlVars, collapse=' + '), # add control vars
			strucChar )  )}
	else if (interaction == TRUE){
			form = formula(
			paste0(  'commitUSD13 ~ ',  # DV
			paste(c(var, paste(c(var, 'Lno_disasters'), collapse = '*')), collapse=' + '), ' + ', # add key var
			paste(cntrlVars, collapse=' + '), # add control vars
			strucChar )  )
	}

	return(form)
}

 
# filename gen
genFileName = function(train, mod, type, zeroInf, keyVar, trainEnd=2002, interaction = FALSE){
	a = ifelse(train, 'trainSamp', 'fullSamp')
	b = mod ; c = type ; d = if(zeroInf){ 'zi' } ; e = paste(keyVar, collapse='')
	f = ifelse(interaction, 'interaction', '' )
	g = paste0(paste(a,b,c,d,e,f, sep='_'), '.rda')
	return( gsub('__','_',g) )
}

# Run models in parallel across imputed datasets
runModelParallel = function(
	cores=detectCores(),
	dataList=iData, trainLogic=FALSE, trainEnd=2002, 
	modType='re', modFamily='gaussian', zeroInfLogic=FALSE, 
	keyRegVar='LstratMu', modStruc=c('id','year'), int = FALSE
	){

	modForm = genModelForm(var=keyRegVar, type=modType, struc=modStruc, interaction = int)
	modName = genFileName(train=trainLogic, mod=modFamily, type=modType, zeroInf=zeroInfLogic, keyVar=keyRegVar, interaction = int)

	print(paste0('Running model: ', Reduce(paste, deparse(modForm))))
	print(paste0('Saving to: ', modName))

	cl=makeCluster(cores) ; registerDoParallel(cl)
	mods = foreach(ii=1:length(dataList), .packages=c('glmmADMB', 'lme4')) %dopar% {

		regData = dataList[[ii]] # Subset to relevant data

		if(trainLogic){
			regData$year=as.numeric(as.character(regData$year)); regData=regData[regData$year<trainEnd,]
			regData$year=factor(regData$year, levels=sort(unique(regData$year)))
		}	

		if(modType=='re' & !zeroInfLogic){
			m=lmer(modForm, data=regData)
		}
		
		if(modType=='re' & zeroInfLogic){
			m=glmmadmb(modForm, data=regData, zeroInflation=zeroInfLogic, family=modFamily, extra.args="-ndi 100000")
		}

		if(modType=='fe'){
			stopifnot(modFamily=='gaussian')
			m=lm(modForm, data=regData)
		}
		
		return(m)
	}
	stopCluster(cl)
	save(mods, file=paste0(pathResults, '/', modName)) # Save output	
}
################################################################

# ################################################################
# # Run main models
# # Full sample model, random effect, LstratMu, runs in a couple of minutes
# runModelParallel(trainLogic=FALSE, modType='re', keyRegVar='LstratMu')

# # robustness check, random effect with zero inflation, takes about six hours to run
# runModelParallel(trainLogic=FALSE, modType='re', zeroInfLogic=TRUE, keyRegVar='LstratMu')

# # robustness check, fixed effects, takes about 30 mins to run
# runModelParallel(trainLogic=FALSE, modType='fe', keyRegVar='LstratMu')
# ################################################################

################################################################
# # Run interaction models
# Full sample model, random effect, LstratMu, runs in a couple of minutes
runModelParallel(trainLogic=FALSE, modType='re', keyRegVar='LstratMu', int = T)

 
# Full sample model, random effect, LallyWt, runs in a couple of minutes
runModelParallel(trainLogic=FALSE, modType='re', keyRegVar='LallyWt', int = T)

# Full sample model, random effect, LunIDpt, runs in a couple of minutes
runModelParallel(trainLogic=FALSE, modType='re', keyRegVar='LunIdPt', int = T)

 

# Full sample model, random effect, LunIDpt, runs in a couple of minutes
runModelParallel(trainLogic=FALSE, modType='re', keyRegVar='Ligo', int = T)

 
# Full sample model, random effect, LallyWt + LunIdPt + Ligo, runs in a couple of minutes
runModelParallel(trainLogic=FALSE, modType='re', keyRegVar=c('LallyWt', 'LunIdPt','Ligo'), int = T)

 

# robustness check, fixed effects, takes about 30 mins to run
runModelParallel(trainLogic=FALSE, modType='fe', keyRegVar='LstratMu', int = TRUE)
# ################################################################

################################################################



# Compare to existing measures using two fold temporally cut cv
# Training, random effect, Lstratmu
runModelParallel(trainLogic=TRUE, modType='re', keyRegVar='LstratMu')

# Training, random effect, LallyWt
runModelParallel(trainLogic=TRUE, modType='re', keyRegVar='LallyWt')

# Training, random effect, LunIdPt
runModelParallel(trainLogic=TRUE, modType='re', keyRegVar='LunIdPt')

# Training, random effect, Ligo
runModelParallel(trainLogic=TRUE, modType='re', keyRegVar='Ligo')

# Training, random effect, LallyWt + LunIdPt + Ligo
runModelParallel(trainLogic=TRUE, modType='re', keyRegVar=c('LallyWt', 'LunIdPt','Ligo'))
################################################################