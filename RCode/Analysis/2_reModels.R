if(Sys.info()['user']=='s7m' | Sys.info()['user']=='janus829'){ source('~/Research/ForeignAid/RCode/setup.R') }
if(Sys.info()['user']=='cindycheng'){ source('~/Documents/Papers/ForeignAid/RCode/setup.R') }

################################################################
# Load reg data
setwd(pathData)
load('iDataDisagg.rda')

# vars for analysis
dvs = c('humanitarianTotal', 'developTotal', 'civSocietyTotal', 'notHumanitarianTotal')
ids = names(iData[[1]])[c(1:3,25)]
ivs = names(iData[[1]])[9:24]

# quick function to create lags for tscs
addLags = function(toLag, data, idNames=ids, dvNames=dvs, ivNames=ivs){
	newData = do.call('cbind', lapply(toLag, function(toLagNumber){
		base = data[,c(idNames, dvNames, ivNames)]
		covData = data[,c(idNames, ivNames)]
		covData$year = num(covData$year) + toLagNumber
		covData$id = with(covData, paste(ccodeS, ccodeR, year, sep='_'))
		names(covData)[5:ncol(covData)] = paste0(
			names(covData)[5:ncol(covData)],'_',toLagNumber+1)
		for(v in setdiff(names(covData), names(base))){
			base$var = covData[match(base$id, covData$id), v]
			names(base)[ncol(base)] = v }
		return( base[,paste0(ivs,'_',toLagNumber+1)] ) }) )
	return(cbind(data, newData)) }

# 
iData = lapply(iData, function(x){
	# add lags
	x = addLags(1:5, x)
	# add dyadic id
	x$id = paste(x$ccodeS, x$ccodeR, sep='_')
	x$id = factor(x$id)
	# create total aid
	x$aidTotal = x$notHumanitarianTotal + x$humanitarianTotal
	# log dvs
	for(dv in c('aidTotal',dvs)){ x[,dv] = log(x[,dv] + 1) }
	return(x) })
save(iData, file=paste0(pathData, '/iDataDisagg_wLags.rda'))
################################################################

################################################################
# RE model

## mod formula
disVar = 'Lno_disasters'
cntrlVars=c(
	'colony' # Colonial variable
	,'Lpolity2' # Institutions
	,'LlnGdpCap' # Macroecon controls
	,'LlifeExpect' # Humanitarian
	,'Lcivwar' # Civil war
	)

# model spec gen
genModelForm = function(dv, var, type, struc, interaction = FALSE, disVar='Lno_disasters'){
	if(type=='re'){ strucChar=paste0(' + ', paste('(1|',struc, ')', collapse=' + ')) }
	if(type=='fe'){ strucChar=paste0(' + ', paste('factor(',struc,')',collapse=' + '), ' - 1') }
	if(type=='none'){ strucChar=NULL }

	if ( interaction == FALSE){
			form = formula(
			paste0(  dv, ' ~ ',  # DV
			paste(var, collapse=' + '), ' + ', # add key var
			paste(c(cntrlVars, disVar), collapse=' + '), # add control vars
			strucChar )  )}
	else if (interaction == TRUE){
			form = formula(
			paste0(  dv, ' ~ ',  # DV
			paste(c(var, paste(c(var, disVar), collapse = '*')), collapse=' + '), ' + ', # add key var
			paste(c(disVar, cntrlVars), collapse=' + '), # add control vars
			strucChar )  ) }
	return(form) }

# filename gen
genFileName = function(dv, train, mod, type, zeroInf, keyVar, trainEnd=2002, interaction = FALSE){
	a = ifelse(train, 'trainSamp', 'fullSamp')
	b = mod ; c = type ; d = if(zeroInf){ 'zi' } ; e = paste(keyVar, collapse='')
	f = ifelse(interaction, 'interaction', '' )
	g = paste0(dv, '_', paste(a,b,c,d,e,f, sep='_'), '.rda')
	return( gsub('__','_',g) ) }

# Run models in parallel across imputed datasets
runModelParallel = function(
	cores=detectCores(),
	dataList=iData, trainLogic=FALSE, trainEnd=2002, 
	modType='re', modFamily='gaussian', zeroInfLogic=FALSE, 
	depVar,
	keyRegVar='LstratMu', modStruc=c('id','year'), 
	int = FALSE, disVarName = 'Lno_disasters'
	){

	modForm = genModelForm(
		dv=depVar, var=keyRegVar, type=modType, struc=modStruc, interaction = int, disVar=disVarName)
	modName = genFileName(
		dv=depVar, train=trainLogic, mod=modFamily, 
		type=modType, zeroInf=zeroInfLogic, keyVar=keyRegVar, interaction = int)

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
	# save(mods, file=paste0(pathResults, '/', modName)) # Save output	
}
################################################################

################################################################
# # Run interaction models
# Full sample model, random effect, LstratMu, runs in a couple of minutes
kivDiffLags = c(paste0('LstratMu',c('',paste0('_',2:6))))
disDiffLags = c(paste0('Lno_disasters',c('',paste0('_',2:6))))
for(dv in dvs){
	for(i in 1:length(kivDiffLags)){
		runModelParallel(
			trainLogic=FALSE, modType='re', 
			keyRegVar=kivDiffLags[i], disVarName=disDiffLags[i], 
			depVar=dv, int = T) } }
################################################################

################################################################
# Compare to existing measures using two fold temporally cut cv
# Training, random effect, Lstratmu
for(dv in dvs){
	runModelParallel(trainLogic=TRUE, modType='re', depVar=dv, keyRegVar='LstratMu')}

# Training, random effect, LallyWt
for(dv in dvs){
	runModelParallel(trainLogic=TRUE, modType='re', depVar=dv, keyRegVar='LallyWt')}

# Training, random effect, LunIdPt
for(dv in dvs){
	runModelParallel(trainLogic=TRUE, modType='re', depVar=dv, keyRegVar='LunIdPt')}

# Training, random effect, Ligo
for(dv in dvs){
	runModelParallel(trainLogic=TRUE, modType='re', depVar=dv, keyRegVar='Ligo')}

# Training, random effect, LallyWt + LunIdPt + Ligo
for(dv in dvs){
	runModelParallel(trainLogic=TRUE, modType='re', depVar=dv, 
		keyRegVar=c('LallyWt', 'LunIdPt','Ligo'))}
################################################################

################################################################
# Run full models for alternate strategic interest variables
ivs = c('LallyWt', 'LunIdPt', 'Ligo')
for (iv in ivs){
	for(dv in dvs[-4]){
	runModelParallel(trainLogic=FALSE, modType='re', depVar=dv, keyRegVar=iv, int = T)}
}
################################################################