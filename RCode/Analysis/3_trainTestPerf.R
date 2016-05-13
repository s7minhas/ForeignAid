if(Sys.info()['user']=='s7m' | Sys.info()['user']=='janus829'){ source('~/Research/ForeignAid/RCode/setup.R') }
if(Sys.info()['user']=='cindycheng'){ source('~/Documents/Papers/ForeignAid/RCode/setup.R') }

################################################################
# Load reg data

load(paste0(pathData, '/noImputationData.rda'))
regData$commitUSD13 = log(regData$commitUSD13 + 1)
xOut = regData[,c('commitUSD13', 
	'colony' ,'Lpolity2','LlnGdpCap','LlifeExpect', 'Lno_disasters','Lcivwar',
	'LstratMu', 'LallyWt', 'LunIdPt', 'Ligo',
	'ccodeS', 'cnameS', 'ccodeR', 'cnameR','year'
	)] %>% na.omit
xOut$id = paste(xOut$ccodeS, xOut$ccodeR, sep='_')
xOut$id = factor(xOut$id)
xOut = xOut[xOut$year>=2002,]

load(paste0(pathData,'/iData.rda'))
# Add dyad random effect
iData = lapply(iData, function(x){
	x = x[,c('commitUSD13', 
		'colony' ,'Lpolity2','LlnGdpCap','LlifeExpect', 'Lno_disasters','Lcivwar',
		'LstratMu', 'LallyWt', 'LunIdPt', 'Ligo',
		'ccodeS', 'ccodeR', 'year'
		)]
	# add dyadic id
	x$id = paste(x$ccodeS, x$ccodeR, sep='_')
	x$id = factor(x$id)
	# log aid flow
	x$commitUSD13 = log(x$commitUSD13 + 1)
	x$year = num(x$year)
	x = x[x$year>=2002,]	
	return(x)
	})


# Load model results
toLoad=list.files(pathResults)[grepl('trainSamp', list.files(pathResults))]
for(out in toLoad){ load(paste0(pathResults, '/', out)) ; assign(gsub('.rda','',out), mods) ; rm(list='mods') }
################################################################

################################################################
# Meld parameter estimates from each and calc out of sample perf
rubinCoef = function(mod){
	modCoef = lapply(mod, function(x){
		beta = fixef(x)
		se = sqrt(diag(vcov(x)))
		return( cbind(beta, se) )
		}) %>% do.call('rbind',.) 

	modSumm = mi.meld(
		q=matrix(modCoef[,1],ncol=length(unique(rownames(modCoef))), byrow=TRUE), 
		se=matrix(modCoef[,2],ncol=length(unique(rownames(modCoef))), byrow=TRUE), 
		byrow=TRUE) %>% lapply(., t) %>% do.call('cbind',.) %>% data.frame(.)

	names(modSumm) = c('beta', 'se')
	modSumm$t = modSumm$beta/modSumm$se
	modSumm$var = unique(rownames(modCoef))
	return(modSumm)
}

getRMSE = function(rCoef){
	pred = t(rCoef$beta %*% t(cbind(1,xOut[,rCoef$var[-1]])))
	(pred - xOut$commitUSD13)^2 %>% mean(.) %>% sqrt(.)	
}

getRMSE(rubinCoef(trainSamp_gaussian_re_LstratMu))
getRMSE(rubinCoef(trainSamp_gaussian_re_LallyWt))
getRMSE(rubinCoef(trainSamp_gaussian_re_LunIdPt))
getRMSE(rubinCoef(trainSamp_gaussian_re_Ligo))
getRMSE(rubinCoef(trainSamp_gaussian_re_LallyWtLunIdPtLigo))

getRMSE_wRE = function(ii, mod, data){
	fromfn = predict(object=mod[[ii]], newdata=data[[ii]], allow.new.levels=TRUE, re.form=~(1|id))
	(fromfn - data[[ii]]$commitUSD13)^2 %>% mean(.) %>% sqrt(.)
}

sort( unlist( lapply(1:length(trainSamp_gaussian_re_LstratMu), function(x){getRMSE_wRE(x, trainSamp_gaussian_re_LstratMu, iData)}) ) )
sort( unlist( lapply(1:length(trainSamp_gaussian_re_LallyWt), function(x){getRMSE_wRE(x, trainSamp_gaussian_re_LallyWt, iData)}) ) )
sort( unlist( lapply(1:length(trainSamp_gaussian_re_LunIdPt), function(x){getRMSE_wRE(x, trainSamp_gaussian_re_LunIdPt, iData)}) ) )
sort( unlist( lapply(1:length(trainSamp_gaussian_re_Ligo), function(x){getRMSE_wRE(x, trainSamp_gaussian_re_Ligo, iData)}) ) )
sort( unlist( lapply(1:length(trainSamp_gaussian_re_LallyWtLunIdPtLigo), function(x){getRMSE_wRE(x, trainSamp_gaussian_re_LallyWtLunIdPtLigo, iData)}) ) )
################################################################