if(Sys.info()['user']=='s7m' | Sys.info()['user']=='janus829'){ source('~/Research/ForeignAid/RCode/setup.R') }
if(Sys.info()['user']=='cindycheng'){ source('~/Documents/Papers/ForeignAid/RCode/setup.R') }
################################################################

################################################################
# load raw data
load(paste0(pathData, '/iDataDisagg.rda'))
testData = iData[[1]]
testData$year = num(testData$year)
################################################################

################################################################
# loop over dvs and stratvar combinations
dvs = c('humanitarianTotal', 'developTotal', 'civSocietyTotal')
dvNames = paste(c('Humanitarian', 'Development', 'Civil Society'), 'Aid')
stratVars = c(
	'LallyWtLunIdPtLigo',
	'LallyWt',
	'Ligo',
	'LstratMu',
	'LunIdPt'
	)
testData = testData[which(testData$year %in% 2003:2005),]
rmseScores = lapply(dvs, function(dv){
	stratRMSEs = lapply(stratVars, function(x){

		# load model for specific strat var
		pth = paste0(pathResults, '/', dv, '_trainSamp_gaussian_re_',x,'_.rda')
		load(pth) # returns mod to workspace

		# generated predicted vals
		mod = mods[[1]]
		predData = cbind(1, testData[,names(fixef(mod))[-1]])
		predVals = data.matrix(predData ) %*% fixef(mod) 

		# calc rmse
		y = log(testData[,dv] + 1)
		rmse = sqrt(mean((predVals - y)^2))
		return(rmse) })
	names(stratRMSEs) = stratVars
	return(unlist(stratRMSEs))
})
names(rmseScores) = dvs

rmseScores = do.call('cbind', rmseScores)
rmseScores = data.frame(rmseScores)
rmseScores$stratVar = rownames(rmseScores)

ggData = melt(rmseScores, id='stratVar')
ggplot(ggData, aes(x=stratVar, y=value)) +
	geom_linerange(aes(ymin=0, ymax=value)) +
	facet_wrap(~variable, nrow=3, scales='free_y')
################################################################