if(Sys.info()['user']=='s7m' | Sys.info()['user']=='janus829'){
	source('~/Research/ForeignAid/RCode/setup.R') }
if(Sys.info()['user']=='cindycheng'){
	source('~/Documents/Papers/ForeignAid/RCode/setup.R') }
################################################################	

################################################################
# PCA variable
load(paste0(pathResults, '/PCA/PCA_FullData_allyIGOUN.rda'))
stratData=PCA_FullData$PCA_AllYrs; rm(list='PCA_FullData')

# component vars
gPth=paste0(pathResults, "/gbmeLatDist/")
load(paste0(gPth, 'allyWtDist.rda')) ; allyDist = data.frame(res)
load(paste0(gPth, 'igoDist.rda')) ; igoDist = data.frame(res)
load(paste0(gPth, 'unNewDist.rda')) ; unDist = data.frame(res)
load(paste0(pathData, '/cow_trade/trade.rda')) # returns `trade`
names(trade)[1:2] = paste0('ccode',1:2)
################################################################

################################################################
## Get monadic covariates from BuildCovarData.R
load(paste0(pathData, '/covData.rda'))

### Subset monadic covariates to relevant years set
vars=c(
	'fdiGdp', 'no_killed', 
	'no_injured', 'no_affected'
	)
covData=covData[,c('cyear', 'ccode','cname', 'year', vars)]
################################################################

################################################################
# Add ids to various frames
addIDs = function(x){
	x$id = with(x, paste0(ccode1, 9999, ccode2) ) ; x$id = num(x$id)
	x$idYr = with(x, paste0(id, year)) ; x$idYr = num(x$idYr)
	return(x) }
stratData = addIDs(stratData) ; allyDist = addIDs(allyDist)
igoDist = addIDs(igoDist) ; unDist = addIDs(unDist)
trade = addIDs(trade)

# merge dyad vars
stratData$allyDist = allyDist$allyDist[match(stratData$idYr, allyDist$idYr)]
stratData$igoDist = igoDist$igoDist[match(stratData$idYr, igoDist$idYr)]
stratData$unDist = unDist$unDist[match(stratData$idYr, unDist$idYr)]
stratData$trade = trade$trade[match(stratData$idYr, trade$idYr)]
stratData$trade = log(stratData$trade + 1)

# minor cleanup
names(stratData)[4:6]=paste0('strat',c('StratMu','StratUp','StratLo'))
################################################################

################################################################
# Create lagged variables, subset by time (>1974 & <2005), and merge
stratData=lagData(stratData, 
	'idYr', 'id', 
	names(stratData)[c(5:6,9:12)] )
covData=lagData(covData, 'cyear', 'ccode', vars)
 
# Subset datasets by time
stratData = stratData[stratData$year>1974 & stratData$year<=2005,]
covData = covData[covData$year>1974 & covData$year<=2005,]
################################################################

################################################################
# Merge datasets into main data file for analysis
load(paste0(pathData, "/iDataDisagg.rda"))

# Add extra strategic variable to each imputed result
stratData$id = with(stratData, paste(ccode1, ccode2, year, sep='_'))
iData = lapply(iData, function(x){
	for(v in names(stratData)[13:18]){
		x$tmp = stratData[match(x$id,stratData$id),v]
		names(x)[ncol(x)] = v }
	return(x) })

# Add receiver level covariates
iData = lapply(iData, function(x){
	recMatch = with(x, paste0(ccodeR, year))
	for(v in names(covData)[9:12]){
		x$tmp = covData[match(recMatch, covData$cyear),v]
		names(x)[ncol(x)] = v }
	return(x) })

# set NAs to 0 for emdat data since its in event format
iData = lapply(iData, function(x){
	x$Lno_killed[is.na(x$Lno_killed)] = 0
	x$Lno_injured[is.na(x$Lno_injured)] = 0
	x$Lno_affected[is.na(x$Lno_affected)] = 0
	return(x) })
################################################################	

################################################################	
save(iData, file = "iDataDisagg_v2.rda")
################################################################	