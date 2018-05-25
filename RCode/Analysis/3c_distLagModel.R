if(Sys.info()['user']=='s7m' | Sys.info()['user']=='janus829'){ source('~/Research/ForeignAid/RCode/setup.R') }
if(Sys.info()['user']=='cindycheng'){ source('~/Dropbox/Documents/Papers/ForeignAid1/RCode/setup.R') }

################################################################
# Load reg data
setwd(pathData)
load('iDataDisagg.rda')

# vars for analysis
dvs = c('humanitarianTotal', 'developTotal', 'civSocietyTotal')
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
################################################################

################################################################
## mod formula
kivs = c('LstratMu', 'Lno_disasters', 'LstratMu*Lno_disasters')
cntrlVars=c(
	'colony' # Colonial variable
	,'Lpolity2' # Institutions
	,'LlnGdpCap' # Macroecon controls
	,'LlifeExpect' # Humanitarian
	,'Lcivwar' # Civil war
	)

# vars
# kivDiffLags = c(paste0('LstratMu',c('',paste0('_',2:6))))
kivDiffLags = rep('LstratMu',6)
disDiffLags = c(paste0('Lno_disasters',c('',paste0('_',2:6))))
intDiffLags = apply(
	matrix(c(kivDiffLags, disDiffLags),ncol=2), 1, 
	function(x){ paste0(x[1],'*',x[2]) })

# run dl models
mods = lapply(dvs, function(dv){
	lapply(iData, function(x){
		modForm = formula(
			paste0(dv, '~',
				paste(
					c( kivDiffLags,  disDiffLags,
						intDiffLags, cntrlVars ),
					collapse=' + '),
				' + (1|id) + (1|year)' ) )
		mod = lmer(modForm, data=x)
		return(mod) }) })
modSumm = lapply(1:length(dvs), function(i){
	return(rubinCoef(mods[[i]])) })
names(modSumm) = dvs

specResults = lapply(1:length(modSumm), function(i){
	print(names(modSumm)[i])
	x = modSumm[[i]]
	# print(x[2:7,])
	# print(x[8:13,])
	# print(x[19:24,])

	print(x[2,,drop=FALSE])
	print(x[3:8,])
	print(x[14:19,])
})
################################################################