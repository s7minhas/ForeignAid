if(Sys.info()['user']=='s7m' | Sys.info()['user']=='janus829'){
	source('~/Research/ForeignAid/RCode/setup.R') }
if(Sys.info()['user']=='cindycheng'){
	source('~/Documents/Papers/ForeignAid/RCode/setup.R') }
################################################################

################################################################
# Load reg data
load(paste0(pathData, "/iDataDisagg_v3.rda"))
 
# vars for analysis
dvs = c(
	'humanitarianTotal', 'developTotal', 
	'civSocietyTotal' 
	)
ids = c("year", "ccodeS", "ccodeR", 'id')
ivs = c(
	"LstratMu", 
	#'LstratStratUp', 'LstratStratLo',
	#"LallyWt", "Ligo", "LunIdPt",
	#'LallyDist', 'LigoDist', 'LunDist', 
	#'Ltrade', 
	"colony", 
	"Lpolity2", "LlnGdpCap", "LlifeExpect", "Lcivwar",	
	"Lno_disasters", 'Lno_killed', 'Ltotal_affected', 'Ltotal_dam'
	)
kivs = c(
	"Lno_disasters",  'Lno_killed', 'Ltotal_affected', 'Ltotal_dam'
	)

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


lapply(iData, function(x){setdiff(c(ids, dvs, ivs), names(x))})
if(!file.exists(paste0(pathData, '/iDataDisagg_wLags_v3.rda'))){
	iData = lapply(iData, function(x){
		# add lags
		# x = addLags(1:5, x)
		# add dyadic id
		x$id = paste(x$ccodeS, x$ccodeR, sep='_')
		x$id = factor(x$id)
		# create total aid
		#x$aidTotal = x$notHumanitarianTotal + x$humanitarianTotal
		#log dvs
		for(dv in c(dvs)){ x[,dv] = log(x[,dv] + 1) }

		return(x) })
	save(iData, file=paste0(pathData, '/iDataDisagg_wLags_v3.rda'))
} else {
	load(paste0(pathData, '/iDataDisagg_wLags_v3.rda'))
}

################################################################

################################################################
# RE model

humMod = lmer(
	humanitarianTotal ~ 
		LstratMu + Lno_disasters + LstratMu * Lno_disasters +
		colony + Lpolity2 + LlnGdpCap + LlifeExpect + Lcivwar + 
		(1|ccodeS) + (1|ccodeR) + (1|year), 
	data=iData[[1]] 
	)

civMod = lmer(
	civSocietyTotal ~ 
		LstratMu + Lno_disasters + LstratMu * Lno_disasters +
		colony + Lpolity2 + LlnGdpCap + LlifeExpect + Lcivwar + 
		(1|ccodeS) + (1|ccodeR) + (1|year), 
	data=iData[[1]] 
	)

devMod = lmer(
	developTotal ~ 
		LstratMu + Lno_disasters + LstratMu * Lno_disasters +
		colony + Lpolity2 + LlnGdpCap + LlifeExpect + Lcivwar + 
		(1|ccodeS) + (1|ccodeR) + (1|year), 
	data=iData[[1]] 
	)

humModFE = lm(
	humanitarianTotal ~ 
		LstratMu + Lno_disasters + LstratMu * Lno_disasters +
		colony + Lpolity2 + LlnGdpCap + LlifeExpect + Lcivwar + 
		factor(ccodeS) + factor(ccodeR) + factor(year) - 1, 
	data=iData[[1]]
	)

civModFE = lm(
	civSocietyTotal ~ 
		LstratMu + Lno_disasters + LstratMu * Lno_disasters +
		colony + Lpolity2 + LlnGdpCap + LlifeExpect + Lcivwar + 
		factor(ccodeS) + factor(ccodeR) + factor(year) - 1, 
	data=iData[[1]]
	)

devModFE = lm(
	developTotal ~ 
		LstratMu + Lno_disasters + LstratMu * Lno_disasters +
		colony + Lpolity2 + LlnGdpCap + LlifeExpect + Lcivwar + 
		factor(ccodeS) + factor(ccodeR) + factor(year) - 1, 
	data=iData[[1]]
	)

round(
	summary(humMod)$'coefficients'[
	c(2:3,nrow(summary(humMod)$'coefficients')),],
	3)
round(
	summary(humModFE)$'coefficients'[
	c(1:2,nrow(summary(humModFE)$'coefficients')),],
	3)

round(
	summary(civMod)$'coefficients'[
	c(2:3,nrow(summary(civMod)$'coefficients')),],
	3)
round(
	summary(civModFE)$'coefficients'[
	c(1:2,nrow(summary(civModFE)$'coefficients')),],
	3)

round(
	summary(devMod)$'coefficients'[
	c(2:3,nrow(summary(devMod)$'coefficients')),],
	3)
round(
	summary(devModFE)$'coefficients'[
	c(1:2,nrow(summary(devModFE)$'coefficients')),],
	3)