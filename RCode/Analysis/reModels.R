if(Sys.info()["user"]=="janus829"){
	source("~/Research/ForeignAid/RCode/setup.R") }

################################################################
# Load reg data
setwd(pathData)
load('regData.rda')
regData = regData[regData$year>1974 & regData$year<=2006,]

# Only include senders with at least 20 recevier data point
## in every year
regData$tmp=1
agg=summaryBy(tmp ~ ccodeS + year, data=regData, FUN=sum, na.rm=TRUE, keep.names=T)
toKeep=paste0(agg[which(agg$tmp>=20),1], agg[which(agg$tmp>=20),2])
regData = regData[which(regData$cyearS %in% toKeep),]

# Remove senders that only have datapoints for a ten year period
agg=summaryBy(tmp ~ ccodeS,
	data=unique(regData[,c('ccodeS', 'year', 'tmp')]), FUN=sum, na.rm=TRUE, keep.names=T)
toKeep=agg[which(agg$tmp>=10),1]
regData = regData[which(regData$ccodeS %in% toKeep),]

# Grouping factors
regData$year = factor(regData$year, levels=sort(unique(regData$year)))
regData$ccodeS = factor(regData$ccodeS)
regData$ccodeS = interaction(regData$year, regData$ccodeS, drop = TRUE) 
regData$ccodeR = factor(regData$ccodeR)
regData$ccodeR = interaction(regData$year, regData$ccodeS, regData$ccodeR, drop = TRUE) 
################################################################

################################################################
# RE model
## mod formula
vars=c(
	'LstratMu', # state interest measure
	'colony' # Colonial variable
	,'Lpolity2' # Institutions
	,'LlnGdpCap' # Macroecon controls
	,'LlifeExpect', 'Lno_disasters' # Humanitarian
	)

## Run model on full sample
modForm=formula(paste0(
	'logAid ~ ', paste(vars, collapse=' + '), 
	'+ (LstratMu|year/ccodeS)'))

mod=lmer(modForm, data=regData)
summary(mod)
sqrt(mean( (resid(mod)^2) ))
###############################################################################

###############################################################################
# Plotting
ranCYrStrat=ranef(mod)$'ccodeS:year'
ids=unlist(lapply(strsplit(rownames(ranCYrStrat), '\\.'), function(x) x[2]))
ranCYrStrat$ccodeS=unlist(lapply(strsplit(ids, ':'), function(x) x[1]))
ranCYrStrat$year=unlist(lapply(strsplit(ids, ':'), function(x) x[2]))

par(mfrow=c(3,3))
for(cntry in unique(ranCYrStrat$ccodeS)){
	slice=ranCYrStrat[which(ranCYrStrat$ccodeS %in% cntry),]
	plot(slice$year, slice$LstratMu, type='l')
	abline(h=0, col='red', lty=2)
	title(paste0(cntry, ': ', unique(panel$cname[panel$ccode==cntry])))
}
par(mfrow=c(1,1))

ranYrStrat=ranef(mod)$'year'
ranYrStrat$year = rownames(ranYrStrat)
plot(ranYrStrat$year, ranYrStrat$LstratMu, type='l')
abline(h=0, col='red', lty=2)
###############################################################################