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
################################################################

################################################################
# RE model
## mod formula
vars=c(
	'LstratMu', # state interest measure
	# 'colony', # Colonial variable
	'Lpolity2', # Institutions
	'LlnGdpCap', # Macroecon controls
	'LlifeExpect', 'Lno_disasters', # Humanitarian
	'Lcivwar'
	)

## Run model on full sample
modForm=formula(paste0(
	'logAid ~ ', paste(vars, collapse=' + '), 
	# ' + (LstratMu|year/ccodeS)' ))
	' + (LstratMu + colony|year) + (LstratMu + SLlnGdpCap + SLpolity2|ccodeS)' ))

mod=lmer(modForm, data=regData)
summary(mod)
ranef(mod)
sqrt(mean( (resid(mod)^2) ))
###############################################################################

###############################################################################
# Plotting

###############################################################################
