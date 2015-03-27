if(Sys.info()['user']=='s7m' | Sys.info()['user']=='janus829'){ source('~/Research/ForeignAid/RCode/setup.R') }
if(Sys.info()['user']=='cindycheng'){ source('~/Documents/Papers/ForeignAid/RCode/setup.R') }
pathGraphics = '~/Research/ForeignAid/Presentations/Graphics/'
################################################################
# Load reg data
setwd(pathData)
load('regData.rda')
regData = ameliaRegData$imp$imp3

# Adjust covariates
regData$LmilMu = regData$LmilMu + abs(regData$LmilMu)

# Only include senders with at least n receivers in every year
regData$tmp=1
agg=summaryBy(tmp ~ ccodeS + year, data=regData, FUN=sum, na.rm=TRUE, keep.names=T)
toKeep=paste0(agg[which(agg$tmp>=10),1], agg[which(agg$tmp>=10),2])
regData = regData[which(regData$cyearS %in% toKeep),]

# Remove senders that only have datapoints for a certain time period
agg=summaryBy(tmp ~ ccodeS,
	data=unique(regData[,c('ccodeS', 'year', 'tmp')]), FUN=sum, na.rm=TRUE, keep.names=T)
toKeep=agg[which(agg$tmp>=7),1]
regData = regData[which(regData$ccodeS %in% toKeep),]

# Load other datasets
setwd(paste0(pathData, '/Components/COW_Alliances/version4.1_stata')); load('ally.rda')
setwd(paste0(pathData, '/Components/COW_IGO')); load('igo.rda')
setwd(paste0(pathData, '/Components/VoetenData')); load('unNew.rda')

# merge in ally, igo, and unnew
allianceFINAL$year = allianceFINAL$year-1
allianceFINAL = allianceFINAL[which(allianceFINAL$year %in% unique(regData$year)),]
allianceFINAL = allianceFINAL[which(allianceFINAL$ccode_1 %in% unique(regData$ccodeS)),]
allianceFINAL = allianceFINAL[which(allianceFINAL$ccode_2 %in% unique(regData$ccodeR)),]
allianceFINAL$idYr = num( paste0(
		allianceFINAL$ccode_1, 9999, 
		allianceFINAL$ccode_2, allianceFINAL$year) )
regData = merge(regData, allianceFINAL[,c('allyWtMax','idYr')], by='idYr', all.x=TRUE, all.y=FALSE)
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
	,'Lcivwar' # Civil war
	)

## Run model on full sample
### Results consistent across various specifications
modForm=formula(paste0(
	'logAid ~ ', paste(vars, collapse=' + '), 
	'+ (1|id) + (1|year)')) # Dyad + year random effects

# mod=lm(modForm, data=regData) # fixed effects estimation
mod=lmer(modForm, data=regData) # random effects estimation