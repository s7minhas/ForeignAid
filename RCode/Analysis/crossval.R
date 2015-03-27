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

# # Grouping factors for hierarchical framework
# regData$year = factor(regData$year, levels=sort(unique(regData$year)))
# regData$ccodeS = factor(regData$ccodeS)
# regData$ccodeS = interaction(regData$year, regData$ccodeS, drop = TRUE) 
# regData$ccodeR = factor(regData$ccodeR)
# regData$ccodeR = interaction(regData$year, regData$ccodeS, regData$ccodeR, drop = TRUE) 
################################################################

################################################################
# RE model

## mod formula
vars=c(
	'LstratMu', # state interest measure
	# 'LmilMu', # military interest measure
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
	'+ (1|ccodeS)'))	# Sender + year random effects

yrs = sort(unique(regData$year))
coefData=NULL
for(ii in seq_along(yrs)){
	slice = regData[regData$year==yrs[ii],]
	mod=lmer(modForm, data=slice) # random effects estimation	
	coefs = summary(mod)$coefficients[1:(length(vars)+1),]
	coefs = cbind(Year=yrs[ii], coefs)
	coefData = rbind(coefData, coefs)
	print(yrs[ii])
}	

# Coefficient plot
varNames = c(
	'Pol. Strat. Distance$_{sr,t-1}$',
	# 'Mil. Strat. Distance$_{sr,t-1}$',
	'Former Colony$_{sr,t-1}$',
	'Polity$_{r,t-1}$',
	'Log(GDP per capita)$_{r,t-1}$',
	'Life Expectancy$_{r,t-1}$',
	'No. Disasters$_{r,t-1}$',
	'Civil War$_{r,t-1}$'
	)

ggcoefplot(coefData=coefData[coefData[,'Year']>1976,], 
	vars=vars, varNames=varNames,
	Noylabel=FALSE, coordFlip=FALSE, revVar=FALSE,
	facet=TRUE, facetColor=FALSE, colorGrey=FALSE,
	facetName='Year', 
	facetBreaks=seq(yrs[1],yrs[length(yrs)],5),
	facetLabs=seq(yrs[1],yrs[length(yrs)],5)
  )