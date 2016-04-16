if(Sys.info()['user']=='s7m' | Sys.info()['user']=='janus829'){ source('~/Research/ForeignAid/RCode/setup.R') }
if(Sys.info()['user']=='cindycheng'){ source('~/Documents/Papers/ForeignAid/RCode/setup.R') }
pathGraphics = '~/Research/ForeignAid/Presentations/Graphics/'
################################################################
# Load reg data
setwd(pathData)
load('IData.rda')
# regData = ameliaRegData$imp$imp3
regData = data.frame(IData)

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

# Grouping factors for hierarchical framework
regData$year = factor(regData$year, levels=sort(unique(regData$year)))
regData$ccodeS = factor(regData$ccodeS)
# regData$ccodeS = interaction(regData$year, regData$ccodeS, drop = TRUE) 
regData$ccodeR = factor(regData$ccodeR)
# regData$ccodeR = interaction(regData$year, regData$ccodeS, regData$ccodeR, drop = TRUE) 
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
	'commitUSD13 ~ ', paste(vars, collapse=' + '), 
	# '+ factor(year) + factor(ccodeS) -1')) # Sender + year fixed effects
	# '+ factor(id) - 1')) # Dyad fixed effects
	# '+ factor(id) + factor(year) - 1')) # Dyad + year fixed effects
	# '+ (1|ccodeS) + (1|year)'))	# Sender + year random effects
	'+ (1|ccodeS) + (1|ccodeR)'))	# Sender + receiver random effects	
	# '+ (1|id)')) # Dyadic fixed effects
	# '+ (1|id) + (1|year)')) # Dyad + year random effects
	# '+ (1|year/ccodeS)'))	# Senders nested within years

# Various models in parallel
ziLog = rep(c('FALSE','TRUE'),4)
fam = rep(c('gaussian','gamma','nbinom','poisson'), each=2)
cl=makeCluster(8)
registerDoParallel(cl)
foreach(ii=1:length(fam), .packages=c('glmmADMB')) %dopar% {
	m=glmmadmb(modForm, data=regData, zeroInflation=ziLog[ii], family=fam[ii], extra.args="-ndi 100000")
	if(ziLog[ii]){ziName = '_zi'} else {ziName=''}
	save(m, file=paste0(pathResults, '/mod_', fam[ii], ziName, '.rda'))
}