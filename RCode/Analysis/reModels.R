if(Sys.info()['user']=='s7m' | Sys.info()['user']=='janus829'){ source('~/Research/ForeignAid/RCode/setup.R') }
if(Sys.info()['user']=='cindycheng'){ source('~/Documents/Papers/ForeignAid/RCode/setup.R') }

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

# Grouping factors for hierarchical framework
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
	'LmilMu', # military interest measure
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
	# '+ factor(year) + factor(ccodeS)')) # Sender + year fixed effects
	# '+ factor(id) + factor(ccodeS) - 1')) # Dyadic + year fixed effecs
	# '+ (1|ccodeS) + (1|year)'))	# Sender + year random effects
	'+ (1|id) + (1|year)')) # Dyad + year random effects
	# '+ (1|year/ccodeS)'))	# Senders nested within years

# mod=lm(modForm, data=regData) # fixed effects estimation
mod=lmer(modForm, data=regData) # random effects estimation

# Model results
summary(mod)$coefficients[1:(length(vars)+1),]
# Quick performance glimpse
sqrt(mean( (resid(mod)^2) ))
#########################################################

#########################################################
# Substantive effects
## Strategic interest
stratEffect = ggsimplot(modelResults=mod, sims=10000, simData=regData, 
  vars=vars, actual=FALSE, brk=0.01, 
  vi='LstratMu', ostat=median, sigma=FALSE, intercept=TRUE,
  ylabel="Log(Aid)$_{t}$", xlabel="Strategic Distance$_{t-1}$",
  plotType='ribbon'
  )
stratEffect=stratEffect + theme(axis.title.y=element_text(vjust=1))
stratEffect

# summary(exp(ggData$Fit))
# c( 672600 - 5248000  )/5248000 : -87%

## Life expectancy
lifeEffect = ggsimplot(modelResults=mod, sims=10000, simData=regData, 
  vars=vars, actual=FALSE, brk=0.1, 
  vi='LlifeExpect', ostat=median, sigma=FALSE, intercept=TRUE,
  ylabel="Log(Aid)$_{t}$", xlabel="Strategic Distance$_{t-1}$",
  plotType='ribbon'
  )
lifeEffect=lifeEffect + theme(axis.title.y=element_text(vjust=1))
lifeEffect
# summary(exp(ggData$Fit))
# c( 2822000 -1397000 )/1397000 : 102%

## Natural disaster 
disastEffect = ggsimplot(modelResults=mod, sims=10000, simData=regData, 
  vars=vars, actual=FALSE, brk=1, 
  vi='Lno_disasters', ostat=median, sigma=FALSE, intercept=TRUE,
  ylabel="Log(Aid)$_{t}$", xlabel="Strategic Distance$_{t-1}$",
  plotType='errorBar'
  )
disastEffect=disastEffect + theme(axis.title.y=element_text(vjust=1))
disastEffect

#summary(exp(ggData$Fit))
#c(125700000 - 1671000    )/ 1671000 : 742%
#########################################################

#########################################################
# cross correlation functions

modData = model.frame(mod)
sender = unique(modData$ccodeS)

par(mfrow = c(2, 3)) # wanted to put all the plots on same page but just too small to interpret
for ( i in 13:18){
ccf( modData$Lno_disasters[which(modData$ccodeS == sender[i])], modData$logAid[which(modData$ccodeS== sender[i])])
}

for ( i in 1:6){
ccf(modData$LstratMu[which(modData$ccodeS == sender[i])], modData$logAid[which(modData$ccodeS== sender[i])])
}

for ( i in 1:6){
ccf( modData$LlifeExpect[which(modData$ccodeS == sender[i])], modData$logAid[which(modData$ccodeS== sender[i])])
}
 

 
#########################################################