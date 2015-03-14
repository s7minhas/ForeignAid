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
	# '+ factor(year) + factor(ccodeS) -1')) # Sender + year fixed effects
	# '+ factor(id) - 1')) # Dyad fixed effects
	# '+ factor(id) + factor(year) - 1')) # Dyad + year fixed effects
	# '+ (1|ccodeS) + (1|year)'))	# Sender + year random effects
	# '+ (1|id)')) # Dyadic fixed effects
	'+ (1|id) + (1|year)')) # Dyad + year random effects
	# '+ (1|year/ccodeS)'))	# Senders nested within years

# mod=lm(modForm, data=regData) # fixed effects estimation
mod=lmer(modForm, data=regData) # random effects estimation

# Model results
summary(mod)$coefficients[1:(length(vars)+1),]
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
  ylabel="Log(Aid)$_{t}$", xlabel="Life Expectancy$_{t-1}$",
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
  ylabel="Log(Aid)$_{t}$", xlabel="No. Disasters$_{t-1}$",
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

#########################################################
# Revise model formula to incorporate interaction
modForm=formula(paste0(
	'logAid ~ ', paste(vars, collapse=' + '), 
	'+ LstratMu * Lno_disasters',
	# '+ LstratMu * LlnGdpCap',
	'+ (1|id) + (1|year)')) # Dyad + year random effects

# Rerun model	
mod=lmer(modForm, data=regData) # random effects estimation

# Peek @ Model results
summary(mod)$coefficients[1:(length(vars)+2),]
sqrt(mean( (resid(mod)^2) ))

# Create scenario matrix
stratRange=with(data=regData, seq(min(LstratMu), max(LstratMu), .01) )
disRange=with(data=regData, seq(min(Lno_disasters), max(Lno_disasters), 1) )
# gdpRange=with(data=regData, seq(min(LlnGdpCap), max(LlnGdpCap), .5) )
scen = with(data=regData, 
	expand.grid(1, stratRange, median(LmilMu), median(colony),
		median(Lpolity2), median(LlnGdpCap), median(LlifeExpect),
		disRange, median(Lcivwar) ) )
# scen = with(data=regData, 
# 	expand.grid(1, stratRange, median(LmilMu), median(colony),
# 		median(Lpolity2), gdpRange, median(LlifeExpect),
# 		median(Lno_disasters), median(Lcivwar) ) )

# Add interaction term
scen = cbind( scen, scen[,2]*scen[,8] )
# scen = cbind( scen, scen[,2]*scen[,6] )
colnames(scen) = colnames( coef(mod)$id )
scen = data.matrix(scen)
pred = scen %*% mod@beta
draws = mvrnorm(10000, mod@beta, vcov(mod))
sysUncert = scen %*% t(draws)
sysInts = t(apply(sysUncert, 1, function(x){ quantile(x, c(0.025, 0.975)) }))

# Combine for plotting
ggData=data.frame(
		cbind(pred, sysInts, scen[,'LstratMu'], scen[,'Lno_disasters'])
		# cbind(pred, sysInts, scen[,'LstratMu'], scen[,'LlnGdpCap'])
	)
names(ggData)=c('fit', 'sysLo', 'sysHi', 'LstratMu', 'Lno_disasters')
# names(ggData)=c('fit', 'sysLo', 'sysHi', 'LstratMu', 'LlnGdpCap')

# Make a surface plot
tmp=ggplot(ggData, aes(x=LstratMu, y=Lno_disasters, fill=fit)) 
tmp=tmp + xlab('Strategic Interest') + ylab('No. Disasters')
# tmp=ggplot(ggData, aes(x=LstratMu, y=LlnGdpCap, fill=fit)) 
# tmp=tmp + xlab('Strategic Interest') + ylab('Log(GDP capita)')
tmp=tmp + geom_tile(colour='darkgrey')
tmp=tmp + scale_fill_gradient2(midpoint=median(regData$logAid), 
	space='rgb', low="#d73027", mid="white", high="#4575b4", name='Log(Aid)\n')
tmp=tmp + scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0))
tmp=tmp + theme(axis.ticks=element_blank(), 
  legend.position='top', legend.key.width=unit(2,'cm'),
  panel.grid.major = element_blank(), panel.grid.minor = element_blank() )
tmp

# Plot rel at various cuts of disasters
disRange=with(data=regData, seq(min(Lno_disasters), max(Lno_disasters), 5) )
# gdpRange=with(data=regData, seq(min(LlnGdpCap), max(LlnGdpCap), 2) )
ggDataSmall = ggData[which(ggData$Lno_disasters %in% disRange),]
# ggDataSmall = ggData[which(ggData$LlnGdpCap %in% gdpRange),]
tmp=ggplot(ggDataSmall, aes(x=LstratMu, y=fit))
tmp=tmp + geom_line()
tmp=tmp + geom_ribbon(aes(ymin=sysLo, ymax=sysHi), alpha=.9)
tmp=tmp + facet_grid(~Lno_disasters)
# tmp=tmp + facet_grid(~LlnGdpCap)
tmp
#########################################################