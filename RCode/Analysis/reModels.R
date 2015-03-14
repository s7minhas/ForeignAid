if(Sys.info()['user']=='s7m' | Sys.info()['user']=='janus829'){ source('~/Research/ForeignAid/RCode/setup.R') }
if(Sys.info()['user']=='cindycheng'){ source('~/Documents/Papers/ForeignAid/RCode/setup.R') }

################################################################
# Load reg data
setwd(pathData)
load('regData.rda')
regData = ameliaRegData$imp$imp3

# Adjust covariates
regData$LmilMu = regData$LmilMu + abs(regData$LmilMu)

# Only include senders with at least n recevier
## in every year
regData$tmp=1
agg=summaryBy(tmp ~ ccodeS + year, data=regData, FUN=sum, na.rm=TRUE, keep.names=T)
toKeep=paste0(agg[which(agg$tmp>=10),1], agg[which(agg$tmp>=10),2])
regData = regData[which(regData$cyearS %in% toKeep),]

# Remove senders that only have datapoints for a certain time period
agg=summaryBy(tmp ~ ccodeS,
	data=unique(regData[,c('ccodeS', 'year', 'tmp')]), FUN=sum, na.rm=TRUE, keep.names=T)
toKeep=agg[which(agg$tmp>=7),1]
regData = regData[which(regData$ccodeS %in% toKeep),]

# Grouping factors
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
modForm=formula(paste0(
	'logAid ~ ', paste(vars, collapse=' + '), 
	# '+ factor(year) + factor(ccodeS)'))		
	'+ (1|ccodeS) + (1|year)'))	
	# '+ (1|year/ccodeS)'))	
	# '+ (LstratMu|year/ccodeS)'))
	# '+ (LstratMu + LmilMu|year/ccodeS)'))
	# '+ (', paste(vars, collapse=' + '), '|year/ccodeS)'))

# mod=lm(modForm, data=regData)
mod=lmer(modForm, data=regData)
summary(mod)
sqrt(mean( (resid(mod)^2) ))
# Save model results
setwd(pathResults)
# save(mod, file='mod_fixef.rda')
# save(mod, file='mod_None.rda')
# save(mod, file='mod_ranStrat.rda')
# save(mod, file='mod_ranStratMil.rda')
# save(mod, file='mod_ranAll.rda')
#########################################################

#########################################################
# Substantive effects
# strategic interest
set.seed(2)
strat=seq(min(regData$LstratMu), max(regData$LstratMu), .1)
# strat = sort(regData$LstratMu)
scen = with(data=regData, expand.grid(1, strat, mean(LmilMu), 
	median(colony), mean(Lpolity2), mean(LlnGdpCap), 
	mean(LlifeExpect), median(Lno_disasters), median(Lcivwar) ) )
colnames(scen)=c('(Intercept)', vars)

# Simulations
sims=1000
betas=summary(mod)$coefficients[,1]
varCov=vcov(mod)
betaDraws = mvrnorm(sims, betas, varCov)
preds = betaDraws %*% t(scen)

# Data for plotting
ggData = t(apply(preds, 2, 
   function(x){ mean = mean(x) ;
     qlo95 = quantile(x, 0.025) ; qhi95 = quantile(x, 0.975) ;
     qlo90 = quantile(x, 0.05) ; qhi90 = quantile(x, 0.95) ;
     rbind(mean, qlo95, qhi95, qlo90, qhi90) } ))
ggData = data.frame(ggData)
colnames(ggData) = c('Fit', 'Lo95', 'Hi95', 'Lo90', 'Hi90')
ggData$x=strat

# Plot
tmp = ggplot(ggData, aes(x=x, y=Fit, ymin=Lo95, ymax=Hi95))
tmp = tmp + geom_line() + geom_ribbon(alpha=0.3)
tmp = tmp + geom_ribbon(aes(ymin=Lo90, ymax=Hi90), alpha=0.5)
# tmp = tmp + geom_rug(sides='b', position='jitter')
tmp

# summary(exp(ggData$Fit))
# c( 672600 - 5248000  )/5248000 : -87%

########################################################
# life expectancy
set.seed(2)
life=seq(min(regData$LlifeExpect), max(regData$LlifeExpect), .1)
# strat = sort(regData$LstratMu)
scen = with(data=regData, expand.grid(1, mean(LstratMu), mean(LmilMu), 
	median(colony), mean(Lpolity2), mean(LlnGdpCap), 
	life,  median(Lno_disasters) , median(Lcivwar) ) )
colnames(scen)=c('(Intercept)', vars)

# Simulations
sims=1000
betas=summary(mod)$coefficients[,1]
varCov=vcov(mod)
betaDraws = mvrnorm(sims, betas, varCov)
preds = betaDraws %*% t(scen)

# Data for plotting
ggData = t(apply(preds, 2, 
   function(x){ mean = mean(x) ;
     qlo95 = quantile(x, 0.025) ; qhi95 = quantile(x, 0.975) ;
     qlo90 = quantile(x, 0.05) ; qhi90 = quantile(x, 0.95) ;
     rbind(mean, qlo95, qhi95, qlo90, qhi90) } ))
ggData = data.frame(ggData)
colnames(ggData) = c('Fit', 'Lo95', 'Hi95', 'Lo90', 'Hi90')
ggData$x=life

# Plot
tmp = ggplot(ggData, aes(x=x, y=Fit, ymin=Lo95, ymax=Hi95))
tmp = tmp + geom_line() + geom_ribbon(alpha=0.3)
tmp = tmp + geom_ribbon(aes(ymin=Lo90, ymax=Hi90), alpha=0.5)
# tmp = tmp + geom_rug(sides='b', position='jitter')
 
tmp
# summary(exp(ggData$Fit))
# c( 2822000 -1397000 )/1397000 : 102%
 
########################################################
# natural disaster
set.seed(2) 
disast=seq(min(regData$Lno_disasters), max(regData$Lno_disasters), .1)
# strat = sort(regData$LstratMu)
scen = with(data=regData, expand.grid(1, mean(LstratMu), mean(LmilMu), 
	median(colony), mean(Lpolity2), mean(LlnGdpCap), 
	mean(LlifeExpect),  disast , median(Lcivwar) ) )
colnames(scen)=c('(Intercept)', vars)

# Simulations
sims=1000
betas=summary(mod)$coefficients[,1]
varCov=vcov(mod)
betaDraws = mvrnorm(sims, betas, varCov)
preds = betaDraws %*% t(scen)

# Data for plotting
ggData = t(apply(preds, 2, 
   function(x){ mean = mean(x) ;
     qlo95 = quantile(x, 0.025) ; qhi95 = quantile(x, 0.975) ;
     qlo90 = quantile(x, 0.05) ; qhi90 = quantile(x, 0.95) ;
     rbind(mean, qlo95, qhi95, qlo90, qhi90) } ))
ggData = data.frame(ggData)
colnames(ggData) = c('Fit', 'Lo95', 'Hi95', 'Lo90', 'Hi90')
ggData$x=disast

# Plot
tmp = ggplot(ggData, aes(x=x, y=Fit, ymin=Lo95, ymax=Hi95))
tmp = tmp + geom_line() + geom_ribbon(alpha=0.3)
tmp = tmp + geom_ribbon(aes(ymin=Lo90, ymax=Hi90), alpha=0.5)
# tmp = tmp + geom_rug(sides='b', position='jitter')
tmp

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
# Plotting
ranCYrStrat=ranef(mod)$'ccodeS:year'
# ids=unlist(lapply(strsplit(rownames(ranCYrStrat), '\\.'), function(x) x[2]))
ids=rownames(ranCYrStrat)
ranCYrStrat$ccodeS=unlist(lapply(strsplit(ids, ':'), function(x) x[1]))
ranCYrStrat$year=unlist(lapply(strsplit(ids, ':'), function(x) x[2]))

par(mfrow=c(3,3))
for(cntry in unique(ranCYrStrat$ccodeS)){
	slice=ranCYrStrat[which(ranCYrStrat$ccodeS %in% cntry),]
	plot(slice$year, slice$LstratMu, type='l')
	# lines(slice$year, slice$LmilMu, lty=2)
	abline(h=0, col='red', lty=2)
	title(paste0(cntry, ': ', unique(panel$cname[panel$ccode==cntry])))
}
par(mfrow=c(1,1))

ranYrStrat=ranef(mod)$'year'
ranYrStrat$year = rownames(ranYrStrat)
plot(ranYrStrat$year, ranYrStrat$LstratMu, type='l')
# lines(ranYrStrat$year, ranYrStrat$LmilMu, lty=2)
abline(h=0, col='red', lty=2)
#########################################################