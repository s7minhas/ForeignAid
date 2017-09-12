if(Sys.info()['user']=='s7m' | Sys.info()['user']=='janus829'){ source('~/Research/ForeignAid/RCode/setup.R') }
if(Sys.info()['user']=='cindycheng'){ source('~/Documents/Papers/ForeignAid/RCode/setup.R') }
################################################################

################################################################
# load data
load(paste0(pathData, '/noImputationData.rda'))
load(paste0(pathData, '/iData_v2.rda'))

# load model
fullSampPath = paste0(pathResults, '/fullSamp_gaussian_re_')
load(paste0(fullSampPath, 'LallyDist.rda')) ; allyMods = mods
load(paste0(fullSampPath, 'LigoDist.rda')) ; igoMods = mods
load(paste0(fullSampPath, 'LunDist.rda')) ; unMods = mods
load(paste0(fullSampPath, 'LstratMu.rda')) ; stratMuMods = mods

load(paste0(fullSampPath, 'LallyDist_interaction.rda')) ; allyIntMods = mods
load(paste0(fullSampPath, 'LigoDist_interaction.rda')) ; igoIntMods = mods
load(paste0(fullSampPath, 'LunDist_interaction.rda')) ; unIntMods = mods
load(paste0(fullSampPath, 'LunDist_interaction.rda')) ; unIntMods = mods
load(paste0(fullSampPath, 'LstratMu_interaction.rda')) ; stratMuIntMods = mods ; rm(mods)

# 
allMods = list(
  ally=allyMods,igo=igoMods,un=unMods, stratMu=stratMuMods,
  allyInt=allyIntMods,igoInt=igoIntMods,unInt=unIntMods, stratMuInt=stratMuIntMods )
modSumm = lapply(allMods, rubinCoef)

# vars
cntrlVars=c(
  'Lno_disasters', 'colony', 'Lpolity2',
  'LlnGdpCap', 'LlifeExpect', 'Lcivwar' )
cntrlVarNames = c(
  'No. Disasters$_{r,t-1}$',    
  'Former Colony$_{sr,t-1}$',
  'Polity$_{r,t-1}$',
  'Log(GDP per capita)$_{r,t-1}$',
  'Life Expectancy$_{r,t-1}$',
  'Civil War$_{r,t-1}$'
  )
################################################################

################################################################
# Model results
genCoefPlot = function(mod, meltImpute=TRUE,
  vars, varNames, dropIndex=1, printPlot=FALSE, fName){

  # Coefficient plot
  if(!meltImpute){coefData=summary(mod)$coefficients}
  if(meltImpute){coefData=rubinCoef(mod, matrixFormat=TRUE)}
  coefP=ggcoefplot(coefData, vars[-dropIndex], varNames[-dropIndex])
  if(printPlot){
    tikz(file=fName, width=8, height=5, standAlone=F)
    coefP
    dev.off() }
  if(!printPlot){ return(coefP) } }


# no interaction models
varNamesNoInt = c('(Intercept)','Pol. Strat. Distance$_{sr,t-1}$', cntrlVarNames)

genCoefPlot(unMods,
  vars=c('(Intercept)','LunDist', cntrlVars),
  varNames=varNamesNoInt,
  fName = paste0(pathGraphics, 'unModCoef.tex') )

# interaction models
varNamesInt = c( '(Intercept)', 'Pol. Strat. Distance$_{sr,t-1}$', cntrlVarNames, 
  'Pol. Strat. Distance$_{sr,t-1}$ \n x No. Disasters$_{r,t-1}$')

genCoefPlot(allyIntMods,
  vars=c('(Intercept)','LallyDist', cntrlVars, 'LallyDist:Lno_disasters'),
  varNames=varNamesInt,
  fName = paste0(pathGraphics, 'allyModIntCoef.tex') )

genCoefPlot(igoIntMods,
  vars=c('(Intercept)','LigoDist', cntrlVars, 'LigoDist:Lno_disasters'),
  varNames=varNamesInt,
  fName = paste0(pathGraphics, 'igoModIntCoef.tex') )

genCoefPlot(unIntMods,
  vars=c('(Intercept)','LunDist', cntrlVars, 'LunDist:Lno_disasters'),
  varNames=varNamesInt,
  fName = paste0(pathGraphics, 'unModIntCoef.tex') )
#########################################################

#########################################################
# Substantive effects
## Strategic interest
stratEffect = ggsimplot(modelResults=mod, sims=10000, simData=regData, 
  vars=, actual=FALSE, brk=0.01, 
  vi='LallyDist', ostat=median, sigma=FALSE, intercept=TRUE,
  ylabel="Log(Aid)$_{t}$", xlabel="Strategic Distance$_{t-1}$",
  plotType='ribbon'
  )
stratEffect=stratEffect + theme(axis.title.y=element_text(vjust=1))
tikz(file=paste0(pathGraphics, 'stratEffect.tex'), width=8, height=5, standAlone=F)
stratEffect
dev.off()

## Natural disaster 
disastEffect = ggsimplot(modelResults=mod, sims=10000, simData=regData, 
  vars=vars, actual=FALSE, brk=1, vRange=1:7,
  vi='Lno_disasters', ostat=median, sigma=FALSE, intercept=TRUE,
  ylabel="Log(Aid)$_{t}$", xlabel="No. Disasters$_{t-1}$",
  plotType='errorBar'
  )
disastEffect=disastEffect + theme(axis.title.y=element_text(vjust=1))
tikz(file=paste0(pathGraphics, 'disastEffect.tex'), width=8, height=5, standAlone=F)
disastEffect
dev.off()

tikz(file=paste0(pathGraphics, 'effects.tex'), width=8, height=4, standAlone=F)
multiplot(list(stratEffect, disastEffect), cols=2)
dev.off()

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

## GDP per capita
gdpQts = quantile(regData$LlnGdpCap, probs=c(0.25, 0.75))
gdpRange = seq(gdpQts[1], gdpQts[2], .1)
gdpEffect = ggsimplot(modelResults=mod, sims=10000, simData=regData, 
  vars=vars, actual=FALSE, brk=0.1, vRange=gdpRange,
  vi='LlnGdpCap', ostat=median, sigma=FALSE, intercept=TRUE,
  ylabel="Log(Aid)$_{t}$", xlabel="Log(GDP per capita)$_{t-1}$",
  plotType='ribbon'
  )
gdpEffect=gdpEffect + theme(axis.title.y=element_text(vjust=1))
gdpEffect

## Polity
# gdpQts = quantile(regData$LlnGdpCap, probs=c(0.25, 0.75))
# gdpRange = seq(gdpQts[1], gdpQts[2], .1)
polEffect = ggsimplot(modelResults=mod, sims=10000, simData=regData, 
  vars=vars, actual=FALSE, brk=0.1, 
  vi='Lpolity2', ostat=median, sigma=FALSE, intercept=TRUE,
  ylabel="Log(Aid)$_{t}$", xlabel="Polity$_{t-1}$",
  plotType='ribbon'
  )
gdpEffect=gdpEffect + theme(axis.title.y=element_text(vjust=1))
gdpEffect
#########################################################

# regData = iData[[1]]
mod = allyMods[[1]]
var = 'LallyDist'

#########################################################
# Create scenario matrix
stratQts = quantile(regData[,var], probs=c(.05,.95), na.rm=TRUE)
# stratRange=with(data=regData, seq(min(LstratMu), max(LstratMu), .01) )
stratRange=with(data=regData, seq(stratQts[1], stratQts[2], .01) )
disRange=with(data=regData, seq(min(Lno_disasters), 5, 1) )
scen = with(data=regData, 
	expand.grid(
		1, stratRange, disRange, 
		median(colony,na.rm=TRUE), median(Lpolity2,na.rm=TRUE), 
		median(LlnGdpCap,na.rm=TRUE), 
		median(LlifeExpect,na.rm=TRUE),median(Lcivwar,na.rm=TRUE)
		) )

# Add interaction term
scen = cbind( scen, scen[,2]*scen[,3] )
colnames(scen) = colnames( coef(mod)$id )
scen = data.matrix(scen)
pred = scen %*% mod@beta
draws = mvrnorm(10000, mod@beta, vcov(mod))
sysUncert = scen %*% t(draws)
sysInts = t(apply(sysUncert, 1, function(x){ quantile(x, c(0.05, 0.95), na.rm=TRUE) }))

# Combine for plotting
ggData=data.frame(
		cbind(pred, sysInts, scen[,var], scen[,'Lno_disasters'])
	)
names(ggData)=c('fit', 'sysLo', 'sysHi', var, 'Lno_disasters')

# Make a surface plot
tmp=ggplot(ggData, aes(x=LallyDist, y=Lno_disasters, fill=fit)) 
tmp=tmp + xlab('Strategic Interest') + ylab('No. Disasters')
tmp=tmp + geom_tile(colour='darkgrey')
tmp=tmp + scale_fill_gradient2(midpoint=median(regData$logAid), 
	space='rgb', low="#d73027", mid="white", high="#4575b4", name='Log(Aid)\n')
tmp=tmp + scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0))
tmp=tmp + theme(axis.ticks=element_blank(), 
  legend.position='top', legend.key.width=unit(2,'cm'),
  panel.grid.major = element_blank(), panel.grid.minor = element_blank() )
tmp

# Plot rel at various cuts of disasters
disRange=with(data=regData, seq(min(Lno_disasters), max(Lno_disasters), 1) )
disRange=c(1,3,5)
ggDataSmall = ggData[which(ggData$Lno_disasters %in% disRange),]
tmp=ggplot(ggDataSmall, aes(x=LallyDist, y=fit))
tmp=tmp + geom_line()
tmp=tmp + scale_x_continuous(breaks=2:5, limits=c(2,5))
tmp=tmp + geom_ribbon(aes(ymin=sysLo, ymax=sysHi), alpha=.7)
tmp=tmp + facet_grid(~Lno_disasters)
tmp=tmp + xlab('Pol. Strat. Distance$_{sr,t-1}$') + ylab("Log(Aid)$_{t}$")
tmp=tmp + theme(axis.ticks=element_blank(), 
  panel.grid.major = element_blank(), panel.grid.minor = element_blank() )
tmp
tikz(file=paste0(pathGraphics, 'intEffect.tex'), width=8, height=5, standAlone=F)
tmp
dev.off()
#########################################################