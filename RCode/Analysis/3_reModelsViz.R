if(Sys.info()['user']=='s7m' | Sys.info()['user']=='janus829'){ source('~/Research/ForeignAid/RCode/setup.R') }
if(Sys.info()['user']=='cindycheng'){ source('~/Documents/Papers/ForeignAid/RCode/setup.R') }
################################################################

################################################################
# load data
load(paste0(pathData, '/noImputationData.rda'))
load(paste0(pathData, '/iData_v2.rda'))

# load model
fullSampPath = paste0(pathResults, '/fullSamp_gaussian_re_')
load(paste0(fullSampPath, 'LigoDist.rda')) ; igoMods = mods
load(paste0(fullSampPath, 'LstratMu.rda')) ; stratMuMods = mods

load(paste0(fullSampPath, 'LigoDist_interaction.rda')) ; igoIntMods = mods
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

# sig
genCoefPlot(igoMods,
  vars=c('(Intercept)','LigoDist', cntrlVars),
  varNames=varNamesNoInt, printPlot=TRUE,
  fName = paste0(pathGraphics, '/igoModCoef.tex') )

genCoefPlot(stratMuMods,
  vars=c('(Intercept)','LstratMu', cntrlVars),
  varNames=varNamesNoInt, printPlot=TRUE,
  fName = paste0(pathGraphics, '/stratMuModCoef.tex') )

# # interaction models
varNamesInt = c( '(Intercept)', 'Pol. Strat. Distance$_{sr,t-1}$', cntrlVarNames, 
  'Pol. Strat. Distance$_{sr,t-1}$ \n x No. Disasters$_{r,t-1}$')

genCoefPlot(igoIntMods,
  vars=c('(Intercept)','LigoDist', cntrlVars, 'LigoDist:Lno_disasters'),
  varNames=varNamesInt, printPlot=TRUE,
  fName = paste0(pathGraphics, '/igoModIntCoef.tex') )

genCoefPlot(stratMuIntMods,
  vars=c('(Intercept)','LstratMu', cntrlVars, 'LstratMu:Lno_disasters'),
  varNames=varNamesInt, printPlot=TRUE,
  fName = paste0(pathGraphics, '/stratMuIntCoef.tex') )
#########################################################

#########################################################
# Substantive effects
## Strategic interest
mod = stratMuMods[[1]] # results consistent for other model iters
simVars = c('LstratMu', cntrlVars)

stratEffect = ggsimplot(modelResults=mod, sims=10000, simData=regData, 
  vars=simVars, actual=FALSE, brk=0.01, 
  vi='LigoDist', ostat=median, sigma=FALSE, intercept=TRUE,
  ylabel="Log(Aid)$_{t}$", xlabel="Strategic Distance$_{t-1}$",
  plotType='ribbon'
  ) + theme(axis.title.y=element_text(vjust=1))
tikz(file=paste0(pathGraphics, '/stratEffect.tex'), width=8, height=5, standAlone=F)
stratEffect
dev.off()

## Natural disaster 
disastEffect = ggsimplot(modelResults=mod, sims=10000, simData=regData, 
  vars=simVars, actual=FALSE, brk=1, vRange=1:10,
  vi='Lno_disasters', ostat=median, sigma=FALSE, intercept=TRUE,
  ylabel="Log(Aid)$_{t}$", xlabel="No. Disasters$_{t-1}$",
  plotType='errorBar'
  ) + theme(axis.title.y=element_text(vjust=1))
tikz(file=paste0(pathGraphics, '/disastEffect.tex'), width=8, height=5, standAlone=F)
disastEffect
dev.off()

##
tikz(file=paste0(pathGraphics, '/effects.tex'), width=8, height=4, standAlone=F)
multiplot(list(stratEffect, disastEffect), cols=2)
dev.off()
#########################################################

#########################################################
# switch to interaction mod
mod = stratMuIntMods[[1]]
var = 'LstratMu'

# Create scenario matrix
stratQts = quantile(regData[,var], probs=c(.05,.95), na.rm=TRUE)
stratRange=with(data=regData, seq(stratQts[1], stratQts[2], .01) )
disRange=with(data=regData, seq(min(Lno_disasters), 10, 2) )
scen = with(data=regData, 
	expand.grid(
		1, stratRange, disRange, 
		median(colony,na.rm=TRUE), median(Lpolity2,na.rm=TRUE), 
		median(LlnGdpCap,na.rm=TRUE), 
		median(LlifeExpect,na.rm=TRUE),median(Lcivwar,na.rm=TRUE)
		) )

# Add interaction term
scen = cbind( scen, scen[,2]*scen[,3] )
colnames(scen) = names(fixef(mod))
scen = data.matrix(scen)
pred = scen %*% mod@beta
draws = mvrnorm(10000, mod@beta, vcov(mod))
sysUncert = scen %*% t(draws)
sysInts95 = t(apply(sysUncert, 1, function(x){ quantile(x, c(0.025, 0.975), na.rm=TRUE) }))
sysInts90 = t(apply(sysUncert, 1, function(x){ quantile(x, c(0.05, 0.95), na.rm=TRUE) }))

# Combine for plotting
ggData=data.frame(
		cbind(pred, sysInts95, sysInts90, scen[,var], scen[,'Lno_disasters'])
    )
names(ggData)=c('fit', 'sysLo95', 'sysHi95', 'sysLo90', 'sysHi90', var, 'Lno_disasters')

# Plot rel at various cuts of disasters
disRange=with(data=regData, seq(min(Lno_disasters), max(Lno_disasters), 2) )
ggDataSmall = ggData[which(ggData$Lno_disasters %in% disRange),]

tmp=ggplot(ggDataSmall, aes(x=LstratMu, y=fit)) +
  geom_line() +
  geom_ribbon(aes(ymin=sysLo90, ymax=sysHi90), alpha=.6) +
  geom_ribbon(aes(ymin=sysLo95, ymax=sysHi95), alpha=.4) +
  facet_grid(~Lno_disasters) +
  xlab('Pol. Strat. Distance$_{sr,t-1}$') + ylab("Log(Aid)$_{t}$") +
  theme(
    axis.ticks=element_blank(), 
    panel.border = element_blank()
  )
tikz(file=paste0(pathGraphics, '/intEffect.tex'), width=8, height=5, standAlone=F)
tmp
dev.off()
#########################################################