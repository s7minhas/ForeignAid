if(Sys.info()['user']=='s7m' | Sys.info()['user']=='janus829'){ source('~/Research/ForeignAid/RCode/setup.R') }
if(Sys.info()['user']=='cindycheng'){ source('~/Documents/Papers/ForeignAid/RCode/setup.R') }
################################################################

################################################################
# load data
load(paste0(pathData, '/noImputationDataAidDisagg.rda'))
load(paste0(pathData, '/iDataDisagg.rda'))

# load model
dvs = c('humanitarianTotal', 'developTotal', 'civSocietyTotal', 'notHumanitarianTotal')
dvNames = c('Humanitarian', 'Development', 'Civil Society', 'Non-Humanitarian')
modPaths = lapply(dvs, function(dv){
  paste0(pathResults, '/', dv, '_fullSamp_gaussian_re_LstratMu_.rda') })
intModPaths = lapply(dvs, function(dv){
  paste0(pathResults, '/', dv, '_fullSamp_gaussian_re_LstratMu_interaction.rda') })
stratMuMods = lapply(modPaths, function(x){load(x);return(mods)}) ; names(stratMuMods) = dvs
stratMuIntMods = lapply(intModPaths, function(x){load(x);return(mods)}) ; names(stratMuIntMods) = dvs

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
varsNoInt=c('(Intercept)','LstratMu', cntrlVars)
varNamesNoInt = c('(Intercept)','Pol. Strat. Distance$_{sr,t-1}$', cntrlVarNames)
varsInt=c('(Intercept)','LstratMu', cntrlVars, 'LstratMu:Lno_disasters')
varNamesInt = c( '(Intercept)', 'Pol. Strat. Distance$_{sr,t-1}$', cntrlVarNames, 
  'Pol. Strat. Distance$_{sr,t-1}$ \n x No. Disasters$_{r,t-1}$')

# 
coefp_colors = c("Positive"=rgb(54, 144, 192, maxColorValue=255), 
  "Negative"= rgb(222, 45, 38, maxColorValue=255),
  "Positive at 90"=rgb(158, 202, 225, maxColorValue=255), 
  "Negative at 90"= rgb(252, 146, 114, maxColorValue=255),
  "Insig" = rgb(150, 150, 150, maxColorValue=255))

summarizeMods = function(mods, dirtyVars, cleanVars){
  modSumm = lapply(1:length(mods), function(i){
    mod = mods[[i]]; summ = rubinCoef(mod)
    summ$dv = names(mods)[i]
    summ$up95 = with(summ, beta + qnorm(.975)*se) ; summ$lo95 = with(summ, beta - qnorm(.975)*se)
    summ$up90 = with(summ, beta + qnorm(.95)*se); summ$lo90 = with(summ, beta - qnorm(.95)*se)
    summ$varClean = cleanVars[match(summ$var, dirtyVars)]
    summ$dvClean = dvNames[match(summ$dv, dvs)]
    summ$sig = NA
    summ$sig[summ$lo90 > 0 & summ$lo95 < 0] = "Positive at 90"
    summ$sig[summ$lo95 > 0] = "Positive"
    summ$sig[summ$up90 < 0 & summ$up95 > 0] = "Negative at 90"
    summ$sig[summ$up95 < 0] = "Negative"
    summ$sig[summ$lo90 < 0 & summ$up90 > 0] = "Insig"
    return(summ) }) %>% do.call('rbind', .)
  return(modSumm) }

#
noIntModSumm = summarizeMods(stratMuMods, varsNoInt, varNamesNoInt)
intModSumm = summarizeMods(stratMuIntMods, varsInt, varNamesInt)
################################################################

################################################################
# Model results
plotRes = function(modSumm){
  modSumm = modSumm[modSumm$var!='(Intercept)',]
  modSumm = modSumm[modSumm$dv!='notHumanitarianTotal',]
  ggplot(modSumm, aes(x=varClean, y=beta, color=sig)) +
    geom_hline(aes(yintercept=0), linetype='dashed', color='grey40') + 
    geom_point() +
    geom_linerange(aes(ymin=lo95,ymax=up95), size=.3) +
    geom_linerange(aes(ymin=lo90,ymax=up90), size=1) +
    scale_color_manual(values=coefp_colors) +
    coord_flip() +
    facet_wrap(~dvClean, ncol=4, scales='free_x') +
    ylab('') + xlab('') + 
    theme(
      axis.ticks = element_blank(), 
      panel.border=element_blank(),
      legend.position='none' ) }

noIntGG = plotRes(noIntModSumm)
intGG = plotRes(intModSumm)
ggsave(noIntGG, file=paste0(pathGraphics, '/noIntCoef.pdf'), width=8, height=6)
ggsave(intGG, file=paste0(pathGraphics, '/intCoef.pdf'), width=8, height=6)
#########################################################

# #########################################################
# # Substantive effects
# ## Strategic interest
# mod = stratMuMods$'humanitarianTotal'[[1]] # results consistent for other model iters
# simVars = c('LstratMu', cntrlVars)

# stratEffect = ggsimplot(modelResults=mod, sims=10000, simData=regData, 
#   vars=simVars, actual=FALSE, brk=0.01, 
#   # vRange=regData$LstratMu[!is.na(regData$LstratMu)],
#   vRange=seq(min(regData$LstratMu,na.rm=TRUE), max(regData$LstratMu, na.rm=TRUE), 0.01),
#   vi='LstratMu', ostat=median, sigma=FALSE, intercept=TRUE,
#   ylabel="Log(Aid)$_{t}$", xlabel="Strategic Distance$_{t-1}$",
#   plotType='ribbon'
#   ) + theme(axis.title.y=element_text(vjust=1))
# # regData$Fit=0 ; regData$Lo95=0 ; regData$Hi95=0 ; regData$Lo90=0; regData$Hi90=0
# # stratEffect = stratEffect + geom_rug(data=regData, aes(x=LstratMu), sides='b') + ylim(0.3,.9)
# tikz(file=paste0(pathGraphics, '/stratEffect.tex'), width=8, height=5, standAlone=F)
# stratEffect
# dev.off()

# ## Natural disaster 
# disastEffect = ggsimplot(modelResults=mod, sims=10000, simData=regData, 
#   vars=simVars, actual=FALSE, brk=1, vRange=1:10,
#   # vRange=regData$Lno_disasters[!is.na(regData$Lno_disasters)],
#   # vRange=seq(min(regData$Lno_disasters,na.rm=TRUE), max(regData$Lno_disasters, na.rm=TRUE), 0.01),
#   vi='Lno_disasters', ostat=median, sigma=FALSE, intercept=TRUE,
#   ylabel="Log(Aid)$_{t}$", xlabel="No. Disasters$_{t-1}$",
#   plotType='errorBar'
#   ) + theme(axis.title.y=element_text(vjust=1))
# # regData$Fit=0 ; regData$Lo95=0 ; regData$Hi95=0 ; regData$Lo90=0; regData$Hi90=0
# # stuff=data.frame(table(regData$Lno_disasters))
# # stuff$freqScale = rescale(stuff$Freq, max(disastEffect$data$Fit), min(disastEffect$data$Fit))

# # disastEffect + geom_bar(data=stuff, aes(x=Var1, y=freqScale), stat='identity', alpha=.4) 

# disastEffect = disastEffect +
#   geom_rug(
#     data=regData[regData$Lno_disasters<=10,], 
#     aes(x=Lno_disasters), sides='b') +
#   scale_x_continuous(limits=c(1,10))
# tikz(file=paste0(pathGraphics, '/disastEffect.tex'), width=8, height=5, standAlone=F)
# disastEffect
# dev.off()

# ##
# tikz(file=paste0(pathGraphics, '/effects.tex'), width=8, height=4, standAlone=F)
# multiplot(list(stratEffect, disastEffect), cols=2)
# dev.off()
# #########################################################

#########################################################
# switch to interaction mod
someplots = lapply(1:length(stratMuIntMods), function(i){
mod = stratMuIntMods[[i]][[1]]
modTitle = dvNames[i]
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
  ggtitle(modTitle) + 
  theme(
    axis.ticks=element_blank(), 
    panel.border = element_blank()
  )
return(tmp)
})


loadPkg('gridExtra')
simComboPlot=grid.arrange(someplots[[1]], someplots[[2]], someplots[[3]], someplots[[4]], nrow=length(stratMuIntMods))
ggsave(simComboPlot, file=paste0(pathGraphics, '/simComboPlot.pdf'), width=8, height=8)

tikz(file=paste0(pathGraphics, '/intEffect.tex'), width=8, height=5, standAlone=F)
tmp
dev.off()
#########################################################