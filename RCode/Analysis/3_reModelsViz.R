if(Sys.info()['user']=='s7m' | Sys.info()['user']=='janus829'){ source('~/Research/ForeignAid/RCode/setup.R') }
if(Sys.info()['user']=='cindycheng'){ source('~/Documents/Papers/ForeignAid/RCode/setup.R') }
################################################################

################################################################
# load data
load(paste0(pathData, '/noImputationDataAidDisagg.rda'))
load(paste0(pathData, '/iDataDisagg.rda'))

# load model
dvs = c('humanitarianTotal', 'developTotal', 'civSocietyTotal')
dvNames = paste0(c('Humanitarian', 'Development', 'Civil Society'), ' Aid')
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
varsNoInt=c('LstratMu', cntrlVars)
varNamesNoInt = c('Strategic Distance$_{sr,t-1}$', cntrlVarNames)
varsInt=c('LstratMu', cntrlVars[1], 'LstratMu:Lno_disasters', cntrlVars[-1])
varNamesInt = c('Strategic Distance$_{sr,t-1}$', cntrlVarNames[1],
  'Strategic Distance$_{sr,t-1}$\n $\\times$ No. Disasters$_{r,t-1}$', cntrlVarNames[-1])

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
    summ = summ[summ$var!='(Intercept)',]    
    summ$varClean = cleanVars[match(summ$var, dirtyVars)]
    summ$dvClean = dvNames[match(summ$dv, dvs)]
    summ$sig = NA
    summ$sig[summ$lo90 > 0 & summ$lo95 < 0] = "Positive at 90"
    summ$sig[summ$lo95 > 0] = "Positive"
    summ$sig[summ$up90 < 0 & summ$up95 > 0] = "Negative at 90"
    summ$sig[summ$up95 < 0] = "Negative"
    summ$sig[summ$lo90 < 0 & summ$up90 > 0] = "Insig"
    return(summ) }) %>% do.call('rbind', .)
  modSumm$varClean = factor(modSumm$varClean, levels=rev(cleanVars))
  modSumm$dvClean = factor(modSumm$dvClean, levels=dvNames[c(1,3,2)])
  return(modSumm) }

#
noIntModSumm = summarizeMods(stratMuMods, varsNoInt, varNamesNoInt)
intModSumm = summarizeMods(stratMuIntMods, varsInt, varNamesInt)
################################################################

################################################################
# Model results
plotRes = function(modSumm){
  # fix some labels
  xlabels = TeX(char(modSumm$varClean))
  xlabels[char(modSumm$varClean)==varNamesInt[3]] = expression( atop(
      'Strategic Distance' ['sr,t-1'],
      'x No. Disasters' ['r,t-1'] ) )

  # viz
  ggplot(modSumm, aes(x=varClean, y=beta, color=sig)) +
    geom_hline(aes(yintercept=0), linetype='dashed', color='grey40') + 
    geom_point() +
    geom_linerange(aes(ymin=lo95,ymax=up95), size=.3) +
    geom_linerange(aes(ymin=lo90,ymax=up90), size=1) +
    scale_color_manual(values=coefp_colors) +
    scale_x_discrete('',labels=xlabels) +    
    coord_flip() +
    facet_wrap(~dvClean, ncol=4, scales='free_x') +
    labs( y='' ) +
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
regData = iData[[1]]
simPlots = lapply(1:length(stratMuIntMods), function(i){
  mod = stratMuIntMods[[i]][[1]]
  modTitle = dvNames[i] ; var = 'LstratMu'

  # Create scenario matrix
  stratQts = quantile(regData[,var], probs=c(.05,.95), na.rm=TRUE)
  stratRange=with(data=regData, seq(stratQts[1], stratQts[2], .01) )
  disRange=with(data=regData, seq(
    min(Lno_disasters), 8, 2) )
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
  sysInts95 = t(apply(sysUncert, 1, function(x){
    quantile(x, c(0.025, 0.975), na.rm=TRUE) }))
  sysInts90 = t(apply(sysUncert, 1, function(x){
    quantile(x, c(0.05, 0.95), na.rm=TRUE) }))

  # Combine for plotting
  ggData=data.frame(
      cbind(pred, sysInts95, sysInts90, 
        scen[,var], scen[,'Lno_disasters'])
      )
  names(ggData)=c('fit', 'sysLo95', 'sysHi95', 
    'sysLo90', 'sysHi90', var, 'Lno_disasters')

  # Plot rel at various cuts of disasters
  disRange=with(data=regData, seq(
    min(Lno_disasters), 8, 2) )
  ggDataSmall = ggData[which(ggData$Lno_disasters %in% disRange),]
  actData = regData[,c('LstratMu', 'Lno_disasters')]
  actData = actData[actData$Lno_disasters %in% seq(0,8,2),]
  actData = actData[actData$LstratMu>=stratQts[1] & actData$LstratMu<=stratQts[2],]

  # change facet labels
  ggDataSmall$Lno_disasters = paste(ggDataSmall$Lno_disasters, 'Disasters$_{r,t-1}$')
  actData$Lno_disasters = paste(actData$Lno_disasters, 'Disasters$_{r,t-1}$')

  # viz
  facet_labeller = function(string){ TeX(string) }
  tmp=ggplot(ggDataSmall, aes(x=LstratMu, y=fit)) +
    geom_line() +
    geom_ribbon(aes(ymin=sysLo90, ymax=sysHi90), alpha=.6) +
    geom_ribbon(aes(ymin=sysLo95, ymax=sysHi95), alpha=.4) +
    geom_rug(data=actData, aes(x=LstratMu,y=min(ggDataSmall$fit)), sides='b', alpha=.1) +
    facet_grid(~Lno_disasters, labeller=as_labeller(facet_labeller, default = label_parsed)) +
    labs(
      x=TeX('Strategic Distance$_{sr,t-1}$'),
      y=TeX("Log(Aid)$_{t}$"),
      title=modTitle
      ) +
    theme(
      axis.ticks=element_blank(), 
      panel.border = element_blank() )
  return(tmp) })

loadPkg('gridExtra')
simComboPlot=grid.arrange(
  simPlots[[1]], simPlots[[3]], simPlots[[2]],
  nrow=length(stratMuIntMods))
ggsave(simComboPlot, file=paste0(pathGraphics, '/simComboPlot.pdf'), width=8, height=8)
#########################################################