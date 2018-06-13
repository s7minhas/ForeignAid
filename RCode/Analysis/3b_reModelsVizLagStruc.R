if(Sys.info()['user']=='s7m' | Sys.info()['user']=='janus829'){ source('~/Research/ForeignAid/RCode/setup.R') }
if(Sys.info()['user']=='cindycheng'){ source('~/Documents/Papers/ForeignAid/RCode/setup.R') }
################################################################

################################################################
# load data
load(paste0(pathData, '/iDataDisagg_wLags.rda'))
regData = iData[[1]]

# load model
dvs = c('humanitarianTotal', 'developTotal', 'civSocietyTotal', 'notHumanitarianTotal')
dvNames = paste(c('Humanitarian', 'Development', 'Civil Society', 'Non-Humanitarian'), 'Aid')
coefp_colors = c("Positive"=rgb(54, 144, 192, maxColorValue=255), 
  "Negative"= rgb(222, 45, 38, maxColorValue=255),
  "Positive at 90"=rgb(158, 202, 225, maxColorValue=255), 
  "Negative at 90"= rgb(252, 146, 114, maxColorValue=255),
  "Insig" = rgb(150, 150, 150, maxColorValue=255))
################################################################

################################################################
for(i in 1:length(dvs)){
  stratMuMods = lapply(c('',paste0(2:6, '_')), function(x){
    pth = paste0(pathResults, '/', dvs[i], '_fullSamp_gaussian_re_LstratMu_',x,'.rda')
    load(pth) ; return(mods) })
  names(stratMuMods) = paste0('Lag ', 1:6)

  modSumm = do.call('rbind', lapply(1:length(stratMuMods), function(i){
    x = stratMuMods[[i]]
    x = rubinCoef(x)
    summ = x[c(2,8),,drop=FALSE]
    summ$lag = names(stratMuMods)[i]
    summ$up95 = with(summ, beta + qnorm(.975)*se) ; summ$lo95 = with(summ, beta - qnorm(.975)*se)
    summ$up90 = with(summ, beta + qnorm(.95)*se); summ$lo90 = with(summ, beta - qnorm(.95)*se)  
    summ$sig = NA
    summ$sig[summ$lo90 > 0 & summ$lo95 < 0] = "Positive at 90"
    summ$sig[summ$lo95 > 0] = "Positive"
    summ$sig[summ$up90 < 0 & summ$up95 > 0] = "Negative at 90"
    summ$sig[summ$up95 < 0] = "Negative"
    summ$sig[summ$lo90 < 0 & summ$up90 > 0] = "Insig"  
    summ$varFacet = c('Strategic Distance', 'No. Disasters')
    return(summ) }))

  tmp=ggplot(modSumm, aes(x=lag, y=beta, color=sig)) +
    geom_hline(aes(yintercept=0), linetype='dashed', color='grey40') + 
    geom_point() +
    geom_linerange(aes(ymin=lo95,ymax=up95), size=.3) +
    geom_linerange(aes(ymin=lo90,ymax=up90), size=1) +
    scale_color_manual(values=coefp_colors) +
    ggtitle(dvNames[i]) +
    facet_wrap(~varFacet, nrow=2, scales='free_x') +
    ylab('') + xlab('') + 
    theme(
      axis.ticks = element_blank(), 
      panel.border=element_blank(),
      legend.position='none' )
  ggsave(tmp, file=paste0(pathGraphics, '/', dvs[i], '_noInt_lagEffect.pdf'), width=8, height=5)
}
################################################################

################################################################
for(i in 1:length(dvs)){
  stratMuIntMods = lapply(c('',paste0(2:6, '_')), function(x){
    pth = paste0(pathResults, '/', dvs[i], '_fullSamp_gaussian_re_LstratMu_', x, 'interaction.rda')
    load(pth) ; return(mods) })
  names(stratMuIntMods) = paste0('Lag ', 1:6)

  modSumm = do.call('rbind', lapply(1:length(stratMuIntMods), function(i){
    x = stratMuIntMods[[i]]
    x = rubinCoef(x)
    summ = x[c(2,3,9),,drop=FALSE]
    summ$lag = names(stratMuIntMods)[i]
    summ$up95 = with(summ, beta + qnorm(.975)*se) ; summ$lo95 = with(summ, beta - qnorm(.975)*se)
    summ$up90 = with(summ, beta + qnorm(.95)*se); summ$lo90 = with(summ, beta - qnorm(.95)*se)  
    summ$sig = NA
    summ$sig[summ$lo90 > 0 & summ$lo95 < 0] = "Positive at 90"
    summ$sig[summ$lo95 > 0] = "Positive"
    summ$sig[summ$up90 < 0 & summ$up95 > 0] = "Negative at 90"
    summ$sig[summ$up95 < 0] = "Negative"
    summ$sig[summ$lo90 < 0 & summ$up90 > 0] = "Insig"  
    summ$varFacet = c('Strategic Distance', 'No. Disasters', 'Strategic Distance x No. Disasters')
    return(summ) }))

  tmp=ggplot(modSumm, aes(x=lag, y=beta, color=sig)) +
    geom_hline(aes(yintercept=0), linetype='dashed', color='grey40') + 
    geom_point() +
    geom_linerange(aes(ymin=lo95,ymax=up95), size=.3) +
    geom_linerange(aes(ymin=lo90,ymax=up90), size=1) +
    scale_color_manual(values=coefp_colors) +
    ggtitle(dvNames[i]) +
    facet_wrap(~varFacet, nrow=3, scales='free_x') +
    ylab('') + xlab('') + 
    theme(
      axis.ticks = element_blank(), 
      panel.border=element_blank(),
      legend.position='none' )
  ggsave(tmp, file=paste0(pathGraphics, '/', dvs[i], '_int_lagEffect.pdf'), width=8, height=7)
}
################################################################

################################################################
# calc substantive effect
simPlots = list()
for(i in 1:length(dvs)){
  stratMuIntMods = lapply(c('',paste0(c(3,5), '_')), function(x){
    pth = paste0(
      pathResults, '/', 
      dvs[i], '_fullSamp_gaussian_re_LstratMu_', x, 'interaction.rda')
    load(pth) ; return(mods) })
  names(stratMuIntMods) = paste0('Lag ', c(1,3,5))

  simResults = lapply(1:length(stratMuIntMods), function(j){
    mod = stratMuIntMods[[j]][[1]] ; var = names(fixef(mod))[2]
    disVar = names(fixef(mod))[3]

    # Create scenario matrix
    stratQts = quantile(regData[,var], probs=c(.05,.95), na.rm=TRUE)
    stratRange=with(data=regData, seq(stratQts[1], stratQts[2], .01) )
    disRange=seq( min(regData[,disVar],na.rm=TRUE), 4, 2 ) 
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
          scen[,var], scen[,disVar])
        )
    names(ggData)=c('fit', 'sysLo95', 'sysHi95', 
      'sysLo90', 'sysHi90', var, disVar)
    names(ggData)[6:7] = c('LstratMu','Lno_disasters')

    # Plot rel at various cuts of disasters
    disRange=with(data=regData, seq(
      min(Lno_disasters), 4, 2) )
    ggDataSmall = ggData[which(ggData$Lno_disasters %in% disRange),]

    # change facet labels
    ggDataSmall$Lno_disasters = paste(ggDataSmall$Lno_disasters, 'Disasters$_{r}$')

    #
    ggDataSmall$lagID = names(stratMuIntMods)[j]
    return(ggDataSmall) })

  # viz
  ggData = do.call('rbind', simResults)
  facet_labeller = function(string){ TeX(string) }
  modTitle = dvNames[i]
  tmp=ggplot(ggData, aes(x=LstratMu, y=fit)) +
    geom_line() +
    geom_ribbon(aes(ymin=sysLo90, ymax=sysHi90), alpha=.6) +
    geom_ribbon(aes(ymin=sysLo95, ymax=sysHi95), alpha=.4) +
    facet_grid(Lno_disasters~lagID, labeller=as_labeller(facet_labeller, default = label_parsed)) +
    labs(
      x=TeX('Strategic Distance$_{sr}$'),
      y=TeX("Log(Aid)$_{t}$"),
      title=modTitle
      ) +
    theme(
      axis.ticks=element_blank(), 
      panel.border = element_blank() )
  simPlots[[i]] = tmp
}

loadPkg('gridExtra')
simComboPlot=grid.arrange(
  simPlots[[1]], simPlots[[3]], simPlots[[2]],
  nrow=length(stratMuIntMods))

ggsave(simComboPlot, file=paste0(pathGraphics, '/simComboPlot_lag.pdf'), width=8, height=16)
################################################################