source('setup.R')
loadPkg('gridExtra')
#########################################################

#########################################################
# Load
load('intake/iDataDisagg_wLags_v3.rda')
load('mods/mainMod.rda')

##
dvNames = paste0(
  c('Humanitarian', 'Development', 'Civil Society'), ' Aid')
#########################################################

#########################################################
# sub effects
regData = iData[[1]]
noDisast = 4
simPlots = lapply(1:length(stratMuIntMods), function(i){
  mod = stratMuIntMods[[i]][[1]]
  beta = mod$summ[,1]
  varcov = mod$varcov
  modTitle = dvNames[i] ; var = 'LstratMu'
  
  # Create scenario matrix
  stratQts = quantile(regData[,var], probs=c(.05,.95), na.rm=TRUE)
  stratRange=with(data=regData, seq(stratQts[1], stratQts[2], .01) )
  disRange=with(data=regData, seq(
    min(Lno_disasters), noDisast, 2) )  
  scen = with(data=regData,
              expand.grid(
                1, stratRange, disRange, 
                median(colony,na.rm=TRUE), median(Lpolity2,na.rm=TRUE), 
                median(LlnGdpCap,na.rm=TRUE), 
                median(LlifeExpect,na.rm=TRUE),median(Lcivwar,na.rm=TRUE)
              ) )
  
  # Add interaction term
  scen = cbind( scen, scen[,2]*scen[,3] )
  colnames(scen) = rownames(mod$summ)
  scen = data.matrix(scen)
  pred = scen %*% beta
  draws = mvrnorm(10000, beta, varcov)
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
    min(Lno_disasters), noDisast, 2) )    
  ggDataSmall = ggData[which(ggData$Lno_disasters %in% disRange),]
  actData = regData[,c('LstratMu', 'Lno_disasters')]
  actData = actData[actData$Lno_disasters %in% seq(0,noDisast,2),]
  actData = actData[
    actData$LstratMu>=stratQts[1] & actData$LstratMu<=stratQts[2],]
  
  # change facet labels
  ggDataSmall$Lno_disasters = paste(
    ggDataSmall$Lno_disasters, 'Disasters$_{r,t-1}$')
  actData$Lno_disasters = paste(
    actData$Lno_disasters, 'Disasters$_{r,t-1}$')
  
  # viz
  facet_labeller = function(string){ TeX(string) }
  tmp=ggplot(ggDataSmall, aes(x=LstratMu, y=fit)) +
    geom_line() +
    geom_ribbon(aes(ymin=sysLo90, ymax=sysHi90), alpha=.6) +
    geom_ribbon(aes(ymin=sysLo95, ymax=sysHi95), alpha=.4) +
    geom_rug(
      data=actData, 
      aes(x=LstratMu,y=min(ggDataSmall$fit)), sides='b', alpha=.1) +
    facet_grid(
      ~Lno_disasters, 
      labeller=as_labeller(facet_labeller, default = label_parsed)) +
    labs(
      x=TeX('Strategic Distance$_{sr,t-1}$'),
      y=TeX("Log(Aid)$_{t}$"),
      title=modTitle
    ) +
    theme(
      axis.ticks=element_blank(), 
      panel.border = element_blank() )
  return(tmp) })

##
simComboPlot=grid.arrange(
  simPlots[[1]], simPlots[[3]], simPlots[[2]],
  nrow=length(stratMuIntMods))
ggsave(simComboPlot, file='floats/figure4.pdf', width=8, height=8)
#########################################################