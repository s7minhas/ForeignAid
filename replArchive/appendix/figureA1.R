source('setupAppendix.R')
loadPkg('gridExtra')
#############################################################

#############################################################
# Load reg data
load('../main/intake/iDataDisagg_wLags_v3.rda')
#############################################################

#############################################################
# create dv lags
iData = lapply(iData, function(df){
  # gen some ids
  df$id = with(df, paste0(ccodeS, 9999, ccodeR) )
  df$id = num(df$id)
  df$idYr = with(df, paste0(id, year))
  df$idYr = num(df$idYr)
  
  # lag
  df = lagData(df,
               'idYr','id', 
               c(
                 'humanitarianTotal', 
                 'developTotal',
                 'civSocietyTotal'
               ) )
  
  return(df) })
#############################################################

#############################################################
# specs
dvs = c(
  'humanitarianTotal',
  'developTotal',
  'civSocietyTotal' )
dvNames = paste0(
  c('Humanitarian', 'Development', 'Civil Society'), ' Aid')
baseSpec = paste(
  c(
    'LstratMu', 'Lno_disasters', 
    'LstratMu * Lno_disasters', 'colony', 
    'Lpolity2', 'LlnGdpCap', 'LlifeExpect',
    'Lcivwar'
  ), collapse=' + ' )
reStruc = '+ (1|id) + (1|year)'

# set up formulas
reModSpecs = lapply(dvs, function(y){
  formula(
    paste0(y, '~', paste0('L',y), ' + ', baseSpec, reStruc)
  ) 
})
#############################################################

#############################################################
# run models for sub effects
if(!file.exists('mods/mod_lagDV.rda')){
  cl=makeCluster(3) ; registerDoParallel(cl)
  
  # results consistent with other imputed datasets
  regData = iData[[1]] 
  stratMuIntMods = foreach(
    spec = reModSpecs, .packages=c('lme4') ) %dopar% {
      mod = lmer(spec, data=regData)
      varcov = vcov(mod)
      summ = cbind(
        'Estimate' = fixef(mod),
        'Std. Error' = sqrt(diag(varcov))
      )
      return(list(
        varcov=varcov,
        summ=summ ))
    }
  stopCluster(cl)
  names(stratMuIntMods) = dvs
  
  # 
  save(stratMuIntMods, file='mods/reMods_wLagDV.rda')
  
} else { load('mods/reMods_wLagDV.rda') }
#############################################################

#############################################################
# sub effects
regData = iData[[1]]
noDisast = 4
simPlots = lapply(1:length(stratMuIntMods), function(i){
  mod = stratMuIntMods[[i]]
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
                1, 
                median(get(paste0('L',names(stratMuIntMods)[i])), na.rm=TRUE),
                stratRange, disRange, 
                median(colony,na.rm=TRUE), median(Lpolity2,na.rm=TRUE), 
                median(LlnGdpCap,na.rm=TRUE), 
                median(LlifeExpect,na.rm=TRUE),median(Lcivwar,na.rm=TRUE)
              ) )
  
  # Add interaction term
  scen = cbind( scen, scen[,3]*scen[,4] )
  colnames(scen) = colnames(varcov)
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

simComboPlot=grid.arrange(
  simPlots[[1]], simPlots[[3]], simPlots[[2]],
  nrow=length(stratMuIntMods))

ggsave(simComboPlot,
       file='floats/figureA1.pdf',
       width=8, height=8)
#############################################################