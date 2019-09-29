source('setupAppendix.R')
################################################################

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

#########################################################
# aid dependence lag structure

# lag 1
load('mods/reMods_wLagDV.rda')
regData = iData[[1]]

# lag 3
# run models for sub effects with varying lag structure
lag3vars = c(
  'LstratMu_3', 'Lno_disasters_3', 
  'LstratMu_3 * Lno_disasters_3', 'colony', 
  'Lpolity2', 'LlnGdpCap', 'LlifeExpect',
  'Lcivwar' )
baseSpec = paste(lag3vars, collapse=' + ' )

# set up formulas
reModSpecs = lapply(dvs, function(y){
  formula(
    paste0(y, '~', paste0('L',y), ' + ', baseSpec, reStruc)
  ) 
})

# run
regData_3 = na.omit(
  regData[,
          c(
            dvs, paste0('L',dvs),
            'id','year', 
            lag3vars[-3])
          ]
)

if(!file.exists('mods/reMods_wLagDV_3.rda')){
  cl=makeCluster(3) ; registerDoParallel(cl)
  stratMuIntMods_3 = foreach(
    spec = reModSpecs, .packages=c('lme4') ) %dopar% {
      mod = lmer(spec, data=regData_3)
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
  names(stratMuIntMods_3) = dvs
  
  #
  save(stratMuIntMods_3, file='mods/reMods_wLagDV_3.rda')
} else { load('mods/reMods_wLagDV_3.rda') }

# lag 5
# run models for sub effects with varying lag structure
lag5vars = c(
  'LstratMu_5', 'Lno_disasters_5', 
  'LstratMu_5 * Lno_disasters_5', 'colony', 
  'Lpolity2', 'LlnGdpCap', 'LlifeExpect',
  'Lcivwar' )
baseSpec = paste(lag5vars, collapse=' + ' )

# set up formulas
reModSpecs = lapply(dvs, function(y){
  formula(
    paste0(y, '~', paste0('L',y), ' + ', baseSpec, reStruc)
  ) 
})

# run
regData_5 = na.omit(
  regData[,
          c(
            dvs, paste0('L',dvs),
            'id','year', 
            lag5vars[-3])
          ]
)

if(!file.exists('mods/reMods_wLagDV_5.rda')){
  cl=makeCluster(3) ; registerDoParallel(cl)
  stratMuIntMods_5 = foreach(
    spec = reModSpecs, .packages=c('lme4') ) %dopar% {
      mod = lmer(spec, data=regData_5)
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
  names(stratMuIntMods_5) = dvs
  
  #
  save(stratMuIntMods_5, file='mods/reMods_wLagDV_5.rda')
} else { load('mods/reMods_wLagDV_5.rda') }

# org results by dv
humMods = list(
  stratMuIntMods$humanitarianTotal,
  stratMuIntMods_3$humanitarianTotal,
  stratMuIntMods_5$humanitarianTotal
)

devMods = list(
  stratMuIntMods$developTotal,
  stratMuIntMods_3$developTotal,
  stratMuIntMods_5$developTotal
)

civMods = list(
  stratMuIntMods$civSocietyTotal,
  stratMuIntMods_3$civSocietyTotal,
  stratMuIntMods_5$civSocietyTotal
)

mods = list(humMods, devMods, civMods)
names(mods) = dvs
################################################################

################################################################
# calc substantive effect
simPlots = list()
for(i in 1:length(dvs)){
  stratMuIntMods = mods[[i]]
  names(stratMuIntMods) = paste0('Lag ', c(1,3,5))
  
  simResults = lapply(1:length(stratMuIntMods), function(j){
    mod = stratMuIntMods[[j]]
    beta = mod$summ[,1]
    varcov = mod$varcov    
    var = rownames(varcov)[3]
    disVar = rownames(varcov)[4]
    
    # Create scenario matrix
    stratQts = quantile(regData[,var], probs=c(.05,.95), na.rm=TRUE)
    stratRange=with(data=regData, seq(stratQts[1], stratQts[2], .01) )
    disRange=seq( min(regData[,disVar],na.rm=TRUE), 4, 2 ) 
    scen = with(data=regData, 
                expand.grid(
                  1, 
                  median(get(paste0('L',dvs[i])), na.rm=TRUE),
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

ggsave(simPlots[[1]], 
       file='floats/figureA2.pdf', 
       width=7, height=4)
ggsave(simPlots[[2]], 
       file='floats/figureA4.pdf', 
       width=7, height=4)
ggsave(simPlots[[3]], 
       file='floats/figureA3.pdf', 
       width=7, height=4)
################################################################