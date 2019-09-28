source('setup.R')
################################################################

################################################################
# Load reg data
load('intake/iDataDisagg_wLags_v3.rda')
load('mods/mainMod.rda')
################################################################

################################################################
# specs for models
dvs = c(
  'humanitarianTotal',
  'developTotal',
  'civSocietyTotal' )
dvNames = paste(
  c('Humanitarian', 'Development', 'Civil Society'), 'Aid')
struc = c('+ (1|id) + (1|year)')
vars = c(
  'LstratMu_3', 'Lno_disasters_3', 
  'LstratMu_3 * Lno_disasters_3',
  'colony', 'Lpolity2', 'LlnGdpCap', 
  'LlifeExpect', 'Lcivwar' )
forms = lapply(dvs, function(y){
  formula( paste0(y, ' ~ ', 
                  paste(vars, collapse=' + '), 
                  struc ) ) })

# run models
if(!file.exists('mods/mainMod_lag3.rda')){
  stratMuIntMods_lag3 = lapply(forms, function(form){
    mods = lapply(iData, function(data){
      mod = lmer(form, data=data) })
    return( mods ) })
  names(stratMuIntMods_lag3) = dvs
  
  # save model
  stratMuIntMods_lag3 = storeResults(stratMuIntMods_lag3)
  save(stratMuIntMods_lag3, file='mods/mainMod_lag3.rda')  
} else { load('mods/mainMod_lag3.rda') }

#
vars[1:3] = c(
  'LstratMu_5', 'Lno_disasters_5', 
  'LstratMu_5 * Lno_disasters_5')
forms = lapply(dvs, function(y){
  formula( paste0(y, ' ~ ', 
                  paste(vars, collapse=' + '), 
                  struc ) ) })

# run models
if(!file.exists('mods/mainMod_lag5.rda')){
  stratMuIntMods_lag5 = lapply(forms, function(form){
    mods = lapply(iData, function(data){
      mod = lmer(form, data=data) })
    return( mods ) })
  names(stratMuIntMods_lag5) = dvs
  
  # save model
  stratMuIntMods_lag5 = storeResults(stratMuIntMods_lag5)
  save(stratMuIntMods_lag5, file='mods/mainMod_lag5.rda')  
} else { load('mods/mainMod_lag5.rda') }

# org lists by dvs
tmp = list(stratMuIntMods, stratMuIntMods_lag3, stratMuIntMods_lag5)
allMods = lapply(names(stratMuIntMods), function(var){
  dvMods = lapply( tmp, function(mods){ mods[[var]][[1]] } )
  names(dvMods) = paste0('Lag ', c(1,3,5))
  return(dvMods) })
names(allMods) = names(stratMuIntMods)
rm(
  tmp,
  stratMuIntMods, stratMuIntMods_lag3, stratMuIntMods_lag5
  )
################################################################

################################################################
# calc substantive effect
regData = iData[[1]]
noDisast = 4
lagIDs = names(allMods[[1]])
simPlots = list()
for(i in 1:length(dvs)){
  simResults = lapply(1:length(lagIDs), function(j){
    mod = allMods[[ dvs[i] ]][[ lagIDs[j] ]]
    beta = mod$summ[,1]
    varcov = mod$varcov    
    
    #
    var = rownames(varcov)[2]
    disVar = rownames(varcov)[3]
    
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
    colnames(scen) = rownames(varcov)
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
    ggDataSmall$lagID = lagIDs[j]
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

ggsave(simPlots[[1]], file='floats/figure5.pdf', width=7, height=4)
ggsave(simPlots[[3]], file='floats/figure6.pdf', width=7, height=4)
ggsave(simPlots[[2]], file='floats/figure7.pdf', width=7, height=4)
################################################################