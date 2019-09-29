source('setupAppendix.R')
loadPkg('gridExtra')
################################################################

################################################################
# Load reg data
load('../main/intake/iDataDisagg_wLags_v3.rda')

# add binary version of disaster variable
iData = lapply( iData, function(df){
  df$L_disaster = 1*(df$Lno_disasters>1)
  return(df) })
################################################################

################################################################
# specs for models
covarsRev = c(
  'LstratMu', 'L_disaster', 
  'LstratMu * L_disaster',
  'colony', 'Lpolity2', 'LlnGdpCap', 
  'LlifeExpect', 'Lcivwar' )
struc = c('+ (1|id) + (1|year)')
dvs = c(
  'humanitarianTotal',
  'civSocietyTotal',
  'developTotal'
)
formsRev = lapply(dvs, function(y){
  formula( paste0(y, ' ~ ', 
                  paste(covarsRev, collapse=' + '), 
                  struc ) ) })

# run models
if(!file.exists('mods/reMods_binDisaster.rda')){
  modsRev_full = lapply(formsRev, function(form){
    mods = lapply(iData, function(data){
      mod = lmer(form, data=data) })
    return( mods ) })
  modsRev_full = storeResults(modsRev_full)
  names(modsRev_full) = dvs
  save(modsRev_full, file='mods/reMods_binDisaster.rda')  
} else { load('mods/reMods_binDisaster.rda') }
################################################################

#########################################################
# switch to interaction mod
regData = iData[[1]]
noDisast = 1
stratMuIntMods = modsRev_full
dvNames = paste0(
  c('Humanitarian', 'Civil Society', 'Development'), ' Aid')

simPlots = lapply(1:length(stratMuIntMods), function(i){
  mod = stratMuIntMods[[i]][[1]]
  beta = mod$summ[,1]
  varcov = mod$varcov  
  modTitle = dvNames[i] ; var = 'LstratMu'
  
  # Create scenario matrix
  stratQts = quantile(regData[,var], probs=c(.05,.95), na.rm=TRUE)
  stratRange=with(data=regData, seq(stratQts[1], stratQts[2], .01) )
  disRange=with(data=regData, seq(
    0, 1, 1) )  
  scen = with(data=regData, 
              expand.grid(
                1, stratRange, disRange, 
                median(colony,na.rm=TRUE), median(Lpolity2,na.rm=TRUE), 
                median(LlnGdpCap,na.rm=TRUE), 
                median(LlifeExpect,na.rm=TRUE),median(Lcivwar,na.rm=TRUE)
              ) )
  
  # Add interaction term
  scen = cbind( scen, scen[,2]*scen[,3] )
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
          scen[,var], scen[,'L_disaster'])
  )
  names(ggData)=c('fit', 'sysLo95', 'sysHi95', 
                  'sysLo90', 'sysHi90', var, 'L_disaster')
  
  # Plot rel at various cuts of disasters
  disRange=with(data=regData, seq(
    min(L_disaster), noDisast, 1) )    
  ggDataSmall = ggData[which(ggData$L_disaster %in% disRange),]
  actData = regData[,c('LstratMu', 'L_disaster')]
  actData = actData[actData$L_disaster %in% seq(0,noDisast,1),]
  actData = actData[
    actData$LstratMu>=stratQts[1] & actData$LstratMu<=stratQts[2],]
  
  # change facet labels
  ggDataSmall$L_disaster[
    ggDataSmall$L_disaster==0] = 'No Disasters$_{r,t-1}$'
  ggDataSmall$L_disaster[
    ggDataSmall$L_disaster==1] = 'One or More Disaster$_{r,t-1}$'
  actData$L_disaster[
    actData$L_disaster==0] = 'No Disasters$_{r,t-1}$'
  actData$L_disaster[
    actData$L_disaster==1] = 'One or More Disaster$_{r,t-1}$'    
  
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
      ~L_disaster, 
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
  simPlots[[1]], simPlots[[2]], simPlots[[3]],
  nrow=length(stratMuIntMods))
ggsave(
  simComboPlot, 
  file='floats/figureA5.pdf',
  width=8, height=8)
#########################################################