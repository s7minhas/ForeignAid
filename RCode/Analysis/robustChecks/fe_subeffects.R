if(Sys.info()['user']=='s7m' | Sys.info()['user']=='janus829'){
	source('~/Research/ForeignAid/RCode/setup.R') }
if(Sys.info()['user']=='cindycheng'){
	source('~/Documents/Papers/ForeignAid/RCode/setup.R') }
################################################################

################################################################
# Load reg data
load(paste0(pathData, '/iDataDisagg_wLags_v3.rda'))

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
      )
    )

  return(df) })
################################################################

################################################################
# Load reg data
dvs = c(
	'humanitarianTotal',
	'developTotal',
	'civSocietyTotal'
	)
dvNames = paste0(
  c('Humanitarian', 'Development', 'Civil Society'), ' Aid')
baseSpec = paste(
	c(
		'LstratMu', 'Lno_disasters', 
		'LstratMu * Lno_disasters', 'colony', 
		'Lpolity2', 'LlnGdpCap', 'LlifeExpect',
		'Lcivwar'
	), collapse=' + ' )
feStruc = '+ factor(ccodeS) - 1'

# set up formulas
feModSpecs = lapply(dvs, function(y){
	formula(paste0(y, '~', paste0('L',y), '+', baseSpec, feStruc)) })
################################################################

################################################################
# run models
cl=makeCluster(5) ; registerDoParallel(cl)

# run fixed effect models
## humanitarian model
humModFE= foreach(df=iData) %dopar% {
		lm(feModSpecs[[1]], data=df) }

## civ society model
civModFE = foreach(df=iData) %dopar% {
		lm(feModSpecs[[2]], data=df) }

## dev model
devModFE = foreach(df=iData) %dopar% {
    lm(feModSpecs[[3]], data=df) }

#
stopCluster(cl)

# org for coef plot in re_fe_compare.R
feMods = lapply(
  list(humModFE, civModFE, devModFE),
  function(impMods){
    coefSumm=lapply(impMods, function(mod){
      summ = summary(mod)$'coefficients'  
      return(summ)
    })
    return(rubinCoef(coefSumm, 'fe'))
  } )
save(feMods,
  file=paste0(pathResults, '/feMods_appendix.rda'))

# org for sub effects analysis
stratMuIntMods = list(
	humModFE[[1]],
	civModFE[[1]],
  devModFE[[1]]  
	)
names(stratMuIntMods) = dvs
################################################################

#########################################################
# sub effects
regData = iData[[1]]
noDisast = 4
simPlots = lapply(1:length(stratMuIntMods), function(i){
  mod = stratMuIntMods[[i]]
  vars = names(coef(mod))[c(1:8, length(coef(mod)) )]
  modTitle = dvNames[i] ; var = 'LstratMu'

  # Create scenario matrix
  stratQts = quantile(regData[,var], probs=c(.05,.95), na.rm=TRUE)
  stratRange=with(data=regData, seq(stratQts[1], stratQts[2], .01) )
  disRange=with(data=regData, seq(
    min(Lno_disasters), noDisast, 2) )  
  scen = with(data=regData, 
    expand.grid(
      median(get(paste0('L',names(stratMuIntMods)[i])), na.rm=TRUE),
      stratRange, disRange, 
      median(colony,na.rm=TRUE), median(Lpolity2,na.rm=TRUE), 
      median(LlnGdpCap,na.rm=TRUE), 
      median(LlifeExpect,na.rm=TRUE),median(Lcivwar,na.rm=TRUE)
      ) )

  # Add interaction term
  scen = cbind( scen, scen[,2]*scen[,3] )
  colnames(scen) = vars
  scen = data.matrix(scen)
  pred = scen %*% coef(mod)[vars]
  draws = mvrnorm(10000, coef(mod)[vars], vcov(mod)[vars,vars])
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

loadPkg('gridExtra')
simComboPlot=grid.arrange(
  simPlots[[1]], simPlots[[3]], simPlots[[2]],
  nrow=length(stratMuIntMods))

ggsave(simComboPlot, file=paste0(
  pathGraphics, '/simComboPlot_fe.pdf'), width=8, height=8)
#########################################################