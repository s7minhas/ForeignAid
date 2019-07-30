if(Sys.info()['user']=='s7m' | Sys.info()['user']=='janus829'){
	source('~/Research/ForeignAid/RCode/setup.R') }
if(Sys.info()['user']=='cindycheng'){
	source('~/Documents/Papers/ForeignAid/RCode/setup.R') }

#
loadPkg(c('lme4'))
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
# specs
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
reStruc = '+ (1|id) + (1|year)'

# set up formulas
reModSpecs = lapply(dvs, function(y){
	formula(
    paste0(y, '~', paste0('L',y), ' + ', baseSpec, reStruc)
    ) 
})
################################################################

################################################################
# run models
if(!file.exists(
	paste0(pathResults, '/reMods_wLagDV.rda')
	)){
	cl=makeCluster(5) ; registerDoParallel(cl)

	# run models
	## humanitarian model
	humModRE= foreach(df=iData, .packages=c('lme4')) %dopar% {
		summary(
			lmer(reModSpecs[[1]], data=df)
			)$'coefficients' }

	## dev model
	devModRE = foreach(df=iData) %dopar% {
		summary(
			lmer(reModSpecs[[2]], data=df)
			)$'coefficients' }

	## civ society model
	civModRE = foreach(df=iData) %dopar% {
		summary(
			lmer(reModSpecs[[3]], data=df)
			)$'coefficients' }

	#
	stopCluster(cl)

	#
	reMods = list(
		rubinCoef(humModRE,'fe'), 
		rubinCoef(devModRE,'fe'), 
		rubinCoef(civModRE,'fe')
		)
	names(reMods) = dvs

	save(reMods, 
		file=paste0(pathResults, '/reMods_wLagDV.rda')
		)
} else {
	load(paste0(pathResults, '/reMods_wLagDV.rda'))
}
################################################################

################################################################
# load re model for comparison
intModPaths = lapply(dvs, function(dv){
  paste0(pathResults, '/', dv, 
    '_fullSamp_gaussian_re_LstratMu_interaction.rda') })
reModsOrig = lapply(intModPaths, 
  function(x){load(x);return(mods)})
names(reModsOrig) = dvs

# get coef summaries for both fe and re mods
reModsOrig = lapply(reModsOrig, function(mod){
	rubinCoef(mod, modType='re') })
################################################################

################################################################
# vars
dvs = c('humanitarianTotal', 'developTotal', 'civSocietyTotal')
dvNames = paste0(
  c('Humanitarian', 'Development', 'Civil Society'), ' Aid')
cntrlVars=c(
  'Lno_disasters',
  paste0('L',dvs),
  'colony', 'Lpolity2',
  'LlnGdpCap', 'LlifeExpect', 'Lcivwar' )
cntrlVarNames = c(
  'No. Disasters$_{r,t-1}$',    
  rep('Lagged DV',3),   
  'Former Colony$_{sr,t-1}$',
  'Polity$_{r,t-1}$',
  'Log(GDP per capita)$_{r,t-1}$',
  'Life Expectancy$_{r,t-1}$',
  'Civil War$_{r,t-1}$'
  )

varsInt=c(
  'LstratMu', cntrlVars[1], 'LstratMu:Lno_disasters', cntrlVars[-1])
varNamesInt = c('Strategic Distance$_{sr,t-1}$', cntrlVarNames[1],
  'Strategic Distance$_{sr,t-1}$\n $\\times$ No. Disasters$_{r,t-1}$', 
  cntrlVarNames[-1])
################################################################

################################################################
modSumm=list(
	reMods[[1]][2:nrow(reMods[[1]]),],
	reModsOrig[[1]][2:nrow(reModsOrig[[1]]),],
	reMods[[2]][2:nrow(reMods[[2]]),],
	reModsOrig[[2]][2:nrow(reModsOrig[[2]]),],
	reMods[[3]][2:nrow(reMods[[3]]),],
	reModsOrig[[3]][2:nrow(reModsOrig[[3]]),]
	)
names(modSumm) = c(
	paste0(c('re_','reOrig_'), dvs[1]),
	paste0(c('re_','reOrig_'), dvs[2]),
	paste0(c('re_','reOrig_'), dvs[3])
	)
################################################################

################################################################
# org data for coef plot
coefp_colors = c("Positive"=rgb(54, 144, 192, maxColorValue=255), 
  "Negative"= rgb(222, 45, 38, maxColorValue=255),
  "Positive at 90"=rgb(158, 202, 225, maxColorValue=255), 
  "Negative at 90"= rgb(252, 146, 114, maxColorValue=255),
  "Insig" = rgb(150, 150, 150, maxColorValue=255))

summarizeMods = function(mods, dirtyVars, cleanVars){
  modSumm = lapply(1:length(mods), function(i){
    summ = mods[[i]]
    summ$dv = names(mods)[i]
    summ$up95 = with(summ, beta + qnorm(.975)*se)
    summ$lo95 = with(summ, beta - qnorm(.975)*se)
    summ$up90 = with(summ, beta + qnorm(.95)*se)
    summ$lo90 = with(summ, beta - qnorm(.95)*se)
    summ = summ[summ$var!='(Intercept)',]    
    summ$varClean = cleanVars[match(summ$var, dirtyVars)]
    tmp = gsub('re_|reOrig_','',summ$dv)
    summ$dvClean = dvNames[match(tmp, dvs)]
    summ$modelType = unlist(lapply(
    	strsplit(summ$dv,'_'),
    	function(x){x[1]}))
    summ$sig = NA
    summ$sig[summ$lo90 > 0 & summ$lo95 < 0] = "Positive at 90"
    summ$sig[summ$lo95 > 0] = "Positive"
    summ$sig[summ$up90 < 0 & summ$up95 > 0] = "Negative at 90"
    summ$sig[summ$up95 < 0] = "Negative"
    summ$sig[summ$lo90 < 0 & summ$up90 > 0] = "Insig"
    return(summ) }) %>% do.call('rbind', .)
  modSumm$varClean = factor(modSumm$varClean, levels=unique(rev(cleanVars)))
  modSumm$dvClean = factor(modSumm$dvClean, levels=dvNames[c(1,3,2)])
  return(modSumm) }

#
intModSumm = summarizeMods(modSumm, varsInt, varNamesInt)

# clean up model type labels
intModSumm$modelType[intModSumm$modelType=='re'] = 'Including Lagged DV'
intModSumm$modelType[intModSumm$modelType=='reOrig'] = 'Original'
intModSumm$modelType = factor(intModSumm$modelType,
  levels=unique(intModSumm$modelType)
  )

# Model results
plotRes = function(modSumm){
  # fix some labels
  xlabels = TeX(char(modSumm$varClean))
  xlabels[char(modSumm$varClean)==varNamesInt[3]] = expression( atop(
      'Strategic Distance' ['sr,t-1'],
      'x No. Disasters' ['r,t-1'] ) )

  # viz
  posDodge = .7
  ggplot(modSumm, aes(x=varClean, y=beta, color=sig, group=modelType)) +
    geom_hline(aes(yintercept=0), linetype='dashed', color='grey40') + 
    geom_point(aes(shape=modelType), size=2, position=position_dodge(width = posDodge)) + 
    geom_linerange(aes(ymin=lo95,ymax=up95), size=.3, position=position_dodge(width = posDodge)) + 
    geom_linerange(aes(ymin=lo90,ymax=up90), size=1, position=position_dodge(width = posDodge)) + 
    scale_color_manual(values=coefp_colors) +
    scale_x_discrete('',labels=xlabels) +    
    coord_flip() +
    facet_grid(~dvClean, scales='free_x') +
    labs( y='' ) +
    guides(
      colour=FALSE
      ) + 
    theme(
      axis.ticks = element_blank(), 
      panel.border=element_blank(),
      legend.position='bottom',
      legend.title=element_blank()
      ) }

intGG = plotRes(intModSumm)
# ggsave(intGG, 
#   file=paste0(pathGraphics, '/intCoef_lagDV.pdf'), 
#   width=8, height=5)
#########################################################

################################################################
# run models for sub effects
cl=makeCluster(3) ; registerDoParallel(cl)

# results consistent with other imputed datasets
regData = iData[[1]] 
stratMuIntMods = foreach(
  spec = reModSpecs, .packages=c('lme4') ) %dopar% {
  mod = lmer(spec, data=regData)
  return(mod)
}
stopCluster(cl)
names(stratMuIntMods) = dvs
################################################################

#########################################################
# sub effects
regData = iData[[1]]
noDisast = 4
simPlots = lapply(1:length(stratMuIntMods), function(i){
  mod = stratMuIntMods[[i]]
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
  pathGraphics, '/simComboPlot_lagDV.pdf'), width=8, height=8)
ggsave(simPlots[[1]], file=paste0(
  pathGraphics, '/simHumanitarianPlot_lagDV.pdf'), width = 7, height = 4)
ggsave(simPlots[[2]], file=paste0(
  pathGraphics, '/simDevelopmentPlot_lagDV.pdf'), width = 7, height = 4)
ggsave(simPlots[[3]], file=paste0(
  pathGraphics, '/simCivilPlot_lagDV.pdf'), width = 7, height = 4)
#########################################################

#########################################################
# aid dependence lag structure

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
cl=makeCluster(3) ; registerDoParallel(cl)
stratMuIntMods_3 = foreach(
  spec = reModSpecs, .packages=c('lme4') ) %dopar% {
    mod = lmer(spec, data=regData_3)
    return(mod) } ; stopCluster(cl)
names(stratMuIntMods_3) = dvs

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
cl=makeCluster(3) ; registerDoParallel(cl)
stratMuIntMods_5 = foreach(
  spec = reModSpecs, .packages=c('lme4') ) %dopar% {
    mod = lmer(spec, data=regData_5)
    return(mod) } ; stopCluster(cl)
names(stratMuIntMods_5) = dvs
################################################################


#########################################################