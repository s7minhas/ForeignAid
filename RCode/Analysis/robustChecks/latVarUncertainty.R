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
################################################################

################################################################
# add extra vars for reviewers

if(!file.exists( paste0(pathData, 'iData_for_r3.rda') )){
  # PCA variable
  load(paste0(pathResults, '/PCA/PCA_FullData_allyIGOUN.rda'))
  stratData=PCA_FullData$PCA_AllYrs; rm(list='PCA_FullData')

  # component vars
  gPth=paste0(pathResults, "/gbmeLatDist/")
  load(paste0(gPth, 'allyWtDist.rda')) ; allyDist = data.frame(res)
  load(paste0(gPth, 'igoDist.rda')) ; igoDist = data.frame(res)
  load(paste0(gPth, 'unNewDist.rda')) ; unDist = data.frame(res)
  load(paste0(pathData, '/cow_trade/trade.rda')) # returns `trade`
  names(trade)[1:2] = paste0('ccode',1:2)

  # Add ids to various frames
  addIDs = function(x){
    x$id = with(x, paste0(ccode1, 9999, ccode2) ) ; x$id = num(x$id)
    x$idYr = with(x, paste0(id, year)) ; x$idYr = num(x$idYr)
    return(x) }
  stratData = addIDs(stratData) ; allyDist = addIDs(allyDist)
  igoDist = addIDs(igoDist) ; unDist = addIDs(unDist)
  trade = addIDs(trade)

  # merge dyad vars
  stratData$allyDist = allyDist$allyDist[match(
    stratData$idYr, allyDist$idYr)]
  stratData$igoDist = igoDist$igoDist[match(
    stratData$idYr, igoDist$idYr)]
  stratData$unDist = unDist$unDist[match(
    stratData$idYr, unDist$idYr)]
  stratData$trade = trade$trade[match(
    stratData$idYr, trade$idYr)]
  stratData$trade = log(stratData$trade + 1)

  # minor cleanup
  names(stratData)[4:6]=paste0('strat',c('StratMu','StratUp','StratLo'))

  # lag
  stratData=lagData(stratData, 
    'idYr', 'id', 
    names(stratData)[c(5:6,9:12)] )

  # merge in with iData
  vars = paste0('L',c(
    'stratStratUp','stratStratLo',
    'allyDist','igoDist','unDist','trade'
    ))
  iData = lapply(iData, function(df){

    # add dyadic ids
    df$did = with(df, 
      paste(ccodeS, ccodeR, year, sep='_'))
    stratData$did = with(stratData, 
      paste(ccode1, ccode2, year, sep='_'))
    
    # merge in vars
    for(v in vars){
      df$v = stratData[match(df$did,stratData$did),v]
      names(df)[ncol(df)] = v }
    
    # remove some empty dayds
    df = df[!is.na(df$LstratStratUp),]
    
    #
    return(df) })

  save(iData, file=paste0(pathData, 'iData_for_r3.rda'))
} else {
  load( paste0(pathData, 'iData_for_r3.rda') ) }

# results similar across imputed datasets
iData = iData[[length(iData)]]

# account for uncertainty in lat var
iData$err = (iData$LstratMu-iData$LstratStratLo)/qnorm(.975)
iData = lapply(1:100, function(i){
  set.seed(i)
  iData$LstratMu = rnorm(nrow(iData),iData$LstratMu,iData$err)
  return(iData) })
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
	formula(paste0(y, '~', baseSpec, reStruc)) })
################################################################

################################################################
# run models
if(!file.exists(
	paste0(pathResults, '/reMods_latVarUncert.rda')
	)){
	cl=makeCluster(5) ; registerDoParallel(cl)

	# run fixed effect models
	## humanitarian model
	humModRE= foreach(df=iData, .packages=c('lme4')) %dopar% {
		summary(
			lmer(reModSpecs[[1]], data=df)
			)$'coefficients' }

	## civ society model
	devModRE = foreach(df=iData) %dopar% {
		summary(
			lmer(reModSpecs[[2]], data=df)
			)$'coefficients' }

	## dev model
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
		file=paste0(pathResults, '/reMods_latVarUncert.rda')
		)
} else {
	load(paste0(pathResults, '/reMods_latVarUncert.rda'))
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

varsInt=c(
  'LstratMu', cntrlVars[1], 'LstratMu:Lno_disasters', cntrlVars[-1])
varNamesInt = c('Strategic Distance$_{sr,t-1}$', cntrlVarNames[1],
  'Strategic Distance$_{sr,t-1}$\n $\\times$ No. Disasters$_{r,t-1}$', 
  cntrlVarNames[-1])

varDef = cbind(varsInt, varNamesInt)
################################################################

################################################################
modSumm=list(
	reMods[[1]][c(1:7,nrow(reMods[[1]])),],
	reModsOrig[[1]][2:nrow(reModsOrig[[1]]),],
	reMods[[2]][c(1:7,nrow(reMods[[2]])),],
	reModsOrig[[2]][2:nrow(reModsOrig[[2]]),],
	reMods[[3]][c(1:7,nrow(reMods[[3]])),],
	reModsOrig[[3]][2:nrow(reModsOrig[[3]]),]
	)
names(modSumm) = c(
	paste0(c('re_','reOrig_'), dvs[1]),
	paste0(c('re_','reOrig_'), dvs[2]),
	paste0(c('re_','reOrig_'), dvs[3])
	)
################################################################

# 
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
  modSumm$varClean = factor(modSumm$varClean, levels=rev(cleanVars))
  modSumm$dvClean = factor(modSumm$dvClean, levels=dvNames[c(1,3,2)])
  return(modSumm) }

#
intModSumm = summarizeMods(modSumm, varsInt, varNamesInt)

# clean up model type labels
intModSumm$modelType[intModSumm$modelType=='re'] = 'Uncertainty in Latent Variable'
intModSumm$modelType[intModSumm$modelType=='reOrig'] = 'Original'
intModSumm$modelType = factor(intModSumm$modelType,
  levels=unique(intModSumm$modelType)
  )
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
ggsave(intGG, 
  file=paste0(pathGraphics, '/intCoef_latVarUncert.pdf'), 
  width=8, height=5)
#########################################################