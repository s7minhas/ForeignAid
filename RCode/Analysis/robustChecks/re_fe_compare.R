if(Sys.info()['user']=='s7m' | Sys.info()['user']=='janus829'){
	source('~/Research/ForeignAid/RCode/setup.R') }
if(Sys.info()['user']=='cindycheng'){
	source('~/Documents/Papers/ForeignAid/RCode/setup.R') }
################################################################

################################################################
# Load reg data
load(paste0(pathData, '/iDataDisagg_wLags_v3.rda'))
dvs = c(
	'humanitarianTotal',
	'civSocietyTotal',
	'developTotal' )
baseSpec = paste(
	c( 'LstratMu', 'Lno_disasters', 
		'LstratMu * Lno_disasters', 'colony', 
		'Lpolity2', 'LlnGdpCap', 'LlifeExpect',
		'Lcivwar' ), collapse=' + ' )
reStruc = '(1|id) + (1|year)'
feStruc = 'factor(id) + factor(year) - 1'

# set up formulas
reModSpecs = lapply(dvs, function(y){
	formula(paste0(y, '~', baseSpec, '+', reStruc)) })
feModSpecs = lapply(dvs, function(y){
	formula(paste0(y, '~', baseSpec, '+', feStruc)) })
################################################################

################################################################
# run random effect models
cl=makeCluster(5) ; registerDoParallel(cl)

## humanitarian model
humModRE = foreach(df=iData, .packages=c('lme4')) %dopar% {
	lmer(reModSpecs[[1]], data=df) }
humSummRE = rubinCoef(humModRE)

## civ society model
civModRE = foreach(df=iData, .packages=c('lme4')) %dopar% {
	lmer(reModSpecs[[2]], data=df) }
civSummRE = rubinCoef(civModRE)

## dev model
devModRE = foreach(df=iData, .packages=c('lme4')) %dopar% {
	lmer(reModSpecs[[3]], data=df) }
devSummRE = rubinCoef(devModRE)

# run fixed effect models
## humanitarian model
humModFE= foreach(df=iData) %dopar% {
	lm(feModSpecs[[1]], data=df) }
# humSummFE = rubinCoef(humModFE)

## civ society model
civModFE = foreach(df=iData) %dopar% {
	lm(feModSpecs[[2]], data=df) }
# civSummFE = rubinCoef(civModFE)

## dev model
devModFE = foreach(df=iData) %dopar% {
	lm(feModSpecs[[3]], data=df) }
# devSummFE = rubinCoef(devModFE)

#
stopCluster(cl)
################################################################

################################################################
# coef summaries
# set.seed(2342) ; impN = sample(1:4, 1)
# round(
# 	summary(humModRE[[impN]])$'coefficients'[
# 	c(2:3,nrow(summary(humModRE[[1]])$'coefficients')),],
# 	3)
# round(
# 	summary(humModFE[[impN]])$'coefficients'[
# 	c(1:2,nrow(summary(humModFE[[1]])$'coefficients')),],
# 	3)

# round(
# 	summary(civModRE[[impN]])$'coefficients'[
# 	c(2:3,nrow(summary(civModRE[[1]])$'coefficients')),],
# 	3)
# round(
# 	summary(civModFE[[impN]])$'coefficients'[
# 	c(1:2,nrow(summary(civModFE[[1]])$'coefficients')),],
# 	3)

# round(
# 	summary(devModRE[[impN]])$'coefficients'[
# 	c(2:3,nrow(summary(devModRE[[1]])$'coefficients')),],
# 	3)
# round(
# 	summary(devModFE[[impN]])$'coefficients'[
# 	c(1:2,nrow(summary(devModFE[[1]])$'coefficients')),],
# 	3)

reMods = list(
	rubinCoef(humModRE),
	rubinCoef(civModRE),
	rubinCoef(devModRE)
	)

feMods = list(
	rubinCoef(humModFE, modType='fe'),
	rubinCoef(civModFE, modType='fe'),
	rubinCoef(devModFE, modType='fe')	
	)

reMods
feMods
################################################################

################################################################
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
varsInt=c(
  'LstratMu', cntrlVars[1], 'LstratMu:Lno_disasters', cntrlVars[-1])
varNamesInt = c('Strategic Distance$_{sr,t-1}$', cntrlVarNames[1],
  'Strategic Distance$_{sr,t-1}$\n $\\times$ No. Disasters$_{r,t-1}$', 
  cntrlVarNames[-1])

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
    summ$up95 = with(summ, beta + qnorm(.975)*se)
    summ$lo95 = with(summ, beta - qnorm(.975)*se)
    summ$up90 = with(summ, beta + qnorm(.95)*se)
    summ$lo90 = with(summ, beta - qnorm(.95)*se)
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

intGG = plotRes(intModSumm)
ggsave(intGG, 
  file=paste0(pathGraphics, '/intCoef.pdf'), width=8, height=6)
################################################################