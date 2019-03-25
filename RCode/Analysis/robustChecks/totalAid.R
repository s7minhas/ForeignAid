if(Sys.info()['user']=='s7m' | Sys.info()['user']=='janus829'){
	source('~/Research/ForeignAid/RCode/setup.R') }
if(Sys.info()['user']=='cindycheng'){
	source('~/Documents/Papers/ForeignAid/RCode/setup.R') }
################################################################

################################################################
# Load reg data
load(paste0(pathData, '/iDataDisagg_wLags_v3.rda'))

# add total aid variable
iData = lapply( iData, function(df){
	df$totAid = sum(
		df$humanitarianTotal,
		df$civSocietyTotal,
		df$developTotal
		)
	return(df) })

# mod specs
dv = c( 'totAid' )
baseSpec = paste(
	c( 'LstratMu', 'Lno_disasters', 
		'LstratMu * Lno_disasters', 'colony', 
		'Lpolity2', 'LlnGdpCap', 'LlifeExpect',
		'Lcivwar' ), collapse=' + ' )
reStruc = '(1|id) + (1|year)'
feStruc = 'factor(id) + factor(year) - 1'

# set up formulas
reModSpec = formula(paste0(dv, '~', baseSpec, '+', reStruc))
################################################################

################################################################
# run random effect models
## total aid
cl=makeCluster(5) ; registerDoParallel(cl)
totAidModRE = foreach(df=iData, .packages=c('lme4')) %dopar% {
	lmer(reModSpec, data=df) }
stopCluster(cl)	
totAidSummRE = rubinCoef(totAidModRE)
################################################################

################################################################
## load re model for comparison and organize into one list

# dv labels
dvs = c(
	'humanitarianTotal',
	'developTotal',
	'civSocietyTotal'
	)
dvNames = paste0(
  c('Humanitarian', 'Development', 'Civil Society'), ' Aid')

# load re model for comparison
intModPaths = lapply(dvs, function(dv){
  paste0(pathResults, '/', dv, 
    '_fullSamp_gaussian_re_LstratMu_interaction.rda') })
reMods = lapply(intModPaths, 
  function(x){load(x);return(mods)})
names(reMods) = dvs

# get coef summaries for both fe and re mods
reMods = lapply(reMods, function(mod){
	rubinCoef(mod, modType='re') })

# rescale totAidSumm coefs
totAidSummRE[,1:2] = totAidSummRE[,1:2] * 1e6

# organize
reMods = c(reMods, list(totAidSummRE))
names(reMods)[length(reMods)] = 'total'

# dvs
dvs = c(dvs,'total')
dvNames = c(dvNames, 'Total Aid')
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
    summ = mods[[i]]
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
  modSumm$dvClean = factor(modSumm$dvClean, levels=dvNames[c(1,3,2,4)])
  return(modSumm) }

#
intModSumm = summarizeMods(reMods, varsInt, varNamesInt)
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
  file=paste0(pathGraphics, '/totAid_Coef.pdf'), width=8, height=5)
################################################################