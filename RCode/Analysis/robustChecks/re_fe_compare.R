if(Sys.info()['user']=='s7m' | Sys.info()['user']=='janus829'){
	source('~/Research/ForeignAid/RCode/setup.R') }
if(Sys.info()['user']=='cindycheng'){
	source('~/Documents/Papers/ForeignAid/RCode/setup.R') }
################################################################

################################################################
# Load reg data
load(paste0(pathData, '/iDataDisagg_wLags_v3.rda'))
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
# load already run fe models from fe_subeffects.R
load(paste0(pathResults, '/feMods_appendix.rda'))
################################################################

################################################################
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

# coef summaries
feMods = lapply(feMods, function(mod){
	mod$pval = 2*pnorm(-abs(mod$t))
	return(mod) } )
reMods = lapply(reMods, function(mod){
	mod$pval = 2*pnorm(-abs(mod$t))
	return(mod) } )
################################################################

################################################################
# make table for fe results
varDefTab = varDef

loadPkg('xtable')
names(feMods) = dvNames
lapply(feMods, function(x){
  rownames(x) = x$var
  x = x[,-which(names(x) %in% c('t','var'))]
  x = x[c(1:3,nrow(x),4:(nrow(x)-1)),]
  x = data.matrix(x)
  x = round(x, 2)
  colnames(x) = c('Estimate', 'Std. Error', 'P-value')
  rownames(x) = varDef[match(rownames(x),varDef[,1]),2]
  rownames(x)[1] = 'Lagged DV'
  out = print.xtable(
    xtable( x,
      align=c('lccc')
      ),
  sanitize.rownames.function=identity
  )
  return(out)
  } )
################################################################

################################################################
modSumm=list(
  feMods[[1]][match(varDef[,1], feMods[[1]]$var),],
  reMods[[1]][match(varDef[,1], reMods[[1]]$var),],
  feMods[[2]][match(varDef[,1], feMods[[2]]$var),],
  reMods[[2]][match(varDef[,1], reMods[[2]]$var),],
  feMods[[3]][match(varDef[,1], feMods[[3]]$var),],
  reMods[[3]][match(varDef[,1], reMods[[3]]$var),]
	)
names(modSumm) = c(
	paste0(c('fe_','re_'), dvs[1]),
	paste0(c('fe_','re_'), dvs[2]),
	paste0(c('fe_','re_'), dvs[3])
	)
################################################################

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
    tmp = gsub('fe_|re_','',summ$dv)
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
intModSumm$modelType[intModSumm$modelType=='fe'] = 'FE'
intModSumm$modelType[intModSumm$modelType=='re'] = 'RE'
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
    facet_grid(modelType~dvClean, scales='free_x') +
    labs( y='' ) +
    theme(
      axis.ticks = element_blank(), 
      panel.border=element_blank(),
      legend.position='none' ) }

intGG = plotRes(intModSumm)
ggsave(intGG, 
  file=paste0(pathGraphics, '/intCoef_fe_re_compare.pdf'), 
  width=8, height=6)
#########################################################