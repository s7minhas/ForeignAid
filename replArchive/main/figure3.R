source('setup.R')
################################################################

################################################################
# Load reg data
load('intake/iDataDisagg_wLags_v3.rda')
################################################################

################################################################
# specs for models
vars = c(
  'LstratMu', 'Lno_disasters', 
  'LstratMu * Lno_disasters',
  'colony', 'Lpolity2', 'LlnGdpCap', 
  'LlifeExpect', 'Lcivwar' )
struc = c('+ (1|id) + (1|year)')
dvs = c(
  'humanitarianTotal',
  'developTotal',
  'civSocietyTotal' )

#
forms = lapply(dvs, function(y){
  formula( paste0(y, ' ~ ', 
                  paste(vars, collapse=' + '), 
                  struc ) ) })

# run models
if(!file.exists('mods/mainMod.rda')){
  stratMuIntMods = lapply(forms, function(form){
    mods = lapply(iData, function(data){
      mod = lmer(form, data=data) })
    return( mods ) })
  names(stratMuIntMods) = dvs
  
  # save model
  stratMuIntMods = storeResults(stratMuIntMods)
  save(stratMuIntMods, file='mods/mainMod.rda')  
} else { load('mods/mainMod.rda') }
################################################################

################################################################
# var names
dvNames = paste0(
  c('Humanitarian', 'Development', 'Civil Society'), ' Aid')

vars[3] = 'LstratMu:Lno_disasters'
varNames = c(
  'Pol. Strat. Distance$_{sr,t-1}$',
  'No. Disasters$_{r,t-1}$', 
  'Pol. Strat. Distance$_{sr,t-1}$\n $\\times$ No. Disasters$_{r,t-1}$',
  'Former Colony$_{sr,t-1}$',
  'Polity$_{r,t-1}$',
  'Log(GDP per capita)$_{r,t-1}$',
  'Life Expectancy$_{r,t-1}$',
  'Civil War$_{r,t-1}$'
)

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
intModSumm = summarizeMods(stratMuIntMods, vars, varNames)
################################################################

################################################################
# Model results
plotRes = function(modSumm){
  # fix some labels
  xlabels = TeX(char(modSumm$varClean))
  xlabels[char(modSumm$varClean)==varNames[3]] = expression( atop(
    'Pol. Strat. Distance' ['sr,t-1'],
    'x No. Disasters' ['r,t-1'] ) )
  
  # viz
  ggplot(modSumm, aes(x=varClean, y=beta, color=sig)) +
    geom_hline(aes(yintercept=0), linetype='dashed', color='grey40') + 
    geom_point() +
    geom_linerange(aes(ymin=lo95,ymax=up95), size=.3) +
    geom_linerange(aes(ymin=lo90,ymax=up90), size=1) +
    scale_color_manual(values=coefp_colors_grey) +
    scale_x_discrete('',labels=xlabels) +    
    coord_flip() +
    facet_wrap(~dvClean, ncol=4, scales='free_x') +
    labs( y='' ) +
    theme(
      axis.ticks = element_blank(), 
      panel.border=element_blank(),
      legend.position='none' ) }

intGG = plotRes(intModSumm)
ggsave(intGG, file='floats/figure3.pdf', width=8, height=6)
#########################################################