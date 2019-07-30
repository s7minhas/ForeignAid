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
## the .rda below is created in latVarUncertainty.R
load( paste0(pathData, 'iData_for_r3.rda') )
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
		'LallyDist', 
    'LigoDist',
    'LunDist',
    'Lno_disasters', 
		'colony', 
		'Lpolity2', 'LlnGdpCap', 'LlifeExpect',
		'Lcivwar'
	), collapse=' + ' )
reStruc = '+ (1|id) + (1|year)'

# set up formulas
reModSpecs = lapply(dvs, function(y){
	formula(paste0(y, '~', baseSpec, reStruc)) })

# stdz vars to allow comparison across
vars = unlist(strsplit(baseSpec, ' + ', fixed=TRUE))
iData = lapply(iData, function(df){
  for(v in vars){
    df[,v] = (df[,v]-mean(df[,v], na.rm=TRUE))/sd(df[,v],na.rm=TRUE)
  }
  return(df) })
################################################################

################################################################
# run models
if(!file.exists(
	paste0(pathResults, '/reMods_indivLatVars.rda')
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
		file=paste0(pathResults, '/reMods_indivLatVars.rda')
		)
} else {
	load(paste0(pathResults, '/reMods_indivLatVars.rda'))
}
################################################################

################################################################
# vars
dvs = c('humanitarianTotal', 'developTotal', 'civSocietyTotal')
dvNames = paste0(
  c('Humanitarian', 'Development', 'Civil Society'), ' Aid')
cntrlVars=unlist(strsplit(baseSpec, ' + ', fixed=TRUE))
cntrlVarNames = c(
  'Strategic Ally Distance$_{sr,t-1}$',
  'Strategic IGO Distance$_{sr,t-1}$',
  'Strategic UN Distance$_{sr,t-1}$',
  'No. Disasters$_{r,t-1}$',    
  'Former Colony$_{sr,t-1}$',
  'Polity$_{r,t-1}$',
  'Log(GDP per capita)$_{r,t-1}$',
  'Life Expectancy$_{r,t-1}$',
  'Civil War$_{r,t-1}$',
  'Trade$_{sr,t-1}$'
  )

varsInt=cntrlVars
varNamesInt = cntrlVarNames
################################################################

################################################################
modSumm=lapply(reMods, function(mod){ mod[2:nrow(mod),] } )
names(modSumm) = names(reMods)
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
coefModSumm = summarizeMods(modSumm, varsInt, varNamesInt)
################################################################

################################################################
# Model results
plotRes = function(modSumm){
  # fix some labels
  xlabels = TeX(char(modSumm$varClean))

  # viz
  posDodge = .7
  ggplot(modSumm, aes(x=varClean, y=beta, color=sig)) +
    geom_hline(aes(yintercept=0), linetype='dashed', color='grey40') + 
    geom_point() + 
    geom_linerange(aes(ymin=lo95,ymax=up95), size=.3) + 
    geom_linerange(aes(ymin=lo90,ymax=up90), size=1) + 
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

ggCoef = plotRes(coefModSumm)
ggsave(ggCoef, 
  file=paste0(pathGraphics, '/intCoef_indivLatVars.pdf'), 
  width=8, height=4)
#########################################################