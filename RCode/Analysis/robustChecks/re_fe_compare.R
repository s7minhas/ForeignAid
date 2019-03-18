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
round(
	summary(humMod)$'coefficients'[
	c(2:3,nrow(summary(humMod)$'coefficients')),],
	3)
round(
	summary(humModFE)$'coefficients'[
	c(1:2,nrow(summary(humModFE)$'coefficients')),],
	3)

round(
	summary(civMod)$'coefficients'[
	c(2:3,nrow(summary(civMod)$'coefficients')),],
	3)
round(
	summary(civModFE)$'coefficients'[
	c(1:2,nrow(summary(civModFE)$'coefficients')),],
	3)

round(
	summary(devMod)$'coefficients'[
	c(2:3,nrow(summary(devMod)$'coefficients')),],
	3)
round(
	summary(devModFE)$'coefficients'[
	c(1:2,nrow(summary(devModFE)$'coefficients')),],
	3)
################################################################


################################################################
# switch to interaction mod
regData = iData[[1]]
noDisast = 4
simPlots = lapply(1:length(stratMuIntMods), function(i){
  mod = stratMuIntMods[[i]][[1]]
  modTitle = dvNames[i] ; var = 'LstratMu'

  # Create scenario matrix
  stratQts = quantile(regData[,var], probs=c(.05,.95), na.rm=TRUE)
  stratRange=with(data=regData, seq(stratQts[1], stratQts[2], .01) )
  disRange=with(data=regData, seq(
    min(Lno_disasters), noDisast, 2) )  
  scen = with(data=regData, 
    expand.grid(
      1, stratRange, disRange, 
      median(colony,na.rm=TRUE), median(Lpolity2,na.rm=TRUE), 
      median(LlnGdpCap,na.rm=TRUE), 
      median(LlifeExpect,na.rm=TRUE),median(Lcivwar,na.rm=TRUE)
      ) )

  # Add interaction term
  scen = cbind( scen, scen[,2]*scen[,3] )
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
    geom_rug(data=actData, 
    	aes(x=LstratMu,y=min(ggDataSmall$fit)), sides='b', alpha=.1) +
    facet_grid(
    	~Lno_disasters, 
    	labeller=as_labeller(
    		facet_labeller, default = label_parsed)) +
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
 simPlots[[3]]
ggsave(simComboPlot, 
	file=paste0(
		pathGraphics, '/simComboPlot.pdf'), 
	width=8, height=8)
ggsave(simPlots[[1]], 
	file=paste0(
		pathGraphics, '/simHumanitarianPlot.pdf'), 
	width = 7, height = 4)
ggsave(simPlots[[2]], 
	file=paste0(
		pathGraphics, '/simDevelopmentPlot.pdf'), 
	width = 7, height = 4)
ggsave(simPlots[[3]], 
	file=paste0(
		pathGraphics, '/simCivilPlot.pdf'), 
	width = 7, height = 4)
################################################################