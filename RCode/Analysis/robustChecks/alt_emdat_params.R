if(Sys.info()['user']=='s7m' | Sys.info()['user']=='janus829'){
	source('~/Research/ForeignAid/RCode/setup.R') }
if(Sys.info()['user']=='cindycheng'){
	source('~/Documents/Papers/ForeignAid/RCode/setup.R') }
################################################################

################################################################
# Load reg data
load(paste0(pathData, '/iDataDisagg_wLags_v3.rda'))

vars = names(iData[[1]])[28:30]
iData = lapply( iData, function(df){
	for(v in vars){ df[,v] = log(df[,v] + 1) }
	return(df) })
################################################################

################################################################
# specs for models
covarsRev = c(
	'LstratMu', 'Lno_killed', 
	'LstratMu * Lno_killed',
	'colony', 'Lpolity2', 'LlnGdpCap', 
	'LlifeExpect', 'Lcivwar' )
struc = c('+ (1|id) + (1|year)')
dvs = c(
	'humanitarianTotal',
	'civSocietyTotal',
	'developTotal'
	)

#
formsRev = lapply(dvs, function(y){
	formula( paste0(y, ' ~ ', 
			paste(covarsRev, collapse=' + '), 
			struc ) ) })

# run models
modsRev_full = lapply(formsRev, function(form){
	mods = lapply(iData, function(data){
		mod = lmer(form, data=data) })
	return( mods ) })
names(modsRev_full) = dvs
modsRev = lapply(modsRev_full,rubinCoef,matrixFormat=TRUE)
################################################################

#########################################################
# switch to interaction mod
regData = iData[[1]]
noDisast = 12
stratMuIntMods = modsRev_full
dvNames = paste0(
  c('Humanitarian', 'Civil Society', 'Development'), ' Aid')

simPlots = lapply(1:length(stratMuIntMods), function(i){
	mod = stratMuIntMods[[i]][[1]]
	modTitle = dvNames[i] ; var = 'LstratMu'

	# Create scenario matrix
	stratQts = quantile(regData[,var], probs=c(.05,.95), na.rm=TRUE)
	stratRange=with(data=regData, seq(stratQts[1], stratQts[2], .01) )
	# with(regData, quantile(Lno_killed[Lno_disasters>0], probs=c(0,.5,1)))
	disRange=c(0, 4, 12)
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
			scen[,var], scen[,'Lno_killed'])
		)
	names(ggData)=c(
		'fit', 'sysLo95', 'sysHi95', 
		'sysLo90', 'sysHi90', var, 'Lno_killed'
		)

	# Plot rel at various cuts of disasters
	ggDataSmall = ggData[which(ggData$Lno_killed %in% disRange),]

	# change facet labels
	lab = ' Deaths from Disasters$_{r,t-1}$'
	ggDataSmall$Lno_killed[
		ggDataSmall$Lno_killed==0] = paste0('Low', lab)
	ggDataSmall$Lno_killed[
		ggDataSmall$Lno_killed==4] = paste0('Medium', lab)
	ggDataSmall$Lno_killed[
		ggDataSmall$Lno_killed==12] = paste0('High', lab)
	ggDataSmall$Lno_killed = factor(
		ggDataSmall$Lno_killed,
		levels=paste0(c('Low','Medium','High'), lab)
		)	

	# viz
	facet_labeller = function(string){ TeX(string) }
	tmp=ggplot(ggDataSmall, aes(x=LstratMu, y=fit)) +
	geom_line() +
	geom_ribbon(aes(ymin=sysLo90, ymax=sysHi90), alpha=.6) +
	geom_ribbon(aes(ymin=sysLo95, ymax=sysHi95), alpha=.4) +
	facet_grid(
	  ~Lno_killed, 
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
  simPlots[[1]], simPlots[[2]], simPlots[[3]],
  nrow=length(stratMuIntMods))
ggsave(
  simComboPlot, 
  file=paste0(
    pathGraphics,
    '/simComboPlot_no_killed.pdf'),
  width=8, height=8)
# #########################################################