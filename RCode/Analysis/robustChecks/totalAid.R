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
totAidSummRE
################################################################