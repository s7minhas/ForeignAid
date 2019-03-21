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