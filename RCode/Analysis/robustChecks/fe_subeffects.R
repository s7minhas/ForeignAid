if(Sys.info()['user']=='s7m' | Sys.info()['user']=='janus829'){
	source('~/Research/ForeignAid/RCode/setup.R') }
if(Sys.info()['user']=='cindycheng'){
	source('~/Documents/Papers/ForeignAid/RCode/setup.R') }
################################################################

################################################################
# Load reg data
load(paste0(pathData, '/iDataDisagg_wLags_v3.rda'))

# create dv lags
iData = lapply(iData, function(df){
  # gen some ids
  df$id = with(df, paste0(ccodeS, 9999, ccodeR) )
  df$id = num(df$id)
  df$idYr = with(df, paste0(id, year))
  df$idYr = num(df$idYr)

  # lag
  df = lagData(df, 
    'idYr','id', 
    c(
      'humanitarianTotal', 
      'developTotal',
      'civSocietyTotal'
      )
    )

  return(df) })
################################################################

################################################################
# Load reg data
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
feStruc = '+ factor(id) + factor(year) - 1'

# set up formulas
feModSpecs = lapply(dvs, function(y){
	formula(paste0(y, '~', paste0('L',y), '+', baseSpec, feStruc)) })
################################################################

################################################################
# run models
cl=makeCluster(5) ; registerDoParallel(cl)

# run fixed effect models
## humanitarian model
humModFE= foreach(df=iData) %dopar% {
	# summary(
		lm(feModSpecs[[1]], data=df)
		# )$'coefficients'
}

## civ society model
civModFE = foreach(df=iData) %dopar% {
	# summary(
		lm(feModSpecs[[2]], data=df)
		# )$'coefficients'
}

## dev model
devModFE = foreach(df=iData) %dopar% {
	# summary(
		lm(feModSpecs[[3]], data=df)
		# )$'coefficients'
}

#
stopCluster(cl)

#
# feMods = list(
# 	rubinCoef(humModFE,'fe'), 
# 	rubinCoef(devModFE,'fe'), 
# 	rubinCoef(civModFE,'fe')
# 	)

# names(feMods) = dvs
################################################################

################################################################
# 
################################################################