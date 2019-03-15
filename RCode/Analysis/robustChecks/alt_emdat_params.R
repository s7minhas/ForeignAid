if(Sys.info()['user']=='s7m' | Sys.info()['user']=='janus829'){
	source('~/Research/ForeignAid/RCode/setup.R') }
if(Sys.info()['user']=='cindycheng'){
	source('~/Documents/Papers/ForeignAid/RCode/setup.R') }
################################################################

################################################################
# Load reg data
load(paste0(pathData, "/iDataDisagg_v3.rda"))
################################################################

################################################################
# specs for models
covarsOrig = c(
	'LstratMu', 'Lno_disasters', 
	'LstratMu * Lno_disasters',
	'colony', 'Lpolity2', 'LlnGdpCap', 
	'LlifeExpect', 'Lcivwar' )
covarsRev = c(
	'LstratMu', 'log(Ltotal_dam +1)', 
	'LstratMu * log(Ltotal_dam +1)',
	'colony', 'Lpolity2', 'LlnGdpCap', 
	'LlifeExpect', 'Lcivwar' )
struc = c('+ (1|id) + (1|year)')
dvs = c(
	'humanitarianTotal',
	'civSocietyTotal',
	'developTotal'
	)
formsOrig = lapply(dvs, function(y){
	formula( paste0(y, ' ~ ', 
			paste(covarsOrig, collapse=' + '),
			struc ) ) })
formsRev = lapply(dvs, function(y){
	formula( paste0(y, ' ~ ', 
			paste(covarsRev, collapse=' + '), 
			struc ) ) })

# run models
modsOrig = lapply(formsOrig, function(form){
	mods = lapply(iData, function(data){
		mod = lmer(form, data=data) })
	return( rubinCoef(mods) ) })
names(modsOrig) = dvs

modsRev = lapply(formsRev, function(form){
	mods = lapply(iData, function(data){
		mod = lmer(form, data=data) })
	return( rubinCoef(mods) ) })
names(modsRev) = dvs

lapply(modsOrig, function(x){ x[c(2:3,nrow(x)),] })
lapply(modsRev, function(x){ x[c(2:3,nrow(x)),] })
################################################################