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
	c(
		'LstratMu', 'Lno_disasters', 
		'LstratMu * Lno_disasters', 'colony', 
		'Lpolity2', 'LlnGdpCap', 'LlifeExpect',
		'Lcivwar'
	), collapse=' + ' )
feStruc = '+ factor(id) + factor(year) - 1'

# set up formulas
feModSpecs = lapply(dvs, function(y){
	formula(paste0(y, '~', baseSpec, feStruc)) })
################################################################

################################################################
# run models
# if(!file.exists(
# 	paste0(pathResults, '/feMods_robustCheck.rda')
# 	)){
	cl=makeCluster(5) ; registerDoParallel(cl)

	# run fixed effect models
	## humanitarian model
	humModFE= foreach(df=iData) %dopar% {
		summary(
			lm(feModSpecs[[1]], data=df)
			)$'coefficients' }

	## civ society model
	civModFE = foreach(df=iData) %dopar% {
		summary(
			lm(feModSpecs[[2]], data=df)
			)$'coefficients' }

	## dev model
	devModFE = foreach(df=iData) %dopar% {
		summary(
			lm(feModSpecs[[3]], data=df)
			)$'coefficients' }

	#
	stopCluster(cl)

	#
	feMods = list(
		humModFE, devModFE, civModFE )
	names(feMods) = dvs
# 	save(feMods, 
# 		file=paste0(pathResults, '/feMods_robustCheck.rda')
# 		)
# } else {
	# load(paste0(pathResults, '/feMods_robustCheck.rda'))
# }
################################################################

################################################################
# load re model for comparison
dvs = c('humanitarianTotal', 'developTotal', 'civSocietyTotal')
dvNames = paste0(
  c('Humanitarian', 'Development', 'Civil Society'), ' Aid')
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
	rubinCoef(mod, modType='fe') })
feMods = feMods[dvs]

feMods = lapply(feMods, function(mod){
	mod$pval = round(2*pnorm(-abs(mod$t)),3)
	return(mod[c(1,2,nrow(mod)),c(ncol(mod)-1,1,ncol(mod))]) } )
reMods = lapply(reMods, function(mod){
	mod$pval = round(2*pnorm(-abs(mod$t)),3)
	return(mod[c(2,3,nrow(mod)),c(ncol(mod)-1,1,ncol(mod))]) } )

for(ii in 1:length(feMods)){
	print(feMods[ii])
	print(reMods[ii])
}
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
varsNoInt=c('LstratMu', cntrlVars)
varNamesNoInt = c('Strategic Distance$_{sr,t-1}$', cntrlVarNames)
varsInt=c(
  'LstratMu', cntrlVars[1], 'LstratMu:Lno_disasters', cntrlVars[-1])
varNamesInt = c('Strategic Distance$_{sr,t-1}$', cntrlVarNames[1],
  'Strategic Distance$_{sr,t-1}$\n $\\times$ No. Disasters$_{r,t-1}$', 
  cntrlVarNames[-1])
################################################################

################################################################
# Model results
digs=3; noModels=length(modSumm)
tableResults = matrix('', nrow=2*length(varDef[,1]), ncol=1+noModels)
tableResults[,1] = rep(varDef[,1],2)
colnames(tableResults) = c('Variable',paste0('Model ',1:noModels))
for(ii in 2:ncol(tableResults)){
	temp = modSumm[[ii-1]]
	n = modResults[[ii-1]]$df.residual
	temp = temp[match(tableResults[,'Variable'], rownames(temp)),]
	estims = temp[1:nrow(varDef),'Estimate']
	estims = round(as.numeric(as.character(estims)),digs)
	tvals = abs(temp[1:nrow(varDef),'t value'])
	tvals = round(as.numeric(as.character(tvals)),digs)
	estims = ifelse(tvals>=qt(0.975,n) & !is.na(tvals) & tvals<qt(0.995,n), 
		paste('$', estims,'^{\\ast}$',sep=''), estims)
	estims = ifelse(tvals>=qt(0.995,n) & !is.na(tvals), 
		paste('$', estims,'^{\\ast\\ast}$',sep=''), estims)	
	estims = ifelse(is.na(estims),'',estims)
	tableResults[1:nrow(varDef),ii] = estims
	serrors = temp[(nrow(varDef)+1):nrow(tableResults),'Std. Error']
	serrors = round(as.numeric(as.character(serrors)),digs)
	serrors = paste('(',serrors,')',sep='')
	serrors = ifelse(serrors=='(NA)','',serrors)
	tableResults[(nrow(varDef)+1):nrow(tableResults),ii] = serrors
}

# Reorganizing rows and variable labels
tableFinal = NULL
for(ii in 1:nrow(varDef)){
	temp = cbind('', t(tableResults[ii+nrow(varDef),2:ncol(tableResults)]))
	tableFinal = rbind(tableFinal, tableResults[ii,], temp) }

# Adding other info
sSize = cbind('n', t(as.vector(mapply(x=modResults, 
	function(x) FUN=length(x$residuals)))))
gSize = cbind('N', t(as.vector(mapply(x=modResults, 
	function(x) FUN=length(x$residuals)-x$df.residual-length(x$coefficient)))))
rSQ = cbind('$R^{2}$', t(as.vector(mapply(x=modResults,
		function(x) FUN=round(summary(x)$r.squared[1],2) ))))
arSQ = cbind('Adj. $R^{2}$', t(as.vector(mapply(x=modResults,
		function(x) FUN=round(summary(x)$r.squared[2],2) ))))
rmse = round(mapply(x=modResults, function(x) FUN=sqrt(mean(x$residuals^2))),2)
fRmse = cbind('RMSE', t(rmse))
tableFinal = rbind(tableFinal, sSize, gSize, rSQ, arSQ, fRmse)
nStats=5
temp=varDef[match(tableFinal[,'Variable'], varDef[,1]),2]
temp[which(is.na(temp))]=tableFinal[,'Variable'][which(is.na(temp))]
tableFinal[,'Variable']=temp

# Add & before every period
tableFinal[,2:ncol(tableFinal)]=apply(tableFinal[,2:ncol(tableFinal)], c(1,2), 
	function(x){ 
		if( grepl('\\$', x) ){ gsub('\\$*\\.', '$&$.', x)
		} else { gsub('\\.', '&.', x) } })

setwd(pathLatex)
print.xtable(xtable(tableFinal, align='llcccccc', caption=captionTable),
	include.rownames=FALSE,
	sanitize.text.function = identity,
	hline.after=c(0,0,nrow(varDef)*2,nrow(varDef)*2+nStats,nrow(varDef)*2+nStats),
	size="footnotesize",	
	file=fileTable )
################################################################