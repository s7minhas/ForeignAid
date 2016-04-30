if(Sys.info()['user']=='s7m' | Sys.info()['user']=='janus829'){ source('~/Research/ForeignAid/RCode/setup.R') }
if(Sys.info()['user']=='cindycheng'){ source('~/Documents/Papers/ForeignAid/RCode/setup.R') }

################################################################
# Load reg data
setwd(pathData)
load('iData.rda')
################################################################

################################################################
# RE model

## mod formula
vars=c(
	'LstratMu', # state interest measure
	'colony' # Colonial variable
	,'Lpolity2' # Institutions
	,'LlnGdpCap' # Macroecon controls
	,'LlifeExpect', 'Lno_disasters' # Humanitarian
	,'Lcivwar' # Civil war
	)

## Run model on full sample
### Results consistent across various specifications
modForm=formula(paste0(
	'commitUSD13 ~ ', paste(vars, collapse=' + '), 
	'+ (1|ccodeS) + (1|ccodeR)'))	# Sender + receiver random effects	

# Various models in parallel
ziLog = rep(c('FALSE','TRUE'),4)
fam = rep(c('gaussian','gamma','nbinom','poisson'), each=2)


cl=makeCluster(8)
registerDoParallel(cl)
foreach(ii=1:length(fam), .packages=c('glmmADMB')) %dopar% {
	m=glmmadmb(modForm, data=regData, zeroInflation=ziLog[ii], family=fam[ii], extra.args="-ndi 100000")
	if(ziLog[ii]){ziName = '_zi'} else {ziName=''}
	save(m, file=paste0(pathResults, '/mod_', fam[ii], ziName, '.rda'))
}