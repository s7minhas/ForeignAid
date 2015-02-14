if(Sys.info()["user"]=="janus829"){
	source("~/Research/ForeignAid/RCode/setup.R") }

################################################################
# Load reg data
setwd(pathData)
load('regData.rda')
regData = regData[regData$year>1974 & regData$year<=2010,]
################################################################

################################################################
# RE model
## mod formula
vars=c(
	'LstratMu', # state interest measure
	'colony', # Colonial variable
	'Lpolity2', # Institutions
	'LlnGdpCap', # Macroecon controls
	'LlifeExpect', 'Lno_disasters', # Humanitarian
	'Lcivwar'
	# ,'SLpolity2', 'SLlnGdpCap' 
	)

sVars=c('SLpolity2', 'SLlnGdpCap')

## Run model on full sample
modForm=formula(paste0(
	'logAid ~ ', paste(vars, collapse=' + '), 
	# ' + (1|ccodeS) + (1|year)' ))
	' + (1|year/ccodeS)' ))
	# ' + (', paste(sVars, collapse=' + '), '|ccodeS)' ))

# mod=lm(modForm, data=regData)
# library(lmtest)
# coeftest(mod)[1:15,]

mod=lmer(modForm, data=regData)
summary(mod)
###############################################################################

###############################################################################
# Plotting
coefCross = coefCross[which(coefCross$ccodeS==2),]

###############################################################################
