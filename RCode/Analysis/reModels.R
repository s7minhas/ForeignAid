if(Sys.info()["user"]=="janus829"){
	source("~/Research/ForeignAid/RCode/setup.R") }

################################################################
# Load reg data
setwd(pathData)
load('regData.rda')
regData = regData[regData$year>1974 & regData$year<=2010,]
# regData$LstratMu = log(regData$LstratMu + abs(min(regData$LstratMu,na.rm=T)) + 1)
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
	'Lcivwar' )

sVars=c('SLpolity2', 'SLlnGdpCap')

## Run model on full sample
modForm=formula(paste0(
	'logAid ~ ', paste(vars, collapse=' + '), 
	' + (1|id)' ))
	# ' + (', paste(sVars, collapse=' + '), '|ccodeS)' ))

yrs=1974:2010
ranCross=NULL
fixCross=NULL
# for(ii in 1:length(yrs)){
	# Subset data by year
	# ii=1
	# slice=regData[which(char(regData$year) %in% char(yrs[ii])),]

	# Run model and pull out results
	modResults=lmer(modForm, data=regData)
	summary(modResults)
	# ugh=cbind(yrs[ii], fixef(modResults)[2])
	# fixCross=rbind(fixCross, ugh)

	# # Fixed effects
	# fixCoefs=fixef(modResults)
	# fixCoefs=data.frame(year=yrs[ii], fixCoefs)
	# fixCross=rbind(fixCross, fixCoefs)

	# # Random effects
	# ranCoefs=coef(modResults)$ccodeS
	# ranCoefs=data.frame(year=yrs[ii], ccodeS=rownames(ranCoefs), ranCoefs)
	# ranCoefs=melt(ranCoefs, id=c('year', 'ccodeS'))
	# ranCross=rbind(ranCoefs, ranCoefs)
}
###############################################################################

###############################################################################
# Plotting
coefCross = coefCross[which(coefCross$ccodeS==2),]

###############################################################################
