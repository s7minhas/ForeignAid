if(Sys.info()["user"]=="janus829"){
	source("~/Research/ForeignAid/RCode/setup.R") }

################################################################
# Load reg data
setwd(pathData)
load('regData.rda')
regData = regData[regData$year>1974 & regData$year<=2010,]
regData$LstratMu = log(regData$LstratMu + abs(min(regData$LstratMu,na.rm=T)) + 1)
################################################################

################################################################
# RE model
## mod formula
vars=c(
	'LstratMu', # state interest measure
	'Lpolity2', # Institutions
	'LlnGdpCap', # Macroecon controls
	'LlifeExpect', 'Lno_disasters', # Humanitarian
	'Lcivwar' )

## Run model on full sample
modForm=formula(paste0(
	'logAid ~ ', paste(vars, collapse=' + '), 
	' + (', paste(vars, collapse=' + '), '|ccodeS)' ))

yrs=1975:2009
coefCross=NULL
for(ii in 1:length(yrs)){
	# Subset data by year
	slice=regData[which(char(regData$year) %in% char(yrs[ii])),]

	# Run model and pull out results
	modResults=lmer(modForm, data=slice)
	ranCoefs=coef(modResults)$ccodeS
	ranCoefs=data.frame(year=yrs[ii], ccodeS=rownames(ranCoefs), ranCoefs)
	ranCoefs=melt(ranCoefs, id=c('year', 'ccodeS'))

	# Organize results
	coefCross=rbind(coefCross, ranCoefs)
}
###############################################################################

###############################################################################
# Plotting
coefCross = coefCross[which(coefCross$ccodeS==2),]

VARS=vars
VARSname=vars

tmp = ggcoefplot(coefData=coefCross, 
	vars=VARS, varNames=VARSname,
  Noylabel=FALSE, coordFlip=FALSE, revVar=FALSE,
  facet=TRUE, facetColor=FALSE, colorGrey=FALSE,
  facetName='cross', 
  facetDim=c(3,3), 
  facetBreaks=seq(yrs[1],2011,3),
  facetLabs=seq(yrs[1],2011,3),
  allBlack=FALSE
  )
tmp