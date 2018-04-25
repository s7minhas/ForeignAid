if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/ForeignAid/RCode/setup.R') }
if(Sys.info()["user"]=="cindycheng"){
    source('~/Documents/Papers/ForeignAid/RCode/setup.R') }

loadPkg('abind')


############################
# Load amen data
load( paste0(pathTnsr,'amenData_all_rescaled.rda') )
############################

############################
# select vars
inclVars = c('agree2un', 'defEntSum', 'igoWt',  'hostLev', 'armsGdp')

yList = lapply(amData, function(y){
	return( y[,,inclVars] ) })

yLagList = lapply(2:length(amData), function(t){
	yLag = amData[[t-1]]
	y = amData[[t]] * NA
	cntries = intersect(rownames(y), rownames(yLag))
	y[cntries,cntries,] = yLag[cntries,cntries,]

})
############################

############################

############################