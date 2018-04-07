if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/ForeignAid/RCode/setup.R') }
if(Sys.info()["user"]=="cindycheng"){
    source('~/Documents/Papers/ForeignAid/RCode/setup.R') }

############################
# Load un and alliance data
load( paste0(pathTnsr, 'ally.rda') )
load( paste0(pathTnsr, 'un.rda') )
load( paste0(pathTnsr, 'unIdealPt.rda') )
load( paste0(pathTnsr, 'igo.rda') )
load( paste0(pathTnsr, 'mids.rda') )
load( paste0(pathTnsr, 'sipri.rda') )

############################

############################
# Form list of n x n x p arrays for every t
# n = # cntries, p = # vars, t = # pds

# Years to loop through
yrs = 1975:2005

# Process data into list of arrays
amData = lapply(yrs, function(yr){
	
	# Pull yearly slice from un data
	aun3Sl = aun3Lfull[[char(yr)]]
	aun2Sl = aun2Lfull[[char(yr)]]
	idealPtl = idealPtDistfull[[char(yr)]]

	# Pull yearly slice from alliance dataset
	anyAllySl = anyAllyL[[char(yr)]]		
	totAllySl = totAllyL[[char(yr)]]
	defEntSl = defEntAllyL[[char(yr)]]
	defEntSumAllySl = defEntSumAllyL[[char(yr)]]
	defAllySl = defAllyL[[char(yr)]]
	wtAllySl = wtAllyL[[char(yr)]]

	# Pull yearly slice from igo
	igoSl = igoL[[char(yr)]]
	igoSWl = igoWL[[char(yr)]]

	# Pull yearly slice from mids data
	hostLevSl = hostLevL[[char(yr)]]
	midCountSl = midCountL[[char(yr)]]
	fatalMidsSl = fatalMidsL[[char(yr)]]

	# pull arms data
	 armsTrsfrsSl = armsTrsfrsL[[char(yr)]]
	 armsPopSl =  armsPopL[[char(yr)]]
	 armsGdpSl = armsGdpL[[char(yr)]]


	# Merge covariate data into frame slice
	stdz = function(x, na=TRUE){ (x - mean(x, na.rm=na))/sd(x, na.rm=na) }
	addVar = function(
		fromVar, fromID, toID=fSl$ij, 
		naZero=TRUE, rescale=FALSE, stdzVar=TRUE){
		tmp = fromVar[match(toID, fromID)]
		tmp = num(tmp)
		if(naZero){ tmp[is.na(tmp)] = 0  }
		if(rescale){ tmp = rescale(tmp, 10, 1) }
		if(stdzVar){ tmp = stdz(tmp) }
		return(tmp)
	}

	fSl = unFrame[[char(yr)]]	
	fSl$agree3un = addVar(aun3Sl$agree3un, aun3Sl$ij, naZero=FALSE)
	fSl$agree2un = addVar(aun2Sl$agree2un, aun2Sl$ij, naZero=FALSE)
	fSl$idealPtun = addVar(idealPtl$idealpointdistance, idealPtl$ij)
	fSl$anyAlly = addVar(anyAllySl$any, anyAllySl$ij)
	fSl$totAllyCnt = addVar(totAllySl$totCnt, totAllySl$ij)
	fSl$defEnt = addVar(defEntSl$defEnt, defEntSl$ij)
	fSl$defEntSum = addVar(defEntSumAllySl$defEntSum, defEntSumAllySl$ij)
	fSl$defense = addVar(defAllySl$defense, defAllySl$ij)
	fSl$allyWt = addVar(wtAllySl$wtAlly, wtAllySl $ij)
	fSl$igo = addVar(igoSl$igo, igoSl$ij)
	fSl$igoWt = addVar(igoSWl$igoWeighted, igoSWl$ij)
	fSl$hostLev = addVar(hostLevSl$HostLevRev, hostLevSl$ij)
	fSl$midCount = addVar(midCountSl$midCountRev, midCountSl$ij)
	fSl$fatalMids = addVar(fatalMidsSl$fatalMidsRev, fatalMidsSl$ij)
	fSl$armsTrsfrs = addVar(armsTrsfrsSl$armsTransfers, armsTrsfrsSl$ij)
	fSl$armsPop = addVar(armsPopSl$armsPop, armsPopSl$ij)
	fSl$armsGdp = addVar(armsGdpSl$armsGdp, armsGdpSl$ij)

	# Create empty array
	cntries = c( fSl$i, fSl$j ) %>% unique() %>% char() %>% sort() 
	vars = names(fSl)[6:ncol(fSl)]
	eArr = array(0, 
		dim=c( length(cntries), length(cntries), length(vars) ), 
		dimnames=list( cntries, cntries, vars ) )
	
	# Add values from relev data
	diagVal = function(x,val=NA){ diag(x)=val ; return(x) }
	for(var in vars){
		eArr[,,var] = acast(i ~ j, data=fSl, value.var=var) %>% .[cntries,cntries] %>% diagVal()
	}
	return(eArr)
	})
names(amData) = yrs

 

 
############################s

############################
# Save
#save(amData, file=paste0(pathTnsr,'amenData_all.rda'))
#save(amData, file=paste0(pathTnsr,'amenData_all_rescaled.rda'))
save(amData, file=paste0(pathTnsr,'amenData_all_stdz.rda'))
############################