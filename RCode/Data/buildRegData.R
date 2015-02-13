if(Sys.info()["user"]=="janus829"){
	source("~/Research/ForeignAid/RCode/setup.R") }

################################################################
# Load DV
setwd(pathData)
load('aidData.rda'); rm(list=c('aidData', 'aidMats', 'IaidMats'))

## Log aid flows
summary(IaidData$commitUSD09)
IaidData$logAid=log(IaidData$commitUSD09 + 1)
################################################################

################################################################
# PCA variable
setwd(pathResults)
load('PCA/PCA_FullData_allyIGOUN.rda')

## Pull out PCA results
stratData=PCA_FullData$PCA_AllYrs; rm(list='PCA_FullData')
################################################################

################################################################
## Get monadic covariates from BuildCovarData.R
setwd(pathData)
load('covData.rda')

### Insert zeros for NA in civwar variable
covData$civwar[is.na(covData$civwar)]=0

### Insert zeros for NA in emdat variabls
covData$no_disasters[is.na(covData$no_disasters)]=0

### Polity rescale
covData$polity2 = covData$polity2 + abs(min(covData$polity2,na.rm=TRUE)) + 1

### Log transformations
covData$lnGdp = log(covData$gdp)
covData$lnGdpCap = log(covData$gdpCAP)
covData$lnGdpGr = log(covData$gdpGR + abs(min(covData$gdpGR, na.rm=TRUE)) +1)
covData$lnPop = log(covData$population)
covData$lnFdi = log(covData$fdi + abs(min(covData$fdi, na.rm=TRUE)) +1)
covData$lnMort5 = log(covData$mort5)

### Subset monadic covariates to relevant years set
vars=c(
	'polity2', # Institutions
	'lnGdpCap', # Macroecon controls
	'lifeExpect', 'no_disasters', # Humanitarian
	'civwar' )
covData=covData[,c('ccode','cname', 'year', vars)]

### Impute missing covariate data
covData$cyear=num(paste0(covData$ccode,covData$year))

# Add five lags to capture possible trends
covData=lagData(covData, 'cyear', 'ccode', vars)
covData=lagData(covData, 'cyear', 'ccode', paste0('L', vars))
covData=lagData(covData, 'cyear', 'ccode', paste0('LL', vars))
covData=lagData(covData, 'cyear', 'ccode', paste0('LLL', vars))
covData=lagData(covData, 'cyear', 'ccode', paste0('LLLL', vars))

# vars and data  to use in imputation
impVars=c(
	'ccode','year', 'polity2',
	apply(
		expand.grid(
			c('','L','LL'), 
			vars[2:length(vars)] ) ,
		1, paste, collapse="")
	)
impData=covData[,impVars]

# impute with gaussian copula [will incorporate imputation w/ other covars later]
covData_sbgcop=sbgcop.mcmc(impData, nsamp=1000, verb=TRUE, seed=1342)

# Setting up imputed dataframe
IcovData=cbind(
	covData[,c('ccode','cname','year')], 
	covData_sbgcop$Y.pmean[,vars] )
setwd(pathData)
save(IcovData, file='IcovData.rda')
################################################################

################################################################
# Merge datasets for regression
# timeframe: 1971-2010

# Add colony variable
colony = read.csv(paste0(pathData,'/Components/ICOW Colonial History 1.0/coldata100.csv'))
colony = colony[,c(2:3)]
colony$ColRulerName=countrycode(colony$ColRuler, 'cown', 'country.name')
colony = colony[!is.na(colony$ColRulerName),]
colony$cname = cname(colony$Name)
colony$cname[colony$cname=='YUGOSLAVIA']='SERBIA'
colony = unique(colony[,c(3:4) ])
colony$ccodeCol=panel$ccode[match(colony$cname,panel$cname)]
colony$ccodeRuler=panel$ccode[match(colony$ColRulerName,panel$cname)]
colony$id = num(paste0( colony$ccodeRuler, 9999, colony$ccodeCol ))

# Create id vectors
if(! 'IcovData' %in% ls() ){ setwd(pathData); load('IcovData.rda') }
IaidData$id=num(paste0(IaidData$ccodeS, 9999, IaidData$ccodeR))
IaidData$idYr=num(paste0(IaidData$ccodeS, 9999, IaidData$ccodeR, IaidData$year))
IaidData$cyearR=num(paste0(IaidData$ccodeR, IaidData$year))
IaidData$cyearS=num(paste0(IaidData$ccodeS, IaidData$year))

stratData$id=paste0(stratData$ccode1, 9999, stratData$ccode2)
stratData$idYr=paste0(stratData$ccode1, 9999, stratData$ccode2, stratData$year)

IcovData$cyear=paste0(IcovData$ccode, IcovData$year)

# Lag covariate data
names(stratData)[4:6]=paste0('strat',c('Mu','Up','Lo'))
stratData=lagData(stratData, 'idYr', 'id', names(stratData)[4:6])
IcovData=lagData(IcovData, 'cyear', 'ccode', vars)

# Subset datasets by time
IaidData = IaidData[IaidData$year>1970 & IaidData$year<=2010,]
stratData = stratData[stratData$year>1970 & stratData$year<=2010,]
IcovData = IcovData[IcovData$year>1970 & IcovData$year<=2010,]

# Merge datasets
regData=IaidData
dim(regData)
# Add strategic variable to regData
regData=merge(regData, stratData[,c(8:11)], by='idYr', all.x=TRUE, all.y=FALSE)
unique(regData[is.na(regData$idYr), 1:6]); dim(regData)
# Add receiver level covariates
regData=merge(regData, IcovData[,c(9:14)], by.x='cyearR', by.y='cyear', all.x=TRUE, all.y=FALSE)
unique(regData[is.na(regData$idYr), 1:6]); dim(regData)
# Add sender level covariates
names(IcovData)=paste0('S',names(IcovData))
regData=merge(regData, IcovData[,c(9:14)], by.x='cyearS', by.y='Scyear', all.x=TRUE, all.y=FALSE)
unique(regData[is.na(regData$idYr), 1:6]); dim(regData)
# Add colony variable
regData$colony=0
regData$colony[which(regData$id %in% colony$id)]=1

setwd(pathData)
save(regData, file='regData.rda')
################################################################