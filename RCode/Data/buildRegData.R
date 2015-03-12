if(Sys.info()['user']=='janus829' | Sys.info()['user']=='janus829'){ source('~/Research/ForeignAid/RCode/setup.R') }

################################################################
# Load DV
setwd(pathData)
load('aidData.rda'); rm(list=c('aidMats'))
## Log aid flows
aidData$logAid=log(aidData$commitUSD09 + 1)
################################################################

################################################################
# PCA variable
setwd(pathResults)
load('PCA/PCA_FullData_allyIGOUN.rda')

## Pull out PCA results
stratData=PCA_FullData$PCA_AllYrs; rm(list='PCA_FullData')

load('PCA/PCA_FullData_midWarArmsSum.rda')
milData=PCA_FullData$PCA_AllYrs; rm(list='PCA_FullData')
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
covData$lnGdpCap = log(covData$gdpCAP)

### Subset monadic covariates to relevant years set
vars=c(
	'polity2', # Institutions
	'lnGdpCap', # Macroecon controls
	'lifeExpect', 'no_disasters', # Humanitarian
	'civwar' )
covData=covData[,c('ccode','cname', 'year', vars)]
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
aidData$id=num(paste0(aidData$ccodeS, 9999, aidData$ccodeR))
aidData$idYr=num(paste0(aidData$ccodeS, 9999, aidData$ccodeR, aidData$year))
aidData$cyearR=num(paste0(aidData$ccodeR, aidData$year))
aidData$cyearS=num(paste0(aidData$ccodeS, aidData$year))

stratData$id=paste0(stratData$ccode1, 9999, stratData$ccode2)
stratData$idYr=paste0(stratData$ccode1, 9999, stratData$ccode2, stratData$year)

milData$id=paste0(milData$ccode1, 9999, milData$ccode2)
milData$idYr=paste0(milData$ccode1, 9999, milData$ccode2, milData$year)

covData$cyear=paste0(covData$ccode, covData$year)

# Lag covariate data
names(stratData)[4:6]=paste0('strat',c('Mu','Up','Lo'))
stratData=lagData(stratData, 'idYr', 'id', names(stratData)[4:6])

names(milData)[4:6]=paste0('mil',c('Mu','Up','Lo'))
milData=lagData(milData, 'idYr', 'id', names(milData)[4:6])

covData=lagData(covData, 'cyear', 'ccode', vars)

# Subset datasets by time
aidData = aidData[aidData$year>1970 & aidData$year<=2010,]
stratData = stratData[stratData$year>1970 & stratData$year<=2010,]
milData = milData[milData$year>1970 & milData$year<=2010,]
covData = covData[covData$year>1970 & covData$year<=2010,]

# Merge datasets
regData=aidData[,which(!names(aidData) %in% 'commitUSD09')]
dim(regData)
# Add strategic variable to regData
regData=merge(regData, stratData[,c(8,9)], by='idYr', all.x=TRUE, all.y=FALSE)
unique(regData[is.na(regData$idYr), 1:6]); dim(regData)
# Add military variable to regData
regData=merge(regData, milData[,c(8,9)], by='idYr', all.x=TRUE, all.y=FALSE)
unique(regData[is.na(regData$idYr), 1:6]); dim(regData)
# Add receiver level covariates
regData=merge(regData, covData[,c(9:14)], by.x='cyearR', by.y='cyear', all.x=TRUE, all.y=FALSE)
unique(regData[is.na(regData$idYr), 1:6]); dim(regData)
# Add sender level covariates
names(covData)=paste0('S',names(covData))
regData=merge(regData, covData[,c(9:14)], by.x='cyearS', by.y='Scyear', all.x=TRUE, all.y=FALSE)
unique(regData[is.na(regData$idYr), 1:6]); dim(regData)
# Add colony variable
regData$colony=0
regData$colony[which(regData$id %in% colony$id)]=1
################################################################

################################################################
# Impute missingness
idVars=c('cyearS', 'cyearR', 'idYr', 'Receiver', 'Sender',
	'cnameS', 'ccodeS', 'cnameR', 'ccodeR')
regVars=names(regData)[-which(names(regData) %in% c(idVars, 'id', 'year'))]

# vars and data  to use in imputation
set.seed(6886)
ameliaRegData=amelia(x=regData, m=5, cs='id', ts='year', 
	lags=regVars, idvars=idVars, polytime=1)

summary(ameliaRegData$imp$imp1)
################################################################

################################################################
# Save
setwd(pathData)
save(regData, ameliaRegData, file='regData.rda')
################################################################