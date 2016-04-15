if(Sys.info()['user']=='s7m' | Sys.info()['user']=='janus829'){ source('~/Research/ForeignAid/RCode/setup.R') }
if(Sys.info()['user']=='cindycheng'){ source('~/Dropbox/Documents/Papers/ForeignAid1/RCode/setup.R') }

################################################################
# Load DV
setwd(pathData)
load('aidDataQwids.rda'); rm(list=c('aidMats'))

aidData$commitUSD13[which(aidData$commitUSD13<0)] = 0 

## Log aid flows
# aidData$logAid=log(aidData$commitUSD13 +1)
 
################################################################

################################################################
# PCA variable
setwd(pathResults)
load('PCA/PCA_FullData_allyIGOUN.rda')
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
	names(covData)
covData=covData[,c('cyear', 'ccode','cname', 'year', vars)]
 
 
################################################################

################################################################
# Add ids to various frames
# timeframe: 1971-2005

# Add colony variable
colony = read.csv(paste0(pathData,'/Components/ICOW Colonial History 1.0/coldata100.csv'), stringsAsFactors = F)
colony = colony[,c(2:3)]
colony$ColRulerName=countrycode(colony$ColRuler, 'cown', 'country.name')
colony = colony[!is.na(colony$ColRulerName),]
colony$cname = cname(colony$Name)
colony$cname[colony$cname=='YUGOSLAVIA']='SERBIA'
colony$cname[which(colony$cname == 'CABO VERDE')] = 'CAPE VERDE'
colony$cname[which(colony$cname == 'CONGO')] = "CONGO, REPUBLIC OF"
colony$cname[which(colony$cname == 'CONGO, THE DEMOCRATIC REPUBLIC OF THE')] = "CONGO, THE DEMOCRATIC REPUBLIC OF"
colony$cname[which(colony$cname == 'LIBYA')] = "LIBYAN ARAB JAMAHIRIYA"
colony$cname[which(colony$cname == 'YEMEN ARAB REPUBLIC')] = 'YEMEN'
colony$cname[which(colony$cname == "YEMEN PEOPLE'S REPUBLIC")] = 'S. YEMEN'
colony$cname[which(colony$cname == "VIET NAM")] = 'VIETNAM'
colony$cname[which(colony$Name == "Republic of Vietnam (South)")] = 'S. VIETNAM'
 
colony$ccodeCol=panel$ccode[match(colony$cname,panel$cname)]
colony$ccodeRuler=panel$ccode[match(cname(colony$ColRulerName),panel$cname)]
colony = colony[-which(is.na(colony$ccodeRuler)),] # gets rid of Austria-Hungary
colony$id = num(paste0( colony$ccodeRuler, 9999, colony$ccodeCol ))

# Create id vectors
aidData$id=num(paste0(aidData$ccodeS, 9999, aidData$ccodeR))
aidData$idYr=num(paste0(aidData$ccodeS, 9999, aidData$ccodeR, aidData$year))
aidData$cyearR=num(paste0(aidData$ccodeR, aidData$year))
aidData$cyearS=num(paste0(aidData$ccodeS, aidData$year))

stratData$id=paste0(stratData$ccode1, 9999, stratData$ccode2)
stratData$idYr=paste0(stratData$ccode1, 9999, stratData$ccode2, stratData$year)
names(stratData)[4:6]=paste0('strat',c('Mu','Up','Lo'))

milData$id=paste0(milData$ccode1, 9999, milData$ccode2)
milData$idYr=paste0(milData$ccode1, 9999, milData$ccode2, milData$year)
names(milData)[4:6]=paste0('mil',c('Mu','Up','Lo'))


################################################################

# ################################################################
# # Spatially weighted strategic variable
# stratMat=DyadBuild( variable='stratMu', dyadData=stratData,
#     cntry1='ccode1', cntry2 = 'ccode2',  time='year',
#     pd=1970:2005, panel=panel, directed=TRUE )

# # the idea
# # say that we have countries i, j, and k. 
# # i and j are close to each other in strategic space, and j has in the past given aid to k
# # does i as a result more likely to give aid to k as well.

# milData=DyadBuild( variable='milMu', dyadData=milData,
#     cntry1='ccode1', cntry2 = 'ccode2',  time='year',
#     pd=1970:2005, panel=panel, directed=TRUE )
# ################################################################

################################################################
# Create lagged variables, subset by time (>1974 & <2005), and merge
stratData=lagData(stratData, 'idYr', 'id', names(stratData)[4:6])
milData=lagData(milData, 'idYr', 'id', names(milData)[4:6])
covData=lagData(covData, 'cyear', 'ccode', vars)
 
# Subset datasets by time
aidData = aidData[aidData$year>1974 & aidData$year<=2005,]
stratData = stratData[stratData$year>1974 & stratData$year<=2005,]
milData = milData[milData$year>1974 & milData$year<=2005,]
covData = covData[covData$year>1974 & covData$year<=2005,]


names(covData)
# Merge datasets
regData=aidData 
dim(regData)
 
# Add strategic variable to regData
regData=merge(regData, stratData[,c(8,9)], by='idYr', all.x=TRUE, all.y=FALSE)
unique(regData[is.na(regData$idYr), 1:6]); dim(regData)
# Add military variable to regData
regData=merge(regData, milData[,c(8,9)], by='idYr', all.x=TRUE, all.y=FALSE)
unique(regData[is.na(regData$idYr), 1:6]); dim(regData)
# Add receiver level covariates
regData=merge(regData, covData[,c(1, 10:14)], by.x='cyearR', by.y='cyear', all.x=TRUE, all.y=FALSE)
unique(regData[is.na(regData$idYr), 1:6]); dim(regData)
# Add sender level covariates
names(covData)=paste0('S',names(covData))
regData=merge(regData, covData[,c(1, 10:14)], by.x='cyearS', by.y='Scyear', all.x=TRUE, all.y=FALSE)
unique(regData[is.na(regData$idYr), 1:6]); dim(regData)
# Add colony variable
regData$colony=0
regData$colony[which(regData$id %in% colony$id)]=1


summary(regData)
################################################################

################################################################
# Impute missingness
idVars=c('cyearS', 'cyearR', 'idYr', 'Receiver', 'Sender',
	'cnameS', 'ccodeS', 'cnameR', 'ccodeR')
regVars=names(regData)[-which(names(regData) %in% c(idVars, 'id', 'year'))]
lagVars=regVars[-which(regVars %in% c('colony'))]

lagVars=regVars[grep('SL|L', regVars)]
 
# vars and data  to use in imputation
set.seed(6886)
ameliaRegData=amelia(x=regData, m=5, cs='id', ts='year', 
	lags=lagVars, idvars=idVars, polytime=1)

# copula 
names(regData)
 
impData=regData[,-which(names(regData)%in% c('idYr', 'cnameS', 'cnameR', 'Receiver', 'Sender', 'id'))]
names(impData)

library(sbgcop) 

# copula imputation
imp = sbgcop.mcmc(impData, nsamp=1000, verb=TRUE, seed=1000)

IData=imp$Y.pmean

summary(IData)
setwd(pathData)
pathData
save(IData, file = "IData.rda")


################################################################

################################################################
# Save
setwd(pathData)
save(regData, ameliaRegData, file='regData.rda')
################################################################