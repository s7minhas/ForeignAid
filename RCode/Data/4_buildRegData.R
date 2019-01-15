if(Sys.info()['user']=='s7m' | Sys.info()['user']=='janus829'){ source('~/Research/ForeignAid/RCode/setup.R') }
if(Sys.info()['user']=='cindycheng'){ source('~/Documents/Papers/ForeignAid/RCode/setup.R') }

################################################################
# Load DV
setwd(pathData)
# load('aidDataQwids.rda'); rm(list=c('aidMats'))
# aidData$commitUSD13[which(aidData$commitUSD13<0)] = 0

load('aidDataDisagg.rda'); rm(list=c('aidMats', 'aidMatsEmergency', 'aidMatsHumanitarian', 'aidMatsReconstruction',
	'aidMatsDisaster', 'aidMatsHumanitarianTotal', 'aidMatsNotHumanitarianTotal'))
 
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
# dyadic trade
trade_raw = read.csv(paste0(pathData, '/COW_Trade_4.0/Dyadic_COW_4.0.csv'))

# reorganize data such that data is reported in terms of exports instead of imports (as it currently is)
trade1 = trade_raw[, grep('ccode2|1', names(trade_raw))]
names(trade1)[which(names(trade1) %in% c('ccode1', 'ccode2'))] = c('ccode2', 'ccode1')
trade1 = trade1[, c('ccode1', 'ccode2', names(trade1)[3:10])]
names(trade1)[3:10] = gsub('1', '', names(trade1)[3:10])

 
trade2 = trade_raw[, grep('ccode1|2', names(trade_raw))]
names(trade2)[3:10] = gsub('2', '', names(trade2)[3:10])

trade = rbind(trade1, trade2)

trade$idYr = paste0(trade$ccode1, '9999', trade$cccode2, trade$year)
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
	'no_killed', 'no_injured', 'no_affected',
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

# Merge datasets
regData=aidData 

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

# Add alliance binary
setwd(pathData)
load('stratInterestMatrics.rda')
allyData = melt(allyWtMats) ; names(allyData) = c('ccode1', 'ccode2', 'LallyWt', 'year')
allyData$year = num(allyData$year) + 1 # equiv to lag
allyData$id = paste0(allyData$ccode1, 9999, allyData$ccode2)
allyData$idYr = num( paste0(allyData$ccode1, 9999, allyData$ccode2, allyData$year) )
allyData = allyData[allyData$year>=1975 & allyData$year<=2005,]
regData$LallyWt = allyData$LallyWt[match(regData$idYr, allyData$idYr)]
regData$LallyWt[is.na(regData$LallyWt)] = 0

# Add igo count
igoData = melt(igoMats) ; names(igoData) = c('ccode1', 'ccode2', 'Ligo', 'year')
igoData$year = num(igoData$year) + 1 # equiv to lag
igoData$id = paste0(igoData$ccode1, 9999, igoData$ccode2)
igoData$idYr = num( paste0(igoData$ccode1, 9999, igoData$ccode2, igoData$year) )
igoData = igoData[igoData$year>=1975 & igoData$year<=2005,]
regData$Ligo = igoData$Ligo[match(regData$idYr, igoData$idYr)]
regData$Ligo[is.na(regData$Ligo)] = 0

# Add un ideal point score
unData = melt(unMats) ; names(unData) = c('ccode1', 'ccode2', 'LunIdPt', 'year')
unData$year = num(unData$year) + 1
unData$id = paste0(unData$ccode1, 9999, unData$ccode2)
unData$idYr = num( paste0(unData$ccode1, 9999, unData$ccode2, unData$year) )
unData = unData[unData$year>=1975 & unData$year<=2005,]
regData$LunIdPt = unData$LunIdPt[match(regData$idYr, unData$idYr)]
regData$LunIdPt[is.na(regData$LunIdPt)] = 0

# Save pre imputation
save(regData, file=paste0(pathData, '/noImputationDataAidDisagg.rda'))
load(file=paste0(pathData, '/noImputationDataAidDisagg.rda'))

head(regData)

unique(regData$ccodeS)%>% length() * unique(regData$ccodeR)%>% length() * length(1975:2005)
head(regData)
summary(regData$commitment_amount_usd_constant_sum)
summary(regData)

################################################################

################################################################
# Impute missingness
idVars=c('cyearS', 'cyearR', 'idYr', 'Receiver', 'Sender',
	'cnameS', 'ccodeS', 'cnameR', 'ccodeR')
regVars=names(regData)[-which(names(regData) %in% c(idVars, 'id', 'year'))]
lagVars=regVars[-which(regVars %in% c('colony'))]
lagVars=regVars[grep('SL|L', regVars)]


# copula 
impData=regData[,-which(names(regData)%in% c('idYr', 'cnameS', 'cnameR', 'Receiver', 'Sender', 'id', 'humanitarianTotal', 'notHumanitarianTotal'))]

 
# Divide up data into monadic and dyadic
ids = names(impData)[which(names(impData) %in% c('cyearS', 'cyearR', 'year', 'ccodeS', 'ccodeR'))]
nodeVars = c( 'Lpolity2', 'LlnGdpCap', 'LlifeExpect', 'Lno_disasters', 'Lcivwar', 
	'SLpolity2', 'SLlnGdpCap', 'SLlifeExpect', 'SLno_disasters', 'SLcivwar' )
# dyadVars = c( 'commitUSD13', 'LstratMu', 'LmilMu', 'LallyWt', 'Ligo', 'LunIdPt', 'colony'  )
dyadVars = c( 'commitment_amount_usd_constant_sum', 'emergencyResponse', 'humanitarianAid', 'reconstructionRelief', 'disasterPreventionRelief', 'civSocietyTotal', 'developTotal',  'LstratMu', 'LmilMu', 'LallyWt', 'Ligo', 'LunIdPt', 'colony'  )
nodeData = unique(impData[,c(ids,nodeVars)])
senVars = c(ids[c(1,3:4)], nodeVars[6:10])
recVars = c(ids[c(2,3,5)], nodeVars[1:5])
install.packages("abind")
library(abind)
tmp = abind(
	nodeData[,senVars],
	nodeData[,recVars],
	along=1
	)
nodeData = unique(tmp)
dyadData = impData[,c(ids,dyadVars)]
dyadVars

library(sbgcop) 
source(paste0(pathCode, '/sbgcop_l2.R'))
imp = sbgcop.mcmc_l2(Y=nodeData[,-1], Y_l2=dyadData[,-c(1:2)], nsamp=5000, seed=6886)

# Eval convergence of nodal imputation model
sbgCor = melt(imp$'C.psamp')
sbgCor = sbgCor[sbgCor$X1 != sbgCor$X2,]
sbgCor$v12 = paste0(sbgCor$X1, sbgCor$X2)
tmp=ggplot(sbgCor, aes(x=X3, y=value, color=X2)) + geom_line() + facet_wrap(~X1, scales='free_y') + xlab('') + ylab('')
tmp=tmp + theme(axis.ticks=element_blank(), panel.border=element_blank(), legend.title=element_blank(), legend.position='bottom')
ggsave(tmp, file=paste0(pathGraphics, '/nodalImputationConvergenceSBGCOPDisagg.pdf'))

sbgCor = melt(imp$'C.psamp_l2')
sbgCor = sbgCor[sbgCor$X1 != sbgCor$X2,]
sbgCor$v12 = paste0(sbgCor$X1, sbgCor$X2)
tmp=ggplot(sbgCor, aes(x=X3, y=value, color=X2)) + geom_line() + facet_wrap(~X1, scales='free_y') + xlab('') + ylab('')
tmp=tmp + theme(axis.ticks=element_blank(), panel.border=element_blank(), legend.title=element_blank(), legend.position='bottom')
ggsave(tmp, file=paste0(pathGraphics, '/fullImputationConvergenceSBGCOPDisagg.pdf'))

# Sample 5 from posterior
impPost = imp$'Y.impute_l2'[,,sample(800:1000, 5)]

# Cleanup
iData = lapply(1:dim(impPost)[3], function(ii){
	# Pull out slice and convert to df	
	x = impPost[,,ii]
	x = data.frame( x )

	# Adjust covariates
	x$LmilMu = x$LmilMu + abs(x$LmilMu)

	# Grouping factors for hierarchical framework
	x$year = factor(x$year, levels=sort(unique(x$year)))
	x$ccodeS = factor(x$ccodeS)
	x$ccodeR = factor(x$ccodeR)
	return(x)
	})

setwd(pathData)
save(iData, file = "iDataDisagg.rda")
save(imp, file='sbgOutput_nodalDyadicImputationDisagg.rda')
################################################################