### Goal of this file is to build the dyadic dataset
## Author: SM

### Load setup
source('/Users/janus829/Desktop/Research/ButheProjects/ForeignAid/AlesinaDollarReplication/RCode/setup.R')

### Load data
setwd(pathData)
aid <- read.csv('datasetSM.csv')

### Subsetting to relevant vars
aid <- aid[,c(1:2, 11:31)]


### Load data
setwd(pathData)
load('allData.rda')
load('SRdataImputed.rda')
load('~/Desktop/Research/BuildingPanelData/panel.rda')
###############################################################

###############################################################
###############################################################
##### DYADIC DATAFRAMES
###############################################################
###############################################################

###############################################################
# COW trade
trade2 <- trade[,c('importer1', 'importer2', 'year', 'flow1', 'flow2')]
colnames(trade2) <- c('state_name1', 'state_name2', 'year', 'imports', 'exports')
trade2 <- trade2[trade2$year>=1970,]

trade2$state_name1 <- as.character(trade2$state_name1)
trade2$state_name2 <- as.character(trade2$state_name2)

trade2$imports[trade2$imports==-9] <- 0 # setting missing to 0
trade2$exports[trade2$exports==-9] <- 0 # setting missing to 0

trade2$state_name1[trade2$state_name1=='Democratic Republic of t'] <- 'Congo, Democratic Republic of'
trade2$state_name2[trade2$state_name2=='Democratic Republic of t'] <- 'Congo, Democratic Republic of'

trade2$state_name1[trade2$state_name1=='Democratic Republic of the Con'] <- 'Congo, Democratic Republic of'
trade2$state_name2[trade2$state_name2=='Democratic Republic of the Con'] <- 'Congo, Democratic Republic of'

trade2$state_name1[trade2$state_name1=='Federated States of Micr'] <- 'Micronesia'
trade2$state_name2[trade2$state_name2=='Federated States of Micr'] <- 'Micronesia'

trade2 <- trade2[trade2$state_name1!="Yemen People's Republic",]
trade2 <- trade2[trade2$state_name2!="Yemen People's Republic",]

states <- unique(append(trade2$state_name1, trade2$state_name2))
temp <- data.frame(cbind(
	states, cname=countrycode(states, 'country.name', 'country.name')))
temp$cname <- as.character(temp$cname)
temp$cname[temp$cname=='Yugoslavia'] <- 'SERBIA'
temp$cname[temp$cname=='Czechoslovakia'] <- 'CZECH REPUBLIC'
temp$ccode <- panel$ccode[match(temp$cname,panel$cname)]

trade2$cname_1 <- temp$cname[match(trade2$state_name1,temp$states)]
trade2$cname_2 <- temp$cname[match(trade2$state_name2,temp$states)]

trade2$ccode_1 <- temp$ccode[match(trade2$state_name1,temp$states)]
trade2$ccode_2 <- temp$ccode[match(trade2$state_name2,temp$states)]

trade2 <- trade2[!is.na(trade2$ccode_1),]
trade2 <- trade2[!is.na(trade2$ccode_2),]

trade2$cyear_1 <- as.numeric(as.character(paste(trade2$ccode_1, trade2$year, sep='')))
trade2$cyear_2 <- as.numeric(as.character(paste(trade2$ccode_2, trade2$year, sep='')))

# Dupe check
slice <- trade2[trade2$year>=1984 & trade2$year<2010,]
temp_1 <- paste(slice$ccode_1,slice$ccode_2,slice$year,sep='')
temp_2 <- paste(slice$ccode_2,slice$ccode_1,slice$year,sep='')
# names(table(temp_1)[table(temp_1)>2])
# names(table(temp_2)[table(temp_2)>2])
# Removing duplicates	
trade2$drop <- 0
# trade2[trade2$state_name1=='German Federal Republic' & trade2$year==1990,]
trade2[trade2$state_name1=='Germany' & trade2$year==1990, 'drop'] <- 1
trade2[trade2$state_name2=='Germany' & trade2$year==1990, 'drop'] <- 1
trade2[trade2$state_name1=='Yemen Arab Republic' & trade2$year==1990, 'drop'] <- 1
trade2[trade2$state_name2=='Yemen Arab Republic' & trade2$year==1990, 'drop'] <- 1
trade2 <- trade2[trade2$drop!=1,]; trade2 <- trade2[,c(1:(ncol(trade2)-1))]

### Construct trade dependence DV
# Bringing in GDP data [ no missing due to sbgcop imputation ]
trade2$gdp_1 <- srDatav3$gdp[match(trade2$cyear_1,srDatav3$cyear)]
trade2$gdp_2 <- srDatav3$gdp[match(trade2$cyear_2,srDatav3$cyear)]

# Time [trimming to 1984 to 2012]
trade2 <- trade2[trade2$year>=1984 & trade2$year<2010,]

# Countries [trimming to countries for which data was collected]
trade2 <- trade2[which(trade2$cyear_1 %in% srDatav3$cyear),]
trade2 <- trade2[which(trade2$cyear_2 %in% srDatav3$cyear),]

### ASIDE
# Create a separate export & import dataset
temp1 <- trade2[,c('ccode_1','ccode_2','year','exports')]
colnames(temp1) <- c('ccode_1','ccode_2','year','exports')
temp2 <- trade2[,c('ccode_2','ccode_1','year','imports')]
colnames(temp2) <- c('ccode_1','ccode_2','year','exports')
exports <- rbind(temp1, temp2)
exports$exports <- exports$exports*1000000

temp1 <- trade2[,c('ccode_1','ccode_2','year','imports')]
colnames(temp1) <- c('ccode_1','ccode_2','year','imports')
temp2 <- trade2[,c('ccode_2','ccode_1','year','exports')]
colnames(temp2) <- c('ccode_1','ccode_2','year','imports')
imports <- rbind(temp1, temp2)
imports$imports <- imports$imports*1000000

trade3 <- cbind(imports[,1:3], trade=imports[,4]+exports[,4])
trade3$cyear_1 <- paste(trade3$ccode_1, trade3$year, sep='')
trade3$cyear_2 <- paste(trade3$ccode_2, trade3$year, sep='')
trade3$cname_1 <- srDatav3$gdp[match(trade3$cyear_1, srDatav3$cyear)]
trade3$cname_2 <- srDatav3$gdp[match(trade3$cyear_2, srDatav3$cyear)]
trade3$gdp_1 <- srDatav3$gdp[match(trade3$cyear_1, srDatav3$cyear)]
trade3$tradeDep <- trade3$trade/trade3$gdp_1

# Subsetting to relevant vars
tradeTot <- trade3[,c(
	'ccode_1','ccode_2','cname_1','cname_2', 'cyear_1', 'cyear_2', 'year',
	'trade')]

tradeDep <- trade3[,c(
	'ccode_1','ccode_2','cname_1','cname_2', 'cyear_1', 'cyear_2', 'year',
	'tradeDep')]
###############################################################

###############################################################
# COW Alliance
alliance2 <- alliance

alliance2 <- unique(alliance2[,c('state_name1', 'state_name2', 'year')])
alliance2$state_name1 <- as.character(alliance2$state_name1)
alliance2$state_name2 <- as.character(alliance2$state_name2)

states <- unique(append(alliance2$state_name1, alliance2$state_name2))
temp <- data.frame(cbind(
	states, cname=countrycode(states, 'country.name', 'country.name')))
temp$cname <- as.character(temp$cname)
temp$cname[temp$cname=='Yugoslavia'] <- 'SERBIA'
temp$cname[temp$cname=='Czechoslovakia'] <- 'CZECH REPUBLIC'
temp$ccode <- panel$ccode[match(temp$cname,panel$cname)]

alliance2$cname_1 <- temp$cname[match(alliance2$state_name1,temp$states)]
alliance2$cname_2 <- temp$cname[match(alliance2$state_name2,temp$states)]

alliance2$ccode_1 <- temp$ccode[match(alliance2$state_name1,temp$states)]
alliance2$ccode_2 <- temp$ccode[match(alliance2$state_name2,temp$states)]

alliance2 <- alliance2[!is.na(alliance2$ccode_1),]
alliance2 <- alliance2[!is.na(alliance2$ccode_2),]

alliance2$cyear_1 <- as.numeric(as.character(paste(alliance2$ccode_1, alliance2$year, sep='')))
alliance2$cyear_2 <- as.numeric(as.character(paste(alliance2$ccode_2, alliance2$year, sep='')))

alliance2$ally <- 1

# Subsetting to relevant vars
alliance2 <- alliance2[,c(
	'ccode_1','ccode_2','cname_1','cname_2', 'cyear_1', 'cyear_2', 'year',
	'ally')]
###############################################################

###############################################################
# PRIO inter-state war
war2 <- war[war$Type==2,]
war2 <- unique(war2[,c('ID','SideA', 'SideA2nd', 'SideB',  'SideB2nd', 'YEAR')])
war2 <- war2[1:(nrow(war2)-1),]

war2$SideA_All <- ifelse(trim(war2$SideA2nd)!='',
	as.character(paste(war2$SideA, war2$SideA2nd, sep=',')),
	as.character(war2$SideA))

war2$SideB_All <- ifelse(trim(war2$SideB2nd)!='',
	as.character(paste(war2$SideB, war2$SideB2nd, sep=',')),
	as.character(war2$SideB))

war2 <- data.frame(war2, row.names=NULL)

# Arranging to panel format and breaking rows with 
# multiple countries listed into separate rows
war3 <- NULL
for(ii in 1:nrow(war2)){
	wSlice <- war2[ii, c('SideA_All','SideB_All','YEAR')]
	Acnts <- trim(unlist(strsplit(wSlice$SideA_All,',')))
	Bcnts <- trim(unlist(strsplit(wSlice$SideB_All,',')))
	lAcnts <- length(Acnts)
	lBcnts <- length(Bcnts)
	if(lAcnts>1 | lBcnts>1){
		wSlice2 <- NULL
		for(jj in 1:lBcnts){
			wSlice1_5 <- cbind(t(t(Acnts)), t(t(Bcnts))[jj], wSlice[,'YEAR'])
			colnames(wSlice1_5) <- c('SideA_All','SideB_All','YEAR')
			wSlice2 <- rbind(wSlice2, wSlice1_5)
		}
		war3 <- rbind(war3, wSlice2)
	} else{
		wSlice2 <- wSlice
		war3 <- rbind(war3, wSlice2)
	}
}

# Adding in cname and ccode
war3$YEAR <- as.numeric(as.character(war3$YEAR))
colnames(war3) <- c('state_name1','state_name2','Year')

war3 <- war3[war3$state_name1!='Hyderabad',]
war3 <- war3[war3$state_name2!='Hyderabad',]

war3$state_name1[war3$state_name1=='United Arab Emirate'] <- 'United Arab Emirates'
war3$state_name2[war3$state_name2=='United Arab Emirate'] <- 'United Arab Emirates'

states <- unique(append(war3$state_name1, war3$state_name2))
temp <- data.frame(cbind(
	states, cname=countrycode(states, 'country.name', 'country.name')))
temp$cname <- as.character(temp$cname)
temp$cname[temp$cname=='Czechoslovakia'] <- 'CZECH REPUBLIC'
temp$ccode <- panel$ccode[match(temp$cname,panel$cname)]

war3$cname_1 <- temp$cname[match(war3$state_name1,temp$states)]
war3$cname_2 <- temp$cname[match(war3$state_name2,temp$states)]

war3$ccode_1 <- temp$ccode[match(war3$state_name1,temp$states)]
war3$ccode_2 <- temp$ccode[match(war3$state_name2,temp$states)]

war3 <- war3[!is.na(war3$ccode_1),]
war3 <- war3[!is.na(war3$ccode_2),]

war3$cyear_1 <- as.numeric(as.character(paste(war3$ccode_1, war3$Year, sep='')))
war3$cyear_2 <- as.numeric(as.character(paste(war3$ccode_2, war3$Year, sep='')))

war3$war <- 1

# Subsetting to relevant vars
colnames(war3)[which(names(war3)=='Year')] <- 'year'
war3 <- war3[,c(
	'ccode_1','ccode_2','cname_1','cname_2', 'cyear_1', 'cyear_2', 'year',
	'war')]
###############################################################

###############################################################
# BITs for dyadic
bitsSignedDyad <- bits[,c('ReporterClean','PartnerClean','Year_Signature', 'signedbitsSM')]
bitsSignedDyad$ccode_1 <- panel$ccode[match(bitsSignedDyad$ReporterClean, panel$cname)]
bitsSignedDyad <- bitsSignedDyad[!is.na(bitsSignedDyad$ccode_1),]
bitsSignedDyad$ccode_2 <- panel$ccode[match(bitsSignedDyad$PartnerClean, panel$cname)]
bitsSignedDyad <- bitsSignedDyad[!is.na(bitsSignedDyad$ccode_2),]
bitsSignedDyad$cyear_1 <- paste(bitsSignedDyad$ccode_1, bitsSignedDyad$Year_Signature, sep='')
bitsSignedDyad$cyear_2 <- paste(bitsSignedDyad$ccode_2, bitsSignedDyad$Year_Signature, sep='')

# Subsetting to relevant vars
colnames(bitsSignedDyad)[which(names(bitsSignedDyad)=='Year_Signature')] <- 'year'
colnames(bitsSignedDyad)[which(names(bitsSignedDyad)=='ReporterClean')] <- 'cname_1'
colnames(bitsSignedDyad)[which(names(bitsSignedDyad)=='PartnerClean')] <- 'cname_2'
bitsSignedDyad <- bitsSignedDyad[,c(
	'ccode_1','ccode_2','cname_1','cname_2', 'cyear_1', 'cyear_2', 'year',
	'signedbitsSM')]

bitsRatifiedDyad <- bits[,c('ReporterClean','PartnerClean','Year_force', 'ratifiedbitsSM')]
bitsRatifiedDyad <- na.omit(bitsRatifiedDyad)
bitsRatifiedDyad$ccode_1 <- panel$ccode[match(bitsRatifiedDyad$ReporterClean, panel$cname)]
bitsRatifiedDyad <- bitsRatifiedDyad[!is.na(bitsRatifiedDyad$ccode_1),]
bitsRatifiedDyad$ccode_2 <- panel$ccode[match(bitsRatifiedDyad$PartnerClean, panel$cname)]
bitsRatifiedDyad <- bitsRatifiedDyad[!is.na(bitsRatifiedDyad$ccode_2),]
bitsRatifiedDyad$cyear_1 <- paste(bitsRatifiedDyad$ccode_1, bitsRatifiedDyad$Year_force, sep='')
bitsRatifiedDyad$cyear_2 <- paste(bitsRatifiedDyad$ccode_2, bitsRatifiedDyad$Year_force, sep='')

# Subsetting to relevant vars
colnames(bitsRatifiedDyad)[which(names(bitsRatifiedDyad)=='Year_force')] <- 'year'
colnames(bitsRatifiedDyad)[which(names(bitsRatifiedDyad)=='ReporterClean')] <- 'cname_1'
colnames(bitsRatifiedDyad)[which(names(bitsRatifiedDyad)=='PartnerClean')] <- 'cname_2'
bitsRatifiedDyad <- bitsRatifiedDyad[,c(
	'ccode_1','ccode_2','cname_1','cname_2', 'cyear_1', 'cyear_2', 'year',
	'ratifiedbitsSM')]
###############################################################

###############################################################
# Cleaned datasets
setwd(pathData)
save(exports, imports, tradeTot, tradeDep, alliance2, war3, 
	bitsSignedDyad, bitsRatifiedDyad,
	file='DYADindivCleaned.rda')
###############################################################

###############################################################
# matrix builder for undirected dyad data from dyadic data
years <- 1984:2009
cntryList <- lapply(years, function(x) FUN=srDatav3[srDatav3$year==x,'ccode'])
names(cntryList) <- years

exportMats <- DyadBuild(variable='exports', dyadData=exports, 
	time=years, countryList=cntryList, directed=TRUE)

tradeTotMats <- DyadBuild(variable='trade', dyadData=tradeTot, 
	time=years, countryList=cntryList, directed=TRUE)

tradeDepMats <- DyadBuild(variable='tradeDep', dyadData=tradeDep, 
	time=years, countryList=cntryList, directed=TRUE)

allyMats <- DyadBuild(variable='ally', dyadData=alliance2, 
	time=years, countryList=cntryList, directed=FALSE)

warMats <- DyadBuild(variable='war', dyadData=war3, 
	time=years, countryList=cntryList, directed=FALSE)

years <- 1960:2009
cntryList <- lapply(years, function(x) FUN=panel[panel$year==x,'ccode'])
names(cntryList) <- years

bitSignMats <- DyadBuild(variable='signedbitsSM', dyadData=bitsSignedDyad, 
	time=years, countryList=cntryList, directed=FALSE)

bitRatifMats <- DyadBuild(variable='ratifiedbitsSM', dyadData=bitsRatifiedDyad, 
	time=years, countryList=cntryList, directed=FALSE)
###############################################################

###############################################################
# matrix builder for undirected dyad data from monadic data
years <- 1984:2009
cntryList <- lapply(years, function(x) FUN=srDatav3[srDatav3$year==x,'ccode'])
names(cntryList) <- years

invProfMats <- undirDyadBuild_fMonad(var='Investment.Profile', 
	monadData=srDatav3, time=years, countryList=cntryList)
propRightMats <- undirDyadBuild_fMonad(var='Property.Rights', 
	monadData=srDatav3, time=years, countryList=cntryList)
LNr_gdpMats <- undirDyadBuild_fMonad(var='LNr_gdp', 
	monadData=srDatav3, time=years, countryList=cntryList)
LNr_gdpCAPMats <- undirDyadBuild_fMonad(var='LNr_gdpCAP', 
	monadData=srDatav3, time=years, countryList=cntryList)
###############################################################

###############################################################
# Save results
save(exportMats, tradeTotMats, tradeDepMats, 
	bitSignMats, bitRatifMats, allyMats, warMats,
	invProfMats, propRightMats, LNr_gdpMats, LNr_gdpCAPMats,
	file='DYADmat.rda')
###############################################################