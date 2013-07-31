### Goal of this file is to build the SR Dataset
## Author: SM

### Load setup
source('/Users/janus829/Desktop/Research/WardProjects/bitFormation/RCode/setup.R')

### Load data
setwd(pathData)
load('allData.rda')
load('~/Desktop/Research/BuildingPanelData/panel.rda')

###############################################################
# Organizing WB data
### Fx for Melting/Cleaning WB Data for Merge

cleanWbData <- function(data, variable){
	var <- variable
	mdata <- melt(data, id=c('Country.Name', 'Country.Code'))
	names(mdata)[4] <- var
	mdata$year <-  as.numeric(as.character(substring(mdata$variable,2)))
	mdata <- mdata[,c(1,2,5,4)]

	# Remove non-country observations and small islands/territories
	drop <- c('Arab World', 'Caribbean small states', 
		'East Asia & Pacific (all income levels)', 
		'East Asia & Pacific (developing only)', 'Euro area', 
		'Europe & Central Asia (all income levels)', 
		'Europe & Central Asia (developing only)', 
		'European Union', 'Heavily indebted poor countries (HIPC)', 
		'High income', 'High income: nonOECD', 'High income: OECD', 
		'Latin America & Caribbean (all income levels)', 
		'Latin America & Caribbean (developing only)', 
		'Least developed countries: UN classification', 
		'Low & middle income', 'Low income', 'Lower middle income', 
		'Middle East & North Africa (all income levels)', 
		'Middle East & North Africa (developing only)', 'Middle income', 
		'North America', 'Not classified', 'OECD members', 
		'Other small states', 'Pacific island small states', 
		'Small states', 'South Asia', 
		'Sub-Saharan Africa (all income levels)', 
		'Sub-Saharan Africa (developing only)', 'Upper middle income', 
		'World',
		 "American Samoa",            "Aruba",                    
		 "Bermuda",                   "Cayman Islands", "Channel Islands",          
		 "Curacao",                   "Faeroe Islands",           
		 "French Polynesia",          "Greenland",                
		 "Guam",                      "Hong Kong SAR, China",     
		 "Isle of Man",               "Macao SAR, China",         
		 "New Caledonia",             "Northern Mariana Islands", 
		 "Puerto Rico",               "Sint Maarten (Dutch part)",
		 "St. Martin (French part)",  "Turks and Caicos Islands", 
		 "Virgin Islands (U.S.)",     "West Bank and Gaza")
	mdata <- mdata[which(!mdata$Country.Name %in% drop),]

	# Setting standardized countryname for WB data
	mdata$Country.Name <- as.character(mdata$Country.Name)
	mdata$Country.Name[mdata$Country.Name=='Korea, Dem. Rep.'] <- 'North Korea' 
	mdata$Country.Name[mdata$Country.Name=='Korea, Rep.'] <- 'South Korea' 
	mdata$cname <- countrycode(mdata$Country.Name, 'country.name', 'country.name')
	mdata$cnameYear <- paste(mdata$cname, mdata$year, sep='')
	
	# Adding in codes from panel
	mdata$ccode <- panel$ccode[match(mdata$cname,panel$cname)]
	mdata$cyear <- paste(mdata$ccode, mdata$year, sep='')
	mdata }

WBfdiClean <- cleanWbData(WBfdi, 'fdi')
WBfdiGdpClean <- cleanWbData(WBfdiGdp, 'fdiGDP')
WBgdpClean <- cleanWbData(WBgdp, 'gdp')
WBgdpGRClean <- cleanWbData(WBgdpGR, 'gdpGR')
WBgdpCapClean <- cleanWbData(WBgdpCap, 'gdpCAP')
WBpopClean <- cleanWbData(WBpop, 'population')
WBgdpDeflatorClean <- cleanWbData(WBgdpDeflator, 'gdpDeflator')

# Make sure order matches
sum(WBfdiClean$cyear!=WBfdiGdpClean$cyear)
sum(WBfdiClean$cyear!=WBgdpClean$cyear)
sum(WBfdiClean$cyear!=WBgdpGRClean$cyear)
sum(WBfdiClean$cyear!=WBgdpCapClean$cyear)
sum(WBfdiClean$cyear!=WBpopClean$cyear)
sum(WBfdiClean$cyear!=WBgdpDeflatorClean$cyear)

# combine data
setwd(pathData)
wbData <- data.frame(cbind(WBfdiClean,
	fdiGdp=WBfdiGdpClean[,4], gdp=WBgdpClean[,4], gdpGR=WBgdpGRClean[,4],
	gdpCAP=WBgdpCapClean[,4], population=WBpopClean[,4], 
	gdpDeflator=WBgdpDeflatorClean[,4] ) )
###############################################################

###############################################################
# constraints
constraints2 <- constraints[constraints$year>=1960,1:10]
constraints2 <- constraints2[!is.na(constraints2$ccode),]

constraints2$cnts_country <- as.character(constraints2$cnts_country)
constraints2$cnts_country[constraints2$cnts_country=='CONGO (BRA)'] <- 'Congo, Republic of'
constraints2$cnts_country[constraints2$cnts_country=='CONGO (KIN)'] <- 'Congo, Democratic Republic of'
constraints2$cnts_country[constraints2$cnts_country=='CONGO DR'] <- 'Congo, Democratic Republic of'
constraints2$cnts_country[constraints2$cnts_country=='GERMAN DR'] <- "Germany Democratic Republic"

constraints2$cname <- countrycode(constraints2$cnts_country, 'country.name', 'country.name')
constraints2[is.na(constraints2$cname),'cname'] <- countrycode(
	constraints2[is.na(constraints2$cname),'polity_country'],
	'country.name', 'country.name')
constraints2$cname[constraints2$cnts_country=='VIETNAM REP'] <- 'S. VIETNAM'
constraints2$cname[constraints2$cnts_country=='YEMEN PDR'] <- 'S. YEMEN'
constraints2$cname[constraints2$cnts_country=="CZECHOS'KIA"] <- 'CZECH REPUBLIC'
constraints2$cname[constraints2$cnts_country=="YUGOSLAVIA"] <- 'SERBIA'
constraints2 <- constraints2[constraints2$cname!='HONG KONG',]

constraints2$cnameYear <- paste(constraints2$cname, constraints2$year, sep='')

names(table(constraints2$cnameYear)[table(constraints2$cnameYear)>1]) # Dupe check

# Adding in codes from panel
constraints2$ccode <- panel$ccode[match(constraints2$cname,panel$cname)]
constraints2$cyear <- paste(constraints2$ccode, constraints2$year, sep='')
table(constraints2$cyear)[table(constraints2$cyear)>1] # Dupe check

# the only variable we want from this dataset is polconiii
# polconiii has some gap years for a few countries
constraints2 <- constraints2[,c('cname', 'ccode', 'year', 'cyear',
	'polconiii')]
###############################################################

###############################################################
# banks dataset
banks2 <- banks[banks$year>=1960,c('code', 'Wbcode', 'country', 
	'year', paste('domestic', 1:9, sep=''))]
banks2 <- banks2[!is.na(banks2$code),]
banks2$country <- trim(banks2$country)

banks2$country <- as.character(banks2$country)
banks2$country[banks2$country=='Congo (BRA)'] <- 'Congo, Republic of'
banks2$country[banks2$country=='Congo (KIN)'] <- 'Congo, Democratic Republic of'
banks2$country[banks2$country=='German DR'] <- "Germany Democratic Republic" 
banks2$country[banks2$country=='German FR'] <- "Germany" 
banks2 <- banks2[banks2$country!='Cyprus: Turkish Sector',]
banks2 <- banks2[banks2$country!='Cyprus: Greek Sector',]
banks2 <- banks2[banks2$country!='Senegambia',]
banks2 <- banks2[banks2$country!='Somaliland',]
banks2 <- banks2[banks2$code!=1145,] # Removing extra cases for Trinidad
banks2 <- banks2[banks2$code!=1247,] # Removing extra cases for Venezuela

banks2$cname <- countrycode(banks2$country, 'country.name', 'country.name')
banks2$cname[banks2$country=='Vietnam REP'] <- 'S. VIETNAM'
banks2$cname[banks2$country=='Yemen PDR'] <- 'S. YEMEN'
banks2$cname[banks2$country=="Yemen PDR (So. Yemen)"] <- 'S. YEMEN'
banks2$cname[banks2$country=="Yugoslavia"] <- 'SERBIA'
banks2 <- banks2[banks2$cname!='HONG KONG',]
banks2$cname[banks2$country=="Czechoslovakia"] <- 'CZECH REPUBLIC'

drop <- unique(banks2[is.na(banks2$cname),c('country')])
banks2 <- banks2[which(!banks2$country %in% drop),]

banks2$cnameYear <- paste(banks2$cname, banks2$year, sep='')

names(table(banks2$cnameYear)[table(banks2$cnameYear)>1]) # Dupe check

# Adding in codes from panel
banks2$ccode <- panel$ccode[match(banks2$cname,panel$cname)]
banks2$cyear <- paste(banks2$ccode, banks2$year, sep='')
drop <- unique(banks2[is.na(banks2$ccode),'cname'])
banks2 <- banks2[which(!banks2$cname %in% drop),]
table(banks2$cyear)[table(banks2$cyear)>1] # Dupe check
###############################################################

###############################################################
# Polity
polity2 <- polity[polity$year>=1960,3:ncol(polity)]

polity2$country <- as.character(polity2$country)
polity2$country[polity2$country=='UAE'] <- 'United Arab Emirates'
polity2$country[polity2$country=='Congo Brazzaville'] <- 'Congo, Republic of'
polity2$country[polity2$country=='Congo Kinshasa'] <- 'Congo, Democratic Republic of'
polity2$country[polity2$country=='Germany East'] <- "Germany Democratic Republic"
polity2$cname <- countrycode(polity2$country, 'country.name', 'country.name')
polity2$cname[polity2$country=='Yemen South'] <- "S. YEMEN"
polity2$cname[polity2$country=='Vietnam South'] <- "S. VIETNAM"
polity2[polity2$cname=='Yugoslavia', 'cname'] <- 'SERBIA'
polity2[polity2$cname=='Czechoslovakia', 'cname'] <- 'CZECH REPUBLIC'

polity2$cnameYear <- paste(polity2$cname, polity2$year, sep='')

polity2$drop <- 0
polity2[polity2$scode=='ETH' & polity2$year==1993, 'drop'] <- 1
polity2[polity2$scode=='GMY' & polity2$year==1990, 'drop'] <- 1
polity2[polity2$scode=='YGS' & polity2$year==1991, 'drop'] <- 1
polity2[polity2$scode=='YGS' & polity2$year==2006, 'drop'] <- 1
polity2[polity2$scode=='SDN' & polity2$year==2011, 'drop'] <- 1
polity2[polity2$scode=='DRV' & polity2$year==1976, 'drop'] <- 1
polity2[polity2$scode=='YAR' & polity2$year==1990, 'drop'] <- 1
polity2 <- polity2[polity2$drop==0,]; polity2 <- polity2[,1:(ncol(polity2)-1)]

names(table(polity2$cnameYear)[table(polity2$cnameYear)>1]) # Dupe check

# Adding in codes from panel
polity2$ccode <- panel$ccode[match(polity2$cname,panel$cname)]
polity2$cyear <- paste(polity2$ccode, polity2$year, sep='')
table(polity2$cyear)[table(polity2$cyear)>1] # Dupe check

# subsetting to relevant vars and then omitting missing cases
# There are no missing cyears for the polity variable and others
# included below
vars <- c('cname', 'ccode', 'year', 'cyear',
	'democ', 'autoc', 'polity', 'xrreg', 'xrcomp',
	'xropen', 'xconst', 'parreg', 'parcomp', 'exrec', 'exconst', 'polcomp')
polity2 <- na.omit(polity2[,vars])
###############################################################

###############################################################
# ICRG data from PRS group
icrg2 <- icrg

icrg2$Country <- as.character(icrg$Country)
icrg2$Country[icrg2$Country=='Congo-Brazzaville'] <- 'Congo, Republic of'
icrg2$Country[icrg2$Country=='Congo-Kinshasa'] <- 'Congo, Democratic Republic of'
drop <- c("Hong Kong", "New Caledonia")
icrg2 <- icrg2[which(!icrg2$Country %in% drop),]
icrg2$cname <- countrycode(icrg2$Country, 'country.name', 'country.name')
icrg2[icrg2$cname=='Czechoslovakia', 'cname'] <- 'CZECH REPUBLIC'

icrg2$cnameYear <- paste(icrg2$cname, icrg2$Year, sep='')

icrg2$drop <- 0
icrg2[icrg2$Country=='Serbia and Montenegro' & icrg2$Year>=2006, 'drop'] <- 1
icrg2[icrg2$Country=='Serbia' & icrg2$Year<2006, 'drop'] <- 1
icrg2[icrg2$Country=='Czechoslovakia' & icrg2$Year>=1993, 'drop'] <- 1
icrg2[icrg2$Country=='Czech Republic' & icrg2$Year<1993, 'drop'] <- 1
icrg2 <- icrg2[icrg2$drop==0,]; icrg2 <- icrg2[,1:(ncol(icrg2)-1)]

table(icrg2$cnameYear)[table(icrg2$cnameYear)>1]

# Adding in codes from panel
icrg2$ccode <- panel$ccode[match(icrg2$cname,panel$cname)]
icrg2$cyear <- paste(icrg2$ccode, icrg2$Year, sep='')
table(icrg2$cyear)[table(icrg2$cyear)>1] # Dupe check

# with the ICRG dataset there are no data gaps in continuous
# years for countries, there is only variation in when 
# the icrg started to collect data
icrg2 <- na.omit(icrg2)
###############################################################

###############################################################
# Disputes
disputes2 <- disputes

disputes2$Country <- as.character(disputes2$Country)
disputes2 <- disputes2[disputes2$Country!='Zaire',]
disputes2$Country[disputes2$Country=='Congo-Brazzaville'] <- 'Congo, Republic of'
disputes2$Country[disputes2$Country=='Congo-Kinshasa'] <- 'Congo, Democratic Republic of'

disputes2$cname <- countrycode(disputes2$Country, 'country.name', 'country.name')
disputes2$cname[disputes2$cname=='Czechoslovakia'] <- 'CZECH REPUBLIC'

drop <- unique(disputes2[which(disputes2$cname %in% setdiff(disputes2$cname, panel$cname)), 'Country'])
disputes2 <- disputes2[which(!disputes2$Country %in% drop),]

disputes2$cnameYear <- paste(disputes2$cname, disputes2$Year, sep='')

table(disputes2$cnameYear)[table(disputes2$cnameYear)>1] # Dupe check

# Adding in codes from panel
disputes2$ccode <- panel$ccode[match(disputes2$cname,panel$cname)]
disputes2$cyear <- paste(disputes2$ccode, disputes2$Year, sep='')
table(disputes2$cyear)[table(disputes2$cyear)>1] # Dupe check
###############################################################

###############################################################
civwar2 <- war[war$Type==3 | war$Type==4,]
civwar2 <- unique(civwar2[,c('SideA', 'YEAR')])

civwar2$SideA <- as.character(civwar2$SideA)
civwar2 <- civwar2[civwar2$SideA!='Hyderabad',]
civwar2$SideA[civwar2$SideA=='Rumania'] <- 'Romania'
civwar2$SideA[civwar2$SideA=='DR Congo (Zaire) '] <- 'Congo, Democratic Republic of'
civwar2$cname <- countrycode(civwar2$SideA, 'country.name', 'country.name')

civwar2$cname[civwar2$cname=='Yugoslavia'] <- 'SERBIA'
civwar2$ccode <- panel$ccode[match(civwar2$cname,panel$cname)]

civwar2$intWar <- 1
civwar2$cyear <- paste(civwar2$ccode, civwar2$YEAR, sep='')
###############################################################

###############################################################
bitsReporter <- bits[,c('Reporter','ReporterClean','ccodeRep',
	'Year_Signature','Year_force','signedbitsSM', 'ratifiedbitsSM', 
	'PartnerClean', 'ccodePar')]
colnames(bitsReporter) <- c('Country', 'cname', 'ccode', 
	'yearSign', 'yearRat', 'signedbitsSM', 'ratifiedbitsSM','other','othercode')
bitsPartner <- bits[,c('Partner','PartnerClean','ccodePar',
	'Year_Signature','Year_force','signedbitsSM', 'ratifiedbitsSM', 
	'ReporterClean', 'ccodeRep')]
colnames(bitsPartner) <- c('Country', 'cname', 'ccode', 
	'yearSign', 'yearRat', 'signedbitsSM', 'ratifiedbitsSM','other','othercode')
bitsMelt <- data.frame(rbind(bitsReporter,bitsPartner))

bitsMelt$Country <- as.character(bitsMelt$Country)
bitsMelt$Country[bitsMelt$Country=='Congo, DR'] <- 'Congo, Democratic Republic of'
bitsMelt$Country[bitsMelt$Country=="Democratic People's Republic of Korea"] <- 'North Korea'
bitsMelt$Country[bitsMelt$Country=="S\355\243o Tom\355\251 and Principe"] <- 'Sao Tome'
bitsMelt$Country[bitsMelt$Country=="ghanistan"] <- 'Afghanistan'

bitsSigned <- unique(bitsMelt); bitsRatified <- unique(na.omit(bitsMelt))

bitsSigned$cname <- countrycode(bitsSigned$Country, 'country.name', 'country.name')
bitsSigned$cnameYear <- paste(bitsSigned$cname, bitsSigned$yearSign, sep='')
drop <- unique(bitsSigned[which(bitsSigned$cname %in% setdiff(bitsSigned$cname, panel$cname)), 'Country'])
bitsSigned <- bitsSigned[which(!bitsSigned$Country %in% drop),]
bitsSigned$ccode <- panel$ccode[match(bitsSigned$cname,panel$cname)]
bitsSigned$cyear <- paste(bitsSigned$ccode, bitsSigned$yearSign, sep='')
bitsSigned <- summaryBy(signedbitsSM ~ cyear, data=bitsSigned, FUN=(sum))
colnames(bitsSigned)[2] <- 'signedbitsSM'

bitsRatified$cname <- countrycode(bitsRatified$Country, 'country.name', 'country.name')
bitsRatified$cnameYear <- paste(bitsRatified$cname, bitsRatified$yearRat, sep='')
drop <- unique(bitsRatified[which(bitsRatified$cname %in% setdiff(bitsRatified$cname, panel$cname)), 'Country'])
bitsRatified <- bitsRatified[which(!bitsRatified$Country %in% drop),]
bitsRatified$ccode <- panel$ccode[match(bitsRatified$cname,panel$cname)]
bitsRatified$cyear <- paste(bitsRatified$ccode, bitsRatified$yearRat, sep='')
bitsRatified <- summaryBy(signedbitsSM ~ cyear, data=bitsRatified, FUN=(sum))
colnames(bitsRatified)[2] <- 'ratifiedbitsSM'	
###############################################################

###############################################################
# Cleaned sender receiver datasets
setwd(pathData)
save(wbData, constraints2, banks2, polity2, icrg2, disputes2, civwar2, 
	bitsSigned, bitsRatified, 
	file='SRindivCleaned.rda')
###############################################################

###############################################################
# Merge results into panel frame
colnames(panel)[7] <- 'cyear'
panel <- panel[,names(panel)[-ncol(panel)]]
panel <- panel[panel$cname!='Zanzibar',]
dim(panel)
srData <- merge(panel, wbData[,c(4,8:14)], by='cyear', all.x=T)
srData <- merge(srData, constraints2[,c(4:5)], by='cyear', all.x=T)
srData <- merge(srData, banks2[,c(5:13,17)], by='cyear', all.x=T)
srData <- merge(srData, polity2[,c(4:16)], by='cyear', all.x=T)
srData <- merge(srData, icrg2[,c(5:16,20)], by='cyear', all.x=T)
srData <- merge(srData, disputes2[,c(4,7,13)], by='cyear', all.x=T)
srData$conc_disputes[is.na(srData$conc_disputes)] <- 0
srData$pend_disputes[is.na(srData$pend_disputes)] <- 0
srData <- merge(srData, civwar2[,c(5:6)], by='cyear', all.x=T)
srData$intWar[is.na(srData$intWar)] <- 0
srData <- merge(srData, bitsRatified, by='cyear', all.x=T)
srData$ratifiedbitsSM[is.na(srData$ratifiedbitsSM)] <- 0
srData <- merge(srData, bitsSigned, by='cyear', all.x=T)
srData$signedbitsSM[is.na(srData$signedbitsSM)] <- 0
dim(srData)

srData$cyear <- as.numeric(as.character(srData$cyear))
###############################################################

###############################################################
# Var transformations
srData$polity[srData$polity==-88] <- -10
srData$polity[srData$polity==-77] <- -10
srData$polity[srData$polity==-66] <- -10
srData$polity <- srData$polity + 10

Investment.ProfileRescale <- rescale(srData$Investment.Profile,10,0)
Bureaucracy.QualityRescale <- rescale(srData$Bureaucracy.Quality,10,0)
CorruptionRescale <- rescale(srData$Corruption,10,0)
Law.and.OrderRescale <- rescale(srData$Law.and.Order,10,0)
srData$Property.Rights <- (Investment.ProfileRescale + 
	Bureaucracy.QualityRescale +
	CorruptionRescale + Law.and.OrderRescale)

srData$cp_disputes <- srData$conc_disputes + srData$pend_disputes

yearDefl <- na.omit(summaryBy(gdpDeflator ~ year, data=srData[srData$cname=='UNITED STATES',], na.rm=T))
names(yearDefl) <- c('year', 'USgdpDeflYr')
yearDefl$USgdpDefl11 <- yearDefl[yearDefl$year==2011,2]
srData <- merge(srData, yearDefl, by='year', all.x=T, all.y=F)
srData$r_fdi <- (srData$fdi/srData$USgdpDeflYr)*srData$USgdpDefl11
srData$r_gdp <- (srData$gdp/srData$USgdpDeflYr)*srData$USgdpDefl11
srData$r_gdpCAP <- (srData$gdpCAP/srData$USgdpDeflYr)*srData$USgdpDefl11

srData$LNfdi <- logNeg(srData$fdi); srData$LNr_fdi <- logNeg(srData$r_fdi)
srData$LNgdp <- log(srData$gdp); srData$LNr_gdp <- log(srData$r_gdp)
srData$LNgdpCAP <- log(srData$gdpCAP); srData$LNr_gdpCAP <- log(srData$r_gdpCAP)
srData$LNpopulation <- log(srData$population)
###############################################################

###############################################################
# Cumulative BITs
vars <- c('signedbitsSM', 'ratifiedbitsSM')
cumul_data <- lapply(vars, function(x) FUN=cumulTS(var=x))

dim(srData)
for(ii in 1:length(cumul_data)){
	toMerge <- cumul_data[[ii]]
	srData <- merge(srData, toMerge[,c(1,3)], by='cyear', all.x=T, all.y=F) }
dim(srData)
###############################################################

###############################################################
# Spatial BITs
setwd(pathData)
# distMats <- list()
years <- 1984:2011
# date <- paste(years, '-12-31', sep='')
# distMats<- lapply(date, function(x) FUN=distmatrix(as.Date(x), type="mindist", useGW=TRUE))
# names(distMats) <- years
# save(distMats, file='mindistMatrices.rda')
load('mindistMatrices.rda')

BitsDispMindist <- NULL
for(i in 1:length(years)){
	distMat <- distMats[[i]]
	# rownames for matrices
	distNames <- as.numeric(rownames(distMat))
	ndistNames <- panel$ccode[match(distNames, panel$GWCODE)]
	rownames(distMat) <- ndistNames; colnames(distMat) <- ndistNames
	
	# # setting bordering countries as having distance of 1
	# distMat[distMat==0] <- 1
	# inv_dmat <- 1/distMat
	
	# setting neighbors to countries with mintdist of less than or equal to 200km
	inv_dmat <- ifelse(distMat<=200,1,0)
	
	# Setting country x to x values to 0
	diag(inv_dmat) <- 0
	# Applying row standardized weights
	inv_dmat_rowst <- inv_dmat/apply(inv_dmat,1,sum)
	
	# Bringing in fdi dataset
	spat_vars <- c('ccode',
		 "csignedbitsSM", "cratifiedbitsSM")
	dataYear <- srData[srData$year==years[i], spat_vars]
	dataYear <- dataYear[which(dataYear$ccode %in% ndistNames),]
	o <- as.character(dataYear$ccode)
	
	inv_dmat_rowst <- inv_dmat_rowst[o,o]
	# data rows with NAs that are in distance matrix
	# this is equivalent to just dropping them from teh 
	# spatial variable calculation
	dataYear[is.na(dataYear)] <- 0
	
	for(j in 1:nrow(inv_dmat_rowst)){
		row_weights <- NULL
		row_weights <- t(t(dataYear[,c(2:ncol(dataYear))]) %*%  inv_dmat_rowst[j,])
		row_weights2 <- NULL
		row_weights2 <- cbind(row_weights, years[i], dataYear$ccode[j])
		BitsDispMindist <- rbind(BitsDispMindist, row_weights2)
	}
}
BitsDispMindist <- data.frame(BitsDispMindist, row.names=NULL)
names(BitsDispMindist) <- c(
	paste('sp_',names(BitsDispMindist)[1:(length(spat_vars)-1)],sep=''),
	'year','ccode')
BitsDispMindist$cyear <- paste(BitsDispMindist$ccode, BitsDispMindist$year, sep='')
dim(srData); dim(BitsDispMindist)
srData <- merge(srData, BitsDispMindist[,c(1:2,5)], by='cyear', all.x=T)
dim(srData); dim(BitsDispMindist)

srData$sp_cp_disputes[is.na(srData$sp_cp_disputes)] <- 0
srData$sp_csignedbitsSM[is.na(srData$sp_csignedbitsSM)] <- 0
srData$sp_cratifiedbitsSM[is.na(srData$sp_cratifiedbitsSM)] <- 0
###############################################################

###############################################################
# Lagging data
srDatav2 <- lagDataSM(srData, 'cyear', 'ccode', names(srData)[8:ncol(srData)], 1)
###############################################################

###############################################################
# Subsetting data

# Timeframe
srDatav2 <- srDatav2[srDatav2$year>=1984 & srDatav2$year<2010,]

### Countries [trimming to countries for which data was collected]
## Not including constraints because it includes gaps in time
# for certain countries
## Not including icrg because it leads to the dropping of so 
# many cases

# cntries <- Reduce(intersect, list(
# 	unique(wbData$cyear),
# 	unique(banks2$cyear), unique(polity2$cyear), 
# 	unique(icrg2$cyear)))
cntries <- Reduce(intersect, list(
	unique(wbData$cyear), unique(banks2$cyear), unique(polity2$cyear)))

srDatav2 <- srDatav2[which(srDatav2$cyear %in% cntries),]

# Variables
vars <- c('cyear', 'year', 'CNTRY_NAME', 'COWCODE', 'GWCODE', 'cname', 'ccode',

	'domestic9', 'intWar',
	# 'domestic1', 'domestic2', 'domestic3', 'domestic4', 'domestic5',
	# 'domestic6', 'domestic7', 'domestic8', 
	
	'polconiii', 'polity', 
	# 'xrreg', 'xrcomp', 'xropen', 'xconst', 
	# 'parreg', 'parcomp', 'exrec', 'exconst', 'polcomp', 
	
	'Investment.Profile', 'Property.Rights', 
	# 'Government.Stability', 'Socioeconomic.Conditions', 
	# 'Internal.Conflict', 'External.Conflict', 'Corruption', 'Military.in.Politics',
	# 'Religion.in.Politics', 'Law.and.Order', 'Ethnic.Tensions', 
	# 'Democratic.Accountability', 'Bureaucracy.Quality', 

	'cp_disputes', 'csignedbitsSM', 'cratifiedbitsSM',
	'sp_csignedbitsSM', 'sp_cratifiedbitsSM', 

	'gdp', 'LNr_gdp', 
	'LNr_gdpCAP', 'LNr_fdi', 'LNpopulation', 'fdiGdp', 'gdpGR')

allVars <- append(vars, paste('lag1_', vars[8:length(vars)], sep=''))
allVars <- vars # removing lagged variables, keep getting error
srDatav2 <- srDatav2[,allVars]
###############################################################

###############################################################
# Imputting missing values through sbgcop
sbgcopTimeSRdata <- system.time(
  SRdataIMP <- sbgcop.mcmc(
  	srDatav2[,names(srDatav2)[c(2,7,8:ncol(srDatav2))]], 
  	nsamp=5000, seed=123456, oden=10, verb=TRUE)
  )

imputed <- SRdataIMP$Y.pmean
srDatav3 <- cbind(srDatav2[,c('CNTRY_NAME','COWCODE','GWCODE',
	'cname','ccode','year','cyear')], 
	imputed[,3:ncol(imputed)])

save(srDatav3, SRdataIMP, sbgcopTimeSRdata, file='SRdataImputed.rda')
###############################################################