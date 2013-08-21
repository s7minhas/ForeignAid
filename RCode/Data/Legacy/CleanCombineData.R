### Goal of this file is to create matching IDs for the various
## datasets being used in this analysis

### Load setup
source('/Users/janus829/Desktop/Research/ButheProjects/ForeignAid/RCode/setup.R')

### Load data
setwd(pathData)
load('allData.rda')

# countrycode
write.csv(na.omit(cbind(
	1:999,
	countrycode(1:999,'cown','country.name'))),
	file='cowcodes_countrynames.csv')

###############################################################
# Organizing WB data
### Fx for Melting/Cleaning WB Data for Merge
cleanWbData <- function(data, variable){
	var <- variable
	mdata <- melt(data, id=c('Country.Name', 'Country.Code'))
	names(mdata)[4] <- var
	mdata$year <-  as.numeric(as.character(substring(mdata$variable,2)))
	mdata <- mdata[,c(1,2,5,4)]

	# Remove non-country observations
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
		'World')
	mdata <- mdata[which(!mdata$Country.Name %in% drop),]

	# WB abbreviations vary for some measures
	mdata$Country.Code <- as.character(mdata$Country.Code)
	mdata[mdata$Country.Name=='Andorra','Country.Code'] <- 'ADO'
	mdata[mdata$Country.Name=='Congo, Dem. Rep.','Country.Code'] <- 'ZAR'
	mdata[mdata$Country.Name=='Isle of Man','Country.Code'] <- 'IMY'
	mdata[mdata$Country.Name=='Romania','Country.Code'] <- 'ROM'
	mdata[mdata$Country.Name=='Timor-Leste','Country.Code'] <- 'TMP'
	mdata[mdata$Country.Name=='West Bank and Gaza','Country.Code'] <- 'WBG'

	# Incorp common IDs from countrycode
	mdata2 <- cbind(mdata[,1:2], 
		cname=countrycode(mdata$Country.Code, 'wb', 'country.name'),
		ccode=countrycode(mdata$Country.Code, 'wb', 'cown'),
		mdata[,3:4])

	# Manually adding ccodes for select countries
	mdata2[mdata2$Country.Name=='Korea, Dem. Rep.','ccode'] <- 731
	mdata2[mdata2$Country.Name=='Aruba','ccode'] <- 1000
	mdata2[mdata2$Country.Name=='Bermuda','ccode'] <- 1001
	mdata2[mdata2$Country.Name=='Cayman Islands','ccode'] <- 1002
	mdata2[mdata2$Country.Name=='Channel Islands','ccode'] <- 1003
	mdata2$cname <- as.character(mdata2$cname)
	mdata2[mdata2$Country.Name=='Channel Islands','cname'] <- 'CHANNEL ISLANDS'
	mdata2[mdata2$Country.Name=='Curacao','ccode'] <- 1004
	mdata2[mdata2$Country.Name=='Faeroe Islands','ccode'] <- 1005
	mdata2[mdata2$Country.Name=='French Polynesia','ccode'] <- 1006
	mdata2[mdata2$Country.Name=='Greenland','ccode'] <- 1007
	mdata2[mdata2$Country.Name=='Guam','ccode'] <- 1008
	mdata2[mdata2$Country.Name=='Hong Kong SAR, China','ccode'] <- 1009
	mdata2[mdata2$Country.Name=='Isle of Man','ccode'] <- 1010
	mdata2[mdata2$Country.Name=='Macao SAR, China','ccode'] <- 1011
	mdata2[mdata2$Country.Name=='New Caledonia','ccode'] <- 1012
	mdata2[mdata2$Country.Name=='Northern Mariana Islands','ccode'] <- 1013
	mdata2[mdata2$Country.Name=='Puerto Rico','ccode'] <- 1014
	mdata2[mdata2$Country.Name=='Serbia','ccode'] <- 1015
	mdata2[mdata2$Country.Name=='Sint Maarten (Dutch part)','ccode'] <- 1016
	mdata2[mdata2$Country.Name=='St. Martin (French part)','ccode'] <- 1017
	mdata2[mdata2$Country.Name=='Turks and Caicos Islands','ccode'] <- 1018
	mdata2[mdata2$Country.Name=='Virgin Islands (U.S.)','ccode'] <- 1019
	mdata2[mdata2$Country.Name=='West Bank and Gaza','ccode'] <- 1020
	mdata2[mdata2$Country.Name=='American Samoa','ccode'] <- 1021
	mdata2 }

WBinflDeflatorClean <- cleanWbData(WBinflDeflator, 'inflDeflator')
WBinflDeflatorClean$cyear <- 
	as.numeric(as.character(
			paste(WBinflDeflatorClean$ccode, WBinflDeflatorClean$year, sep='')))

WBgdpDeflatorClean <- cleanWbData(WBgdpDeflator, 'gdpDeflator')
WBgdpDeflatorClean$cyear <- 
	as.numeric(as.character(
		paste(WBgdpDeflatorClean$ccode, WBgdpDeflatorClean$year, sep='')))

WBgdpCapClean <- cleanWbData(WBgdpCap, 'gdpCAP')
WBgdpCapClean$cyear <- 
	as.numeric(as.character(
		paste(WBgdpCapClean$ccode, WBgdpCapClean$year, sep='')))

WBpopClean <- cleanWbData(WBpop, 'population')
WBpopClean$cyear <- 
	as.numeric(as.character(
		paste(WBpopClean$ccode, WBpopClean$year, sep='')))	

# Make sure order matches
sum(WBinflDeflatorClean$cyear!=WBgdpDeflatorClean$cyear)
sum(WBinflDeflatorClean$cyear!=WBgdpCapClean$cyear)
sum(WBinflDeflatorClean$cyear!=WBpopClean$cyear)

# combine data
setwd(pathData)
wbData <- data.frame(cbind(WBinflDeflatorClean,
	gdpDeflator=WBgdpDeflatorClean[,6], 
	gdpCAP=WBgdpCapClean[,6],
	population=WBpopClean[,6]))
save(wbData, file='wbData.rda')
###############################################################

###############################################################
# Polity
polity2 <- polity[polity$year>=1960,3:ncol(polity)]
polity2$ccode <- countrycode(polity2$country, 'country.name', 'cown') 

# Manual Corrections
# unique(polity2[which(is.na(polity2$ccode)),1:3])
polity2[polity2$country=="Korea North", 'ccode'] <- 731
polity2[polity2$country=="Serbia", 'ccode'] <- 1015
polity2[polity2$country=="UAE", 'ccode'] <- 696
polity2[polity2$country=="Serbia and Montenegro", 'ccode'] <- 345
polity2[polity2$country=="Germany East", 'ccode'] <- 265
polity2[polity2$country=="Congo Kinshasa", 'ccode'] <- 490

polity2$cyear <- as.numeric(as.character(
	paste(polity2$ccode, polity2$year, sep='')))

# Need to remove duplicate cases
polity2$temp <- 1:nrow(polity2)
# N. Vietnam 1976 and Vietnam repeat 1976
# Ethiopia repeat 1993
# W Germany and Germany repeat 1990
# Drop S. Vietnam between 1960 to 1975
# Drop Sudan repeat 2011
# Drop South Yemen
multiples <- names(table(polity2$cyear)[table(polity2$cyear)>1])
temp <- unique(polity2[which(polity2$cyear %in% multiples), c(1:4,10,ncol(polity2))])
rownames(temp) <- 1:nrow(temp)
# temp
# Choose cases to drop
drop <- temp[c(17,18,20,22:37,38,64,66,67:90),'temp']
polity2 <- polity2[which(!polity2$temp %in% drop),]
polity2 <- polity2[,1:36]
###############################################################

###############################################################
# ICRG data from PRS group
icrg$ccode <- countrycode(icrg$Country, 'country.name', 'cown')

# Manual corrections
# unique(icrg[is.na(icrg$ccode),1:3])
icrg[icrg$Country=='Hong Kong', 'ccode'] <- 1009
icrg[icrg$Country=='Korea, North', 'ccode'] <- 731
icrg[icrg$Country=='New Caledonia', 'ccode'] <- 1012
icrg[icrg$Country=='Serbia and Montenegro', 'ccode'] <- 345
icrg[icrg$Country=='Congo-Brazzaville', 'ccode'] <- 484
icrg[icrg$Country=='Congo-Kinshasa', 'ccode'] <- 490
icrg <- icrg[which(!icrg$Country %in% 'Serbia'),] # Drop Serbia
icrg <- icrg[which(!is.na(icrg$ccode)),] # Dropping extra NA cases

icrg$cyear <- as.numeric(as.character(
	paste(icrg$ccode, icrg$Year, sep='')))

# Remove duplicate cases
# icrg$temp <- 1:nrow(icrg)
# multiples <- names(table(icrg$cyear)[table(icrg$cyear)>1])
# temp <- unique(icrg[which(icrg$cyear %in% multiples), c(2,4,17,18,19)])
# rownames(temp) <- 1:nrow(temp)
# temp[order(temp$ccode),]
###############################################################

###############################################################
# WGI
cleanWgData <- function(data, variable){
	colnames(data)[1:2] <- c('Country.Name','Country.Code')
	data2 <- cleanWbData(data, variable)	

	data2[data2$cname=='AMERICAN SAMOA','ccode'] <- 1021
	data2[data2$cname=='ANGUILLA','ccode'] <- 1022
	data2[data2$cname=='ARUBA','ccode'] <- 1000
	data2[data2$cname=='BERMUDA','ccode'] <- 1001
	data2[data2$cname=='CAYMAN ISLANDS','ccode'] <- 1002
	data2[data2$cname=='COOK ISLANDS','ccode'] <- 1023
	data2[data2$cname=='FRENCH GUIANA','ccode'] <- 1024
	data2[data2$cname=='GREENLAND','ccode'] <- 1007
	data2[data2$cname=='GUAM','ccode'] <- 1008
	data2[data2$cname=='HONG KONG','ccode'] <- 1009
	data2[data2$cname=='JERSEY','ccode'] <- 1025
	data2[data2$cname=="KOREA, DEMOCRATIC PEOPLE'S REPUBLIC OF",'ccode'] <- 731
	data2[data2$cname=='MACAO','ccode'] <- 1011
	data2[data2$cname=='MARTINIQUE','ccode'] <- 1026
	data2 <- data2[which(data2$cname!='NETHERLANDS ANTILLES'),]
	data2[data2$cname=='NEW CALEDONIA','ccode'] <- 1012
	data2 <- data2[which(data2$cname!='NIUE'),]
	data2[data2$cname=='PUERTO RICO','ccode'] <- 1014
	data2 <- data2[which(data2$cname!='REUNION'),]
	data2[data2$cname=='SERBIA','ccode'] <- 345
	data2[data2$cname=='VIRGIN ISLANDS, U.S.','ccode'] <- 1019
	data2[data2$cname=='PALESTINIAN TERRITORY, OCCUPIED','ccode'] <- 1020

	data2$cyear <- 
		as.numeric(as.character(
			paste(data2$ccode, data2$year, sep='')))
	data2 }

WGIregQualClean <- cleanWgData(WGIregQual, 'WGIregQual')
WGIruleLawClean <- cleanWgData(WGIruleLaw, 'WGIruleLaw')
WGIcorrClean <- cleanWgData(WGIcorr, 'WGIcorr')

# temp <- data2[is.na(data2$ccode),3:4];unique(temp)
# Make sure order matches
# sum(WGIregQualClean$cyear!=WGIruleLawClean$cyear)
# sum(WGIregQualClean$cyear!=WGIcorrClean$cyear)

# combine data
setwd(pathData)
wgData <- data.frame(cbind(WGIregQualClean,
	WGIruleLaw=WGIruleLawClean[,6],
	WGIcorr=WGIcorrClean[,6]))
save(wgData, file='wgData.rda')
###############################################################

###############################################################
# Banks data
banksData$ccode <- countrycode(banksData$sftgname, 'country.name', 'cown')

banksData[banksData$sftgname=="Korea, North", 'ccode'] <- 731
banksData[banksData$sftgname=="Serbia", 'ccode'] <- 1015
banksData[banksData$sftgname=="Serbia and Montenegro", 'ccode'] <- 345
banksData[banksData$sftgname=='Congo-Brazzaville', 'ccode'] <- 484
banksData[banksData$sftgname=='Congo-Kinshasa', 'ccode'] <- 490
banksData[banksData$sftgname=="Germany, East", 'ccode'] <- 265

banksData[banksData$sftgname=='Ethiopia' & banksData$year>=1993,] <-
	 banksData[banksData$sftgname=='Ethiopia(1993-)' & banksData$year>=1993,]
banksData <- banksData[which(banksData$sftgname!='Ethiopia(1993-)'),]

banksData[banksData$sftgname=='Germany' & banksData$year<1990,] <-
	 banksData[banksData$sftgname=='Germany, West' & banksData$year<1990,]
banksData <- banksData[which(banksData$sftgname!='Germany, West'),]

banksData[banksData$sftgname=='Serbia and Montenegro' & banksData$year<1992,] <-
	 banksData[banksData$sftgname=='Yugoslavia, Former' & banksData$year<1992,]
banksData <- banksData[which(banksData$sftgname!='Yugoslavia, Former'),]

banksData[banksData$sftgname=='Russia' & banksData$year<1991,] <-
	 banksData[banksData$sftgname=='USSR(Soviet Union)' & banksData$year<1991,]
banksData <- banksData[which(banksData$sftgname!='USSR(Soviet Union)'),]

banksData[banksData$sftgname=='Pakistan(1972-)' & banksData$year<1972,] <-
	 banksData[banksData$sftgname=='Pakistan(1947-1971)' & banksData$year<1972,]
banksData <- banksData[which(banksData$sftgname!='Pakistan(1947-1971)'),]

banksData <- banksData[which(banksData$sftgname!='Yemen, North'),]
banksData <- banksData[which(banksData$sftgname!='Yemen, South'),]
banksData <- banksData[which(banksData$sftgname!='Vietnam, North'),]
banksData <- banksData[which(banksData$sftgname!='Vietnam, South'),]

banksData$cyear <- as.numeric(as.character(
			paste(banksData$ccode, banksData$year, sep='')))


# unique(banksData[is.na(banksData$ccode),'sftgname'])
# multiples <- unique(substr(names(table(banksData$cyear)[table(banksData$cyear)>1]),1,3))
# temp <- unique(banksData[which(banksData$ccode %in% multiples),c('sftgname', 'ccode')])
# temp[order(temp$ccode),]
# banksData[banksData$ccode==816,1:5]
###############################################################

###############################################################
# OECD data
OECDdata$ccode <- countrycode(OECDdata$Recipient, 'country.name', 'cown')

OECDdata[OECDdata$Recipient=="Korea, Dem. Rep.", 'ccode'] <- 731
OECDdata[OECDdata$Recipient=='Aruba','ccode'] <- 1000
OECDdata[OECDdata$Recipient=='Bermuda','ccode'] <- 1001
OECDdata[OECDdata$Recipient=='Cook Islands','ccode'] <- 1023
OECDdata[OECDdata$Recipient=='French Polynesia','ccode'] <- 1006
OECDdata[OECDdata$Recipient=='Hong Kong, China','ccode'] <- 1009
OECDdata[OECDdata$Recipient=='Macao','ccode'] <- 1011
OECDdata[OECDdata$Recipient=='New Caledonia','ccode'] <- 1012
OECDdata[OECDdata$Recipient=='Serbia','ccode'] <- 345
OECDdata[OECDdata$Recipient=='West Bank & Gaza Strip','ccode'] <- 1020

OECDdata <- OECDdata[which(OECDdata$Recipient!='Gibraltar'),]
OECDdata <- OECDdata[which(OECDdata$Recipient!='St. Helena'),]
OECDdata <- OECDdata[which(OECDdata$Recipient!='States Ex-Yugoslavia'),]

OECDdata$cyear <- as.numeric(as.character(
			paste(OECDdata$ccode, OECDdata$Year, sep='')))

# unique(OECDdata[is.na(OECDdata$ccode),'Recipient'])
# multiples <- unique(substr(names(table(OECDdata$cyear)[table(OECDdata$cyear)>1]),1,3))
# temp <- unique(OECDdata[which(OECDdata$ccode %in% multiples),c('Recipient', 'ccode')])
# temp[order(temp$ccode),]
# OECDdata[OECDdata$ccode==345,1:4]
###############################################################

###############################################################
# UNDP data
UNDPdata$ccode <- countrycode(UNDPdata$Country, 'country.name', 'cown')

UNDPdata[UNDPdata$Country=='Hong Kong, China (SAR)', 'ccode'] <- 1009
UNDPdata[UNDPdata$Country=="Korea (Democratic People's Rep. of)", 'ccode'] <- 731
UNDPdata[UNDPdata$Country=='Palestine, State of','ccode'] <- 1020
UNDPdata[UNDPdata$Country=='Serbia','ccode'] <- 345

UNDPdata$cyear <- as.numeric(as.character(
			paste(UNDPdata$ccode, UNDPdata$Year, sep='')))

# unique(UNDPdata[is.na(UNDPdata$ccode),'Country'])
# multiples <- unique(substr(names(table(UNDPdata$cyear)[table(UNDPdata$cyear)>1]),1,3))
# temp <- unique(UNDPdata[which(UNDPdata$ccode %in% multiples),c('Country', 'ccode')])
# temp[order(temp$ccode),]
###############################################################

###############################################################
# Combining data
setwd(pathData)
save(wbData, polity2, icrg, wgData, banksData, OECDdata, UNDPdata, file='cleanedData.rda')

### Load setup
source('/Users/janus829/Desktop/Research/ButheProjects/ForeignAid/RCode/setup.R')
setwd(pathData)
load('cleanedData.rda')

frame <- na.omit(cbind(1:999, countrycode(1:999,'cown','country.name')))
frame <- rbind(frame, matrix(c('731', 'NORTH KOREA'),nrow=1,ncol=2,byrow=T))
frame <- data.frame(frame)
frame$X1 <- as.numeric(as.character(frame$X1))
colnames(frame) <- c('ccode', 'cname')

dframe <- NULL; frame$year <- NA; years <- seq(1960,2012,1)
for(ii in 1:length(years)){
	frame$year <- years[ii]; dframe <- rbind(dframe, frame) }
dframe$cyear <- paste(dframe$ccode, dframe$year, sep='')

combData <- merge(dframe, wbData[,c(6:10)],by='cyear',all.x=T,all.y=F)
unique(combData[is.na(combData$ccode), 1:5]); dim(combData)
combData <- merge(combData, polity2[,c(7:36)],by='cyear',all.x=T,all.y=F)
unique(combData[is.na(combData$ccode), 1:5]); dim(combData)
combData <- merge(combData, icrg[,c(5:16,18)],by='cyear',all.x=T,all.y=F)
unique(combData[is.na(combData$ccode), 1:5]); dim(combData)
combData <- merge(combData, wgData[,c(6:9)],by='cyear',all.x=T,all.y=F)
unique(combData[is.na(combData$ccode), 1:5]); dim(combData)
combData <- merge(combData, banksData[,c(4:12,14)],by='cyear',all.x=T,all.y=F)
unique(combData[is.na(combData$ccode), 1:5]); dim(combData)
combData <- merge(combData, OECDdata[,c(3:11,13)],by='cyear',all.x=T,all.y=F)
unique(combData[is.na(combData$ccode), 1:5]); dim(combData)
combData <- merge(combData, UNDPdata[,c(3:10,12)],by='cyear',all.x=T,all.y=F)
unique(combData[is.na(combData$ccode), 1:5]); dim(combData)

save(combData, file='combinedData.rda')
write.csv(combData, file='combinedData.csv')
###############################################################