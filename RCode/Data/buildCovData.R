if(Sys.info()["user"]=="janus829"){
	source("~/Research/ForeignAid/RCode/setup.R") }
if(Sys.info()["user"]=="cindycheng"){
	source("~/Documents/Papers/ForeignAid/RCode/setup.R") }

###############################################################
# WB data
setwd(paste(pathData, '/Components/WB/',sep=''))
WBgdp=read.csv('NY.GDP.MKTP.CD_Indicator_MetaData_en_EXCEL.csv')
WBgdpCap=read.csv('NY.GDP.PCAP.CD_Indicator_MetaData_en_EXCEL.csv')
WBgdpgr=read.csv('NY.GDP.MKTP.KD.ZG_Indicator_MetaData_en_EXCEL.csv')
WBfdi=read.csv('BX.KLT.DINV.CD.WD_Indicator_MetaData_en_EXCEL.csv')
WBfdiGdp=read.csv('BX.KLT.DINV.WD.GD.ZS_Indicator_MetaData_en_EXCEL.csv')
WBpop=read.csv('SP.POP.TOTL_Indicator_MetaData_en_EXCEL.csv')
setwd(paste(pathData, '/Components/NeedsData',sep=''))
life = read.csv('life_expectancy.csv')
litAdult=read.csv('literacy_adult.csv')
litYouth=read.csv('literacy_youth.csv')
malHeight=read.csv('malnutrition_height.csv')
malWeight=read.csv('malnutrition_weight.csv')
mort5=read.csv('mortality_under5.csv')
schPre=read.csv('schoolenrollment_preprimary.csv')
schPri=read.csv('schoolenrollment_primary.csv')

WBgdpClean=cleanWbData(WBgdp, 'gdp')
WBgdpCapClean=cleanWbData(WBgdpCap, 'gdpCAP')
WBgdpgrClean=cleanWbData(WBgdpgr, 'gdpGR')
WBfdiClean=cleanWbData(WBfdi, 'fdi')
WBfdiGdpClean=cleanWbData(WBfdiGdp, 'fdiGDP')
WBpopClean=cleanWbData(WBpop, 'population')
lifeClean=cleanWbData(life, 'lifeExpect')
litAdultClean=cleanWbData(litAdult, 'litAdult')
litYouthClean=cleanWbData(litYouth, 'litYouth')
malHeightClean=cleanWbData(malHeight, 'malHeight')
malWeightClean=cleanWbData(malWeight, 'malWeight')
mort5Clean=cleanWbData(mort5, 'mort5')
schPreClean=cleanWbData(schPre, 'schPre')
schPriClean=cleanWbData(schPri, 'schPri')

# Make sure order matches
sum(WBfdiClean$cyear!=WBfdiGdpClean$cyear)
sum(WBfdiClean$cyear!=WBgdpClean$cyear)
sum(WBfdiClean$cyear!=WBgdpCapClean$cyear)
sum(WBfdiClean$cyear!=WBgdpgrClean$cyear)
sum(WBfdiClean$cyear!=WBpopClean$cyear)
sum(WBfdiClean$cyear!=lifeClean$cyear)
sum(WBfdiClean$cyear!=litAdultClean$cyear)
sum(WBfdiClean$cyear!=litYouthClean$cyear)
sum(WBfdiClean$cyear!=malHeightClean$cyear)
sum(WBfdiClean$cyear!=malWeightClean$cyear)
sum(WBfdiClean$cyear!=mort5Clean$cyear)
sum(WBfdiClean$cyear!=schPreClean$cyear)
sum(WBfdiClean$cyear!=schPriClean$cyear)

# combine data
setwd(pathData)
wbData=data.frame(cbind(WBgdpClean,
	gdpCAP=WBgdpCapClean[,4],
	gdpGR=WBgdpgrClean[,4],
	fdi=WBfdiClean[,4],
	fdiGdp=WBfdiGdpClean[,4],
	population=WBpopClean[,4],
	lifeExpect=lifeClean[,4],
	litAdult=litAdultClean[,4],
	litYouth=litYouthClean[,4],	
	malHeight=malHeightClean[,4],
	malWeight=malWeightClean[,4],
	mort5=mort5Clean[,4],
	schPre=schPreClean[,4],
	schPri=schPriClean[,4]
	 ) )
###############################################################

###############################################################
# Polity
setwd(paste(pathData, '/Components',sep=''))
polity=read.csv('p4v2011.csv')

polity2=polity[polity$year>=1960,3:ncol(polity)]

polity2$country=as.character(polity2$country)
polity2$country[polity2$country=='UAE']='United Arab Emirates'
polity2$country[polity2$country=='Congo Brazzaville']='Congo, Republic of'
polity2$country[polity2$country=='Congo Kinshasa']='Congo, Democratic Republic of'
polity2$country[polity2$country=='Germany East']="Germany Democratic Republic"
polity2$cname=cname(polity2$country)
polity2$cname[polity2$country=='Yemen South']="S. YEMEN"
polity2$cname[polity2$country=='Vietnam South']="S. VIETNAM"
polity2[polity2$country=='Yugoslavia', 'cname']='SERBIA'
polity2[polity2$country=='Czechoslovakia', 'cname']='CZECH REPUBLIC'

polity2$cnameYear=paste(polity2$cname, polity2$year, sep='')

polity2$drop=0
polity2[polity2$scode=='ETH' & polity2$year==1993, 'drop']=1
polity2[polity2$scode=='GMY' & polity2$year==1990, 'drop']=1
polity2[polity2$scode=='YGS' & polity2$year==1991, 'drop']=1
polity2[polity2$scode=='YGS' & polity2$year==2006, 'drop']=1
polity2[polity2$scode=='SDN' & polity2$year==2011, 'drop']=1
polity2[polity2$scode=='DRV' & polity2$year==1976, 'drop']=1
polity2[polity2$scode=='YAR' & polity2$year==1990, 'drop']=1
polity2=polity2[polity2$drop==0,]; polity2=polity2[,1:(ncol(polity2)-1)]

names(table(polity2$cnameYear)[table(polity2$cnameYear)>1]) # Dupe check

# Adding in codes from panel
polity2$ccode=panel$ccode[match(polity2$cname,panel$cname)]
polity2$cyear=paste(polity2$ccode, polity2$year, sep='')
table(polity2$cyear)[table(polity2$cyear)>1] # Dupe check
###############################################################

###############################################################
# ICRG data from PRS group
setwd(paste(pathData, '/Components',sep=''))
icrg=read.csv('PRS_Melted_Format.csv')

icrg2=icrg

icrg2$Country=as.character(icrg$Country)
icrg2$Country[icrg2$Country=='Congo-Brazzaville']='Congo, Republic of'
icrg2$Country[icrg2$Country=='Congo-Kinshasa']='Congo, Democratic Republic of'
drop=c("Hong Kong", "New Caledonia")
icrg2=icrg2[which(!icrg2$Country %in% drop),]
icrg2$cname=cname(icrg2$Country)
icrg2[icrg2$cname=='Czechoslovakia', 'cname']='CZECH REPUBLIC'

icrg2$cnameYear=paste(icrg2$cname, icrg2$Year, sep='')

icrg2$drop=0
icrg2[icrg2$Country=='Serbia and Montenegro' & icrg2$Year>=2006, 'drop']=1
icrg2[icrg2$Country=='Serbia' & icrg2$Year<2006, 'drop']=1
icrg2[icrg2$Country=='Czechoslovakia' & icrg2$Year>=1993, 'drop']=1
icrg2[icrg2$Country=='Czech Republic' & icrg2$Year<1993, 'drop']=1
icrg2=icrg2[icrg2$drop==0,]; icrg2=icrg2[,1:(ncol(icrg2)-1)]

table(icrg2$cnameYear)[table(icrg2$cnameYear)>1]

# Adding in codes from panel
icrg2$ccode=panel$ccode[match(icrg2$cname,panel$cname)]
icrg2$cyear=paste(icrg2$ccode, icrg2$Year, sep='')
table(icrg2$cyear)[table(icrg2$cyear)>1] # Dupe check
###############################################################

###############################################################
# PRIO Civil War
setwd(paste(pathData, '/Components/PRIO_ArmedConflict', sep=''))
war=read.csv('ucdp.prio.armed.conflict.v4.2013.csv')
civwar=unique(war[war$Type==3 | war$Type==4,c('SideA', 'YEAR')])

# Cleaning country names
civwar$SideA=as.character(civwar$SideA)
civwar=civwar[civwar$SideA!='Hyderabad',]
civwar$SideA[civwar$SideA=='United Arab Emirate']='United Arab Emirates'
civwar$SideA[civwar$SideA=='Rumania']='Romania'
civwar$SideA[civwar$SideA=='Serbia (Yugoslavia)']='SERBIA'
civwar$SideA[civwar$SideA=='DR Congo (Zaire) ']='Congo, Democratic Republic of'

civwar$cname=cname(civwar$SideA)
civwar$cname[civwar$cname=='Czechoslovakia']='CZECH REPUBLIC'
civwar$cnameYear=paste(civwar$cname,civwar$YEAR,sep='')
names(table(civwar$cnameYear)[table(civwar$cnameYear)>1])

civwar$ccode=panel$ccode[match(civwar$cname,panel$cname)]
civwar$cyear=paste(civwar$ccode, civwar$YEAR, sep='')
names(table(civwar$cyear)[table(civwar$cyear)>1])

civwar$civwar=1
###############################################################

###############################################################
# banks dataset
setwd(paste(pathData, '/Components/Banks Cross National Time Series', sep=''))
banks=read.csv('CNTSDATA.csv')

banks2=banks[banks$year>=1960,c('code', 'Wbcode', 'country', 
	'year', paste('domestic', 1:9, sep=''))]
banks2=banks2[!is.na(banks2$code),]
banks2$country=trim(banks2$country)

banks2$country=as.character(banks2$country)
banks2$country[banks2$country=='Congo (BRA)']='Congo, Republic of'
banks2$country[banks2$country=='Congo (KIN)']='Congo, Democratic Republic of'
banks2$country[banks2$country=='German DR']="Germany Democratic Republic" 
banks2$country[banks2$country=='German FR']="Germany" 
banks2=banks2[banks2$country!='Cyprus: Turkish Sector',]
banks2=banks2[banks2$country!='Cyprus: Greek Sector',]
banks2=banks2[banks2$country!='Senegambia',]
banks2=banks2[banks2$country!='Somaliland',]
banks2=banks2[banks2$code!=1145,] # Removing extra cases for Trinidad
banks2=banks2[banks2$code!=1247,] # Removing extra cases for Venezuela

banks2$cname=cname(banks2$country)
banks2$cname[banks2$country=='Vietnam REP']='S. VIETNAM'
banks2$cname[banks2$country=='Yemen PDR']='S. YEMEN'
banks2$cname[banks2$country=="Yemen PDR (So. Yemen)"]='S. YEMEN'
banks2$cname[banks2$country=="Yugoslavia"]='SERBIA'
banks2=banks2[banks2$cname!='HONG KONG',]
banks2$cname[banks2$country=="Czechoslovakia"]='CZECH REPUBLIC'

drop=unique(banks2[is.na(banks2$cname),c('country')])
banks2=banks2[which(!banks2$country %in% drop),]

banks2$cnameYear=paste(banks2$cname, banks2$year, sep='')

names(table(banks2$cnameYear)[table(banks2$cnameYear)>1]) # Dupe check

# Adding in codes from panel
banks2$ccode=panel$ccode[match(banks2$cname,panel$cname)]
banks2$cyear=paste(banks2$ccode, banks2$year, sep='')
drop=unique(banks2[is.na(banks2$ccode),'cname'])
banks2=banks2[which(!banks2$cname %in% drop),]
table(banks2$cyear)[table(banks2$cyear)>1] # Dupe check
###############################################################

###############################################################
setwd(paste(pathData, '/Components',sep=''))
fh=read.csv('FHdata.csv')
fh$Country=trim(as.character(fh$Country))

fh$drop=0
fh[fh$Country=='Germany' & fh$Year<1990, 'drop']=1
fh[fh$Country=='Germany, E.' & fh$Year>=1990, 'drop']=1
fh[fh$Country=='Germany, W.' & fh$Year>=1990, 'drop']=1
fh[fh$Country=='Russia' & fh$Year<1991, 'drop']=1
fh[fh$Country=='USSR' & fh$Year>=1991, 'drop']=1
fh[fh$Country=='Vietnam' & fh$Year<1977, 'drop']=1
fh[fh$Country=='Vietnam, N.' & fh$Year>=1977, 'drop']=1
fh[fh$Country=='Vietnam, S.' & fh$Year>=1977, 'drop']=1
fh[fh$Country=='Yemen' & fh$Year<1990, 'drop']=1
fh[fh$Country=='Yemen, N.' & fh$Year>=1990, 'drop']=1
fh[fh$Country=='Yemen, S.' & fh$Year>=1990, 'drop']=1

fh[fh$Country=='Yugoslavia' & fh$Year<1992, 'drop']=1
fh[fh$Country=='Yugoslavia (Serbia & Montenegro)' & fh$Year>=1992, 'drop']=1
fh=fh[fh$drop==0,]; fh=fh[,1:(ncol(fh)-1)]

fh$Country[fh$Country=='Congo (Brazzaville)']='Congo, Republic of'
fh$Country[fh$Country=='Congo (Kinshasa)']='Congo, Democratic Republic of'
fh$Country[fh$Country=='Germany, E.']="Germany Democratic Republic" 
fh$Country[fh$Country=='Germany, W.']="Germany" 

fh$cname=cname(fh$Country)
fh$cname[fh$Country=='Vietnam, S.']='S. VIETNAM'
fh$cname[fh$Country=='Yemen, S.']='S. YEMEN'

fh$cnameYear=paste(fh$cname, fh$Year, sep='')

names(table(fh$cnameYear)[table(fh$cnameYear)>1]) # Dupe check

# Adding in codes from panel
fh$ccode=panel$ccode[match(fh$cname,panel$cname)]
fh$cyear=paste(fh$ccode, fh$Year, sep='')
drop=unique(fh[is.na(fh$ccode),'cname'])
fh=fh[which(!fh$cname %in% drop),]
table(fh$cyear)[table(fh$cyear)>1] # Dupe check
###############################################################

###############################################################
# food supply
setwd(paste(pathData, '/Components/NeedsData',sep=''))
food = read.csv('foodsupply.csv')

food2=melt(food[,c(1,7:ncol(food))], id='countries')
names(food2)=c('Country', 'year', 'food')
food2$year=num(substr(food2$year,2,5))

food2$drop=0
food2[food2$Country=='Ethiopia PDR' & food2$year>=1993,'drop']=1
food2[food2$Country=='Ethiopia' & food2$year<1993,'drop']=1
food2[food2$Country=='Belgium-Luxembourg' & food2$year>=2000,'drop']=1
food2[food2$Country=='Luxembourg' & food2$year<2000,'drop']=1
food2[food2$Country=='USSR' & food2$year>=1992,'drop']=1
food2[food2$Country=='Russian Federation' & food2$year<1992,'drop']=1
food2[food2$Country=='Yugoslav SFR' & food2$year>=1992,'drop']=1
food2[food2$Country=='Serbia and Montenegro' & food2$year<1992,'drop']=1
food2[food2$Country=='Serbia and Montenegro' & food2$year>=2006,'drop']=1
food2[food2$Country=='Serbia' & food2$year<2006,'drop']=1
food2[food2$Country=='Czechoslovakia' & food2$year>=1993,'drop']=1
food2[food2$Country=='Czech Republic' & food2$year<1993,'drop']=1
food2=food2[food2$drop!=1,]; food2=food2[,1:(ncol(food2)-1)]

food2$cname=cname(food2$Country)
food2$cname[food2$Country=="Yugoslav SFR"]='SERBIA'
food2$cname[food2$Country=="Czechoslovakia"]='CZECH REPUBLIC'
food2$cname[food2$Country=="Democratic People's Republic of Korea"]="KOREA, DEMOCRATIC PEOPLE'S REPUBLIC OF"
food2$cname[food2$Country=="Republic of Korea"]="KOREA, REPUBLIC OF"
food2$cnameYear=paste(food2$cname, food2$year, sep='')
names(table(food2$cnameYear)[table(food2$cnameYear)>1])

food2$ccode=panel$ccode[match(food2$cname,panel$cname)]
food2$cyear=paste(food2$ccode,food2$year,sep='')
drop=unique(food2[is.na(food2$ccode),'cname'])
food2=food2[which(!food2$cname %in% drop),]
names(table(food2$cyear)[table(food2$cyear)>1])
###############################################################

###############################################################
# Natural disaster data
emdat2=read.csv(paste0(pathData,'/Components/EMDAT/emdat.csv'))
emdat=emdat2

# Convert to standard country names
emdat$country_name=trim(emdat$country_name)
emdat$country_name[emdat$country_name=='Germany Dem Rep']="Germany Democratic Republic"
emdat$cname=cname(emdat$country_name)

# Drop some countries
emdat$drop=0
emdat[emdat$country_name=='Germany Fed Rep' & emdat$year==1991, 'drop']=1
emdat = emdat[which(emdat$drop!=1),]

# Cname corrections
emdat$cname[emdat$country_name=='Czechoslovakia']='CZECH REPUBLIC'
emdat$cname[emdat$country_name=='Yemen P Dem Rep']='S. YEMEN'
emdat$cname[emdat$country_name=='Yugoslavia']='SERBIA'

# Dupe check
emdat$cnameYear=paste(emdat$cname, emdat$year, sep='')
names(table(emdat$cnameYear)[table(emdat$cnameYear)>1]) # Dupe check

# Add countrycode
emdat$ccode=panel$ccode[match(emdat$cname,panel$cname)]
emdat$cyear=paste(emdat$ccode,emdat$year,sep='')

# Drop countries with no ccodes: throws away small islands
emdat = emdat[!is.na(emdat$ccode),]

# Final dupe check
names(table(emdat$cyear)[table(emdat$cyear)>1])
###############################################################

###############################################################
# Colonial dummy data
library(doBy)
colony = read.csv(paste0(pathData,'/Components/ICOW Colonial History 1.0/coldata100.csv'))

# subset data - note that original colonial ruler, the country that a colony gained independence from and the country that a colony seceded from are not necessarily the same - as such I stack the data below to extract this info
colonyA = colony[, c('State', 'Name', 'ColRuler', 'IndDate', 'GWsys')]
colonyB = colony[, c('State', 'Name', 'IndFrom', 'IndDate', 'GWsys')]
colonyC = colony[, c('State', 'Name', 'SecFrom', 'IndDate', 'GWsys')]
names(colonyB)[3] = 'ColRuler'  ; names(colonyC)[3] = 'ColRuler'
colony2 = rbind(colonyA, colonyB, colonyC)

# remove dupes
colony2 = colony2[-which(duplicated(colony2)),]

# extract years
colony2$IndYr = as.numeric(substr(colony2$IndDate, 1, 4))
colony2$GWSysYr = as.numeric(substr(colony2$GWsys, 1, 4))

# clean up names of the colonizers
colony3 = colony2[-which(colony2$GWSysYr == -9|colony2$ColRuler ==-9), -which(names(colony2) %in% c('IndDate', 'GWsys'))]
colony3$ColRulerName = panel$CNTRY_NAME[match(colony3$ColRuler, panel$ccode)]
colony3$ColRuler[which(is.na(colony3$ColRulerName))] 
colony3$ColRulerName[which(is.na(colony3$ColRulerName))]= c("Germany", rep("Austria-Hungary", 5), rep("United States", 5), rep("Germany", 2), rep("Austria-Hungary", 2), rep("Korea", 2))
colony3$colony = 1
 
# expand dataset to panel format
colony4 = panelyear(colony3, colony3$IndYr, rep(2015, length(colony3$IndYr)))

# clean up and aggregate years
colony5 = colony4[which(colony4$year>1959),]
colony5$colony[which(colony5$year < colony5$GWSysYr)] = 0

# make variable for number of years since a colony gained independence/seceded
colony5$colYears = unlist(lapply(split(colony5$colony, colony5$Name), cumsum))
 
# reshape data and make dummy variables for each colonizer
colony5 = colony5[-which(colony5$ColRulerName == "Austria-Hungary"), - which(names(colony5) %in% c("IndYr", "GWSysYr"))]
colony6 = reshape(colony5, timevar = "ColRulerName", idvar = c("State", "Name", "ColRuler", "colYears", "year"), direction = 'wide', sep = "_")
colony6[is.na(colony6)] = 0
 
# aggregate by colony and year
names(colony6)[c(6, 18, 28)] = c("colony_UK", "colony_US", "colony_SouthAfrica")
colony5$ColRulerName[which(colony5$ColRulerName=="United Kingdom")] = "UK"
colony5$ColRulerName[which(colony5$ColRulerName=="South Africa")] = "SouthAfrica"
colony5$ColRulerName[which(colony5$ColRulerName=="United States")] = "US"

 
colony7 = summaryBy(formula(paste(paste("colony", unique(colony5$ColRulerName), sep = "_", collapse = "+"), paste("Name", "year", sep = "+"), sep = "~")), FUN = sum, data = colony6, keep.names = T, id = c("State", "ColRuler", "colYears"))
colony7$colDummy = 1
colony7$numColonizers = rowSums(colony7[, c(3:34)])
 
# fix country names
colony7$State[which(colony7$State == 316)] = 315
colony7$ccode = panel$ccode[match(colony7$State, panel$ccode)]
colony7$cname = panel$cname[match(colony7$State, panel$ccode)]
colony7$country_name = panel$CNTRY_NAME[match(colony7$State, panel$ccode)]
colony7$colony_code = panel$ccode[match(colony7$ColRuler, panel$ccode)]
colony7$colony_cname = panel$cname[match(colony7$ColRuler, panel$ccode)]
colony7$colony_name = panel$CNTRY_NAME[match(colony7$ColRuler, panel$ccode)]
colony7$cyear=paste(colony7$ccode,colony7$year,sep='')

# Dupe check
colony7$cnameYearColony=paste(paste(colony7$cname, colony7$year, sep=''), colony7$colony_cname, sep = '')
names(table(colony7$cnameYearColony)[table(colony7$cnameYearColony)>1])
which(duplicated(colony7$cyear)==T)
 
colonyFINAL = colony7[ , c(38:45, 2, 37, 46, 47, 3:34)]
names(colonyFINAL)
###############################################################

###############################################################
# Combining data
frame=unique(panel[,c('ccode', 'cname')])
dframe=NULL; frame$year=NA; years=seq(1960,2012,1)
for(ii in 1:length(years)){
	frame$year=years[ii]; dframe=rbind(dframe, frame) }
dframe$cyear=paste(dframe$ccode, dframe$year, sep='')
dim(dframe)
covData=merge(dframe, wbData[,c(4,8:ncol(wbData))],by='cyear',all.x=T,all.y=F)
unique(covData[is.na(covData$ccode), 1:5]); dim(covData)
covData=merge(covData, polity2[,c(7:10,12:20,ncol(polity2))],by='cyear',all.x=T,all.y=F)
unique(covData[is.na(covData$ccode), 1:5]); dim(covData)
covData=merge(covData, icrg2[,c(5:16,ncol(icrg2))],by='cyear',all.x=T,all.y=F)
unique(covData[is.na(covData$ccode), 1:5]); dim(covData)
covData=merge(covData, banks2[,c(5:13,ncol(banks2))],by='cyear',all.x=T,all.y=F)
unique(covData[is.na(covData$ccode), 1:5]); dim(covData)
covData=merge(covData, civwar[,6:7],by='cyear',all.x=T,all.y=F)
unique(covData[is.na(covData$ccode), 1:5]); dim(covData)
covData=merge(covData, fh[,c(3:5,ncol(fh))],by='cyear',all.x=T,all.y=F)
unique(covData[is.na(covData$ccode), 1:5]); dim(covData)
covData=merge(covData, food2[,c(3,ncol(food2))],by='cyear',all.x=T,all.y=F)
unique(covData[is.na(covData$ccode), 1:5]); dim(covData)
covData=merge(covData, emdat[,c(4:10,15)], by='cyear', all.x=T, all.y=F)
unique(covData[is.na(covData$ccode), 1:5]); dim(covData)
covData=merge(covData, colonyFINAL[,c(11, 1, 2, 10,13:44)], by='cyear', all.x=T, all.y=F)
unique(covData[is.na(covData$ccode), 1:5]); dim(covData)
covData[, 65:99][is.na(covData[, 65:99])]= 0

setwd(pathData)
save(covData, file='covData.rda')
###############################################################