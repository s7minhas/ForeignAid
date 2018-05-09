if(Sys.info()['user']=='s7m' | Sys.info()['user']=='janus829'){ source('~/Research/ForeignAid/RCode/setup.R') }
if(Sys.info()['user']=='cindycheng'){ source('~/Documents/Papers/ForeignAid/RCode/setup.R') }
library(dplyr)
################################################################
setwd(paste0(pathData, '/components/AidDataCore_ResearchRelease_Level1_v3'))

load(paste0(pathData, '/iData_v2.rda'))

senders = iData[[1]]$ccodeS %>% unique() %>% as.character() %>% as.numeric()
panel$CNTRY_NAME[match(senders, panel$ccode)] %>% unique()


recipients = iData[[1]]$ccodeR %>% unique() %>% as.character() %>% as.numeric()
panel$CNTRY_NAME[match(recipients, panel$ccode)] %>% unique() %>% sort()

aidDataRaw= read.csv('AidDataCoreDonorRecipientYearPurpose_ResearchRelease_Level1_v3.1.csv', stringsAsFactors = FALSE)

# Get aggregated data, note you double checked and this aggregation 
# matches the data found in 'AidDataCoreDonorRecipientYear_ResearchRelease_Level1_v3.1.csv'
aidDataAgg = aidDataRaw %>%
	group_by(donor, recipient, year) %>%
	summarise(commitment_amount_usd_constant_sum = sum(commitment_amount_usd_constant_sum)) %>%
	data.frame()

# Get humanitarian data
aidDataRaw$purposeNameAgg = NA
aidDataRaw$purposeNameAgg[which(aidDataRaw$coalesced_purpose_code == 70000)] = 'humanitarianAid'
aidDataRaw$purposeNameAgg[which(aidDataRaw$coalesced_purpose_code %in% c(72000, 72010, 72020, 72030, 72040, 72050))] = 'emergencyResponse'
aidDataRaw$purposeNameAgg[which(aidDataRaw$coalesced_purpose_code %in% c(73010))] = 'reconstructionRelief'
aidDataRaw$purposeNameAgg[which(aidDataRaw$coalesced_purpose_code %in% c(74010))] = 'disasterPreventionRelief'
    
 
aidDataHumanitarian = aidDataRaw[-which(is.na(aidDataRaw$purposeNameAgg)),]
aidDataHumanitarianAgg = aidDataHumanitarian %>% group_by(donor, recipient, year, purposeNameAgg) %>% summarise(commitment_amount_usd_constant_sum = sum(commitment_amount_usd_constant_sum)) %>% data.frame()


 
aidDataHumanitarianAgg = reshape(aidDataHumanitarianAgg,
	timevar = "purposeNameAgg",
	idvar = c('donor', 'recipient', 'year'),
	direction = 'wide')
names(aidDataHumanitarianAgg) = gsub('commitment_amount_usd_constant_sum.', '', names(aidDataHumanitarianAgg))
aidDataHumanitarianAgg$humanitarianTotal = rowSums(aidDataHumanitarianAgg[, c('emergencyResponse', 'humanitarianAid', 'reconstructionRelief', 'disasterPreventionRelief')], na.rm = TRUE)

 
# combine 
aidData = merge(aidDataAgg, aidDataHumanitarianAgg, by = c('donor', 'recipient', 'year'), all.x = TRUE)
aidData[is.na(aidData)] = 0
aidData$notHumanitarianTotal = aidData$commitment_amount_usd_constant_sum - aidData$humanitarianTotal
################################################################

################################################################
# Cleaning country names and adding ccodes
senders=char(unique(aidData$donor))
senders=data.frame(cntry=senders,cnameS=cname(senders), stringsAsFactors = F)
senders[is.na(senders[,2]),1] # check to make sure all NAs are IGOs
senders$cnameS[which(senders$cnameS == 'UNITED STATES OF AMERICA')] = "UNITED STATES"
senders$cnameS[which(senders$cnameS == 'UNITED KINGDOM OF GREAT BRITAIN AND NORTHERN IRELAND')] = "UNITED KINGDOM"
senders$ccodeS=num(panel$ccode[match(senders$cnameS,panel$cname)])
senders[is.na(senders[,3]),1] # check to make sure all NAs are IGOs
 

receivers=char(unique(aidData$recipient))
receivers=data.frame(cbind(cntry=receivers,cnameR=cname(receivers)), stringsAsFactors = F)
receivers$cnameR[receivers$cntry=='Bolivia'] =  "BOLIVIA, PLURINATIONAL STATE OF"
receivers$cnameR[receivers$cntry=='Cape Verde'] = 'CAPE VERDE'
receivers$cnameR[receivers$cntry=='Congo, Republic of'] = "CONGO, REPUBLIC OF"
receivers$cnameR[receivers$cntry=='Congo, Democratic Republic of'] = "CONGO, THE DEMOCRATIC REPUBLIC OF"
receivers$cnameR[receivers$cntry=='Cote d`Ivoire'] =  "COTE D'IVOIRE"
receivers$cnameR[receivers$cntry=='Gambia'] = "GAMBIA"
receivers$cnameR[receivers$cntry=='Guinea-Bissau'] = "GUINEA-BISSAU"
receivers$cnameR[receivers$cntry=='Korea'] = "KOREA, REPUBLIC OF"
receivers$cnameR[receivers$cntry=='Korea, Democratic Republic of'] = "KOREA, DEMOCRATIC PEOPLE'S REPUBLIC OF" 
receivers$cnameR[receivers$cntry=='Iran'] = "IRAN, ISLAMIC REPUBLIC OF"
receivers$cnameR[receivers$cntry=='Libya'] = "LIBYAN ARAB JAMAHIRIYA"
receivers$cnameR[receivers$cntry=='Macedonia, FYR'] = "MACEDONIA, THE FORMER YUGOSLAV REPUBLIC OF"
receivers$cnameR[receivers$cntry=='Moldova'] = "MOLDOVA, REPUBLIC OF"
receivers$cnameR[receivers$cntry=='Tanzania'] = "TANZANIA, UNITED REPUBLIC OF"
receivers$cnameR[receivers$cntry=='Viet Nam'] = 'VIETNAM'
receivers$cnameR[receivers$cntry=='United Kingdom'] = "UNITED KINGDOM"
receivers$cnameR[receivers$cntry=='Yugoslavia'] = "SERBIA"
receivers[is.na(receivers[,2]),1] # check to make sure all NAs are IGOs/regions
receivers$ccodeR=num(panel$ccode[match(receivers$cnameR,panel$cname)])
receivers[is.na(receivers[,3]),1] # check to make sure all NAs are IGOs/regions

names(aidData)[1:2] = c('Sender', 'Receiver')
# Adding back into major dataframe
aidData=merge(aidData,senders,by.x='Sender',by.y='cntry',all.x=T)
aidData=merge(aidData,receivers,by.x='Receiver',by.y='cntry',all.x=T)

 
# Dealing with NAs
aidData=aidData[which(!is.na(aidData$cnameR)),] # gets rid of region/group recipients
aidData=aidData[which(!is.na(aidData$ccodeR)),] # gets rid of small countries
aidData=aidData[which(!is.na(aidData$cnameS)),] # gets rid of IGO sending cases
aidData=aidData[which(!is.na(aidData$cnameR)),] # gets rid of region/group recipients

aidData = aidData[-which(aidData$year == 9999),]  
################################################################

################################################################
# Build Aid adjacency matrices
aidMats=DyadBuild(variable='commitment_amount_usd_constant_sum', dyadData=aidData,
	cntry1='ccodeS', cntry2='ccodeR', time='year',
	pd=1970:2010, panel=panel, directed=TRUE)
aidMatsEmergency = DyadBuild(variable='emergencyResponse', dyadData=aidData,
	cntry1='ccodeS', cntry2='ccodeR', time='year',
	pd=1970:2010, panel=panel, directed=TRUE) 
aidMatsHumanitarian = DyadBuild(variable='humanitarianAid', dyadData=aidData,
	cntry1='ccodeS', cntry2='ccodeR', time='year',
	pd=1970:2010, panel=panel, directed=TRUE) 
aidMatsReconstruction = DyadBuild(variable='reconstructionRelief', dyadData=aidData,
	cntry1='ccodeS', cntry2='ccodeR', time='year',
	pd=1970:2010, panel=panel, directed=TRUE) 
aidMatsDisaster = DyadBuild(variable='disasterPreventionRelief', dyadData=aidData,
	cntry1='ccodeS', cntry2='ccodeR', time='year',
	pd=1970:2010, panel=panel, directed=TRUE) 
aidMatsHumanitarianTotal = DyadBuild(variable='humanitarianTotal', dyadData=aidData,
	cntry1='ccodeS', cntry2='ccodeR', time='year',
	pd=1970:2010, panel=panel, directed=TRUE) 
aidMatsNotHumanitarianTotal = DyadBuild(variable='notHumanitarianTotal', dyadData=aidData,
	cntry1='ccodeS', cntry2='ccodeR', time='year',
	pd=1970:2010, panel=panel, directed=TRUE) 
################################################################

###############################################################
# include only original oecd donor countries
oecd = c('AUSTRIA', 'BELGIUM', 'CANADA', 'DENMARK', 'FRANCE', 'GERMANY',
	'GREECE', 'ICELAND', 'IRELAND', 'ITALY', 'LUXEMBOURG', 'NETHERLANDS',
	'NORWAY', 'PORTUGAL', 'SPAIN', 'SWEDEN', 'UNITED KINGDOM', 'UNITED STATES' )
aidData = aidData[which(aidData$cnameS %in% oecd),]
aidData = aidData[which(!aidData$cnameR %in% oecd),]

# Limit to set number of donors and receivers
dCntries = unique(aidData$cnameS)
rCntries = unique(aidData$cnameR)
################################################################

################################################################
setwd(pathData)
save(aidData, aidMats, aidMatsEmergency, aidMatsHumanitarian, aidMatsReconstruction,
	aidMatsDisaster, aidMatsHumanitarianTotal, aidMatsNotHumanitarianTotal, file='aidDataDisagg.rda')
################################################################