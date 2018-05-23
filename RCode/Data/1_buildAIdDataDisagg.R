if(Sys.info()['user']=='s7m' | Sys.info()['user']=='janus829'){ source('~/Research/ForeignAid/RCode/setup.R') }
if(Sys.info()['user']=='cindycheng'){ source('~/Documents/Papers/ForeignAid/RCode/setup.R') }
library(dplyr)
################################################################
setwd(paste0(pathData, '/components/AidDataCore_ResearchRelease_Level1_v3'))
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
aidDataRaw$purposeNameAgg[which(aidDataRaw$coalesced_purpose_code %in% c(
	72000, 72010, 72020, 72030, 72040, 72050))] = 'emergencyResponse'
aidDataRaw$purposeNameAgg[which(aidDataRaw$coalesced_purpose_code %in% c(73010))] = 'reconstructionRelief'
aidDataRaw$purposeNameAgg[which(aidDataRaw$coalesced_purpose_code %in% c(74010))] = 'disasterPreventionRelief'
 
# Get democracy/civil society data
aidDataRaw$purposeNameAgg[which(aidDataRaw$coalesced_purpose_code %in% c(
	15000, 15100, 15105, 15150))] = 'govCivilSociety'
aidDataRaw$purposeNameAgg[which(aidDataRaw$coalesced_purpose_code %in% c(42010))] = 'womenSupport'
aidDataRaw$purposeNameAgg[which(aidDataRaw$coalesced_purpose_code %in% c(
	92000, 92005, 92010, 92020, 92030))] = 'ngoGovSupport'


# Get development data
aidDataRaw$purposeNameAgg[which(aidDataRaw$coalesced_purpose_code %in% c(
	11000, 11100, 11105, 11110, 11120, 11130, 11182, 11220, 11230, 
	11240, 11320, 11330, 11420, 11430))] = 'education'
aidDataRaw$purposeNameAgg[which(aidDataRaw$coalesced_purpose_code %in% c(
	12000, 12005, 12100, 12105, 12110, 12181, 12182, 12191, 12220, 12230,
	12240,12250, 12261, 12281, 13000, 13005, 13010, 13020, 13030, 13040, 13081))] = 'health'
aidDataRaw$purposeNameAgg[which(aidDataRaw$coalesced_purpose_code %in% c(
	14000, 14005, 14010, 14015, 14020, 14030, 14040, 14050, 14081, 14082))] = 'waterSanitation'
aidDataRaw$purposeNameAgg[which(aidDataRaw$coalesced_purpose_code %in% c(
	16020, 16030, 16050, 16081))] = 'otherSocialInfrastructureAndServices'


aidDataRaw$purposeNameAgg[which(aidDataRaw$coalesced_purpose_code %in% c(
	21005, 21010, 21020, 21030, 21040, 21050, 21061, 21081, # transportation and storage
	23000, 23005, 23010, 23020, 23030, 23040, 23050, 23055, 23081, 23082, # energy
	24000, 24005, 24010, 24020, 24030, 24040, 24081, 25010, 25020,25081# banking
	))] = 'economicInfrastructureAndServices'

aidDataRaw$purposeNameAgg[which(aidDataRaw$coalesced_purpose_code %in% c(
	31000, 31100, 31105,31110,31120,31130, 31140, 31150, 31181, 31182, 31191, 
	31205, 31210, 31220, 31281, 31282, 31291, 31300, 31305, 31310, 31320, 
	31330, 31381, 31382, 31391))] = 'agricultureForestyFishing'
aidDataRaw$purposeNameAgg[which(aidDataRaw$coalesced_purpose_code %in% c(
	32000, 32105, 32110, 32120, 32130, 32140, 32181, 32182, 32191, 32200, 
	32205, 32210, 32220, 32281, 32310))] = 'industryMiningConstruction'
aidDataRaw$purposeNameAgg[which(aidDataRaw$coalesced_purpose_code %in% c(
	41000, 41005, 41010, 41020, 41030, 41040, 41050, 41081, 41082))] = 'environmentalProtection'
aidDataRaw$purposeNameAgg[which(aidDataRaw$coalesced_purpose_code %in% c(
	33210, 43030, 43040, 43050, 43050, 43081,  43082))] = 'otherDevelopmentAid'
aidDataRaw$purposeNameAgg[which(aidDataRaw$coalesced_purpose_code %in% c(
	52010))] = 'foodAid'
aidDataRaw$purposeNameAgg[which(aidDataRaw$coalesced_purpose_code %in% c(
	60010, 60020, 60030, 60040))] = 'debtRelief'



# not included ; the reasons these sectors were not included was that their ability to purely promote development seemed muddled
# Economic and development planning/policy; legal and judicial development; government administration
# `conflict prevention and resolution, peace and security`; 
# `other social infrastructure and services': 16010, social/welfare services
# communications: includes stuff like funding for radio, television
# other commodity assistnace: import support, export support
# trade policy and regulations
# refugees
# unallocated/unsepcified
# administrative costs of donors
 aidDataSectors= aidDataRaw[-which(is.na(aidDataRaw$purposeNameAgg)),]
aidDataSectorsAgg = aidDataSectors %>%
	group_by(donor, recipient, year, purposeNameAgg) %>%
	summarise(commitment_amount_usd_constant_sum = sum(commitment_amount_usd_constant_sum)) %>%
	data.frame()

aidDataSectorsAgg = reshape(aidDataSectorsAgg,
	timevar = "purposeNameAgg",
	idvar = c('donor', 'recipient', 'year'),
	direction = 'wide')
names(aidDataSectorsAgg) = gsub('commitment_amount_usd_constant_sum.', '', names(aidDataSectorsAgg))
aidDataSectorsAgg$humanitarianTotal = rowSums(aidDataSectorsAgg[, 
	c('emergencyResponse', 'humanitarianAid', 'reconstructionRelief', 'disasterPreventionRelief')], na.rm = TRUE)
aidDataSectorsAgg$civSocietyTotal = rowSums(aidDataSectorsAgg[, 
	c('govCivilSociety', 'womenSupport',  'ngoGovSupport')], na.rm = TRUE)
aidDataSectorsAgg$developTotal = rowSums(aidDataSectorsAgg[, 
	c('education', 'health', 'waterSanitation', 'otherSocialInfrastructureAndServices', 
		'economicInfrastructureAndServices', 'industryMiningConstruction', 
		'environmentalProtection', 'otherDevelopmentAid', 'debtRelief')], na.rm = TRUE)
 
  
 
 
# combine 
aidData = merge(aidDataAgg, aidDataSectorsAgg, by = c('donor', 'recipient', 'year'), all.x = TRUE)
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
# aidMatsEmergency = DyadBuild(variable='emergencyResponse', dyadData=aidData,
# 	cntry1='ccodeS', cntry2='ccodeR', time='year',
# 	pd=1970:2010, panel=panel, directed=TRUE) 
# aidMatsHumanitarian = DyadBuild(variable='humanitarianAid', dyadData=aidData,
# 	cntry1='ccodeS', cntry2='ccodeR', time='year',
# 	pd=1970:2010, panel=panel, directed=TRUE) 
# aidMatsReconstruction = DyadBuild(variable='reconstructionRelief', dyadData=aidData,
# 	cntry1='ccodeS', cntry2='ccodeR', time='year',
# 	pd=1970:2010, panel=panel, directed=TRUE) 
# aidMatsDisaster = DyadBuild(variable='disasterPreventionRelief', dyadData=aidData,
# 	cntry1='ccodeS', cntry2='ccodeR', time='year',
# 	pd=1970:2010, panel=panel, directed=TRUE) 
# aidMatsHumanitarianTotal = DyadBuild(variable='humanitarianTotal', dyadData=aidData,
# 	cntry1='ccodeS', cntry2='ccodeR', time='year',
# 	pd=1970:2010, panel=panel, directed=TRUE) 
# aidMatsNotHumanitarianTotal = DyadBuild(variable='notHumanitarianTotal', dyadData=aidData,
# 	cntry1='ccodeS', cntry2='ccodeR', time='year',
# 	pd=1970:2010, panel=panel, directed=TRUE) 
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
save(aidData, 
	#aidMats, aidMatsEmergency, aidMatsHumanitarian, aidMatsReconstruction,
	#aidMatsDisaster, aidMatsHumanitarianTotal, aidMatsNotHumanitarianTotal, 
	file='aidDataDisagg.rda')
################################################################