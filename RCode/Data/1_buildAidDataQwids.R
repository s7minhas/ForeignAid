if(Sys.info()['user']=='s7m' | Sys.info()['user']=='janus829'){ source('~/Research/ForeignAid/RCode/setup.R') }
if(Sys.info()['user']=='cindycheng'){ source('~/Documents/Papers/ForeignAid/RCode/setup.R') }

################################################################
setwd(paste0(pathData, '/AidData'))
aidData = read.csv('qwidsOECD.csv', skip = 2, stringsAsFactors = F)
 

aidData = reshape(	aidData[, -which(names(aidData) %in% c('X.2', 'Time.Period'))],
					varying = paste('X', 1960:2014, sep = ''),
					v.names = 'commitUSD13',
					timevar = 'year',
					times = 1960:2014,
					direction = 'long')
aidData$commitUSD13 = as.numeric(gsub('\\.\\.', 0, aidData$commitUSD13))
names(aidData)[1:2] = c('Sender', 'Receiver')

aidData =  aidData[-grep('Donor', aidData$Sender),
						-which(names(aidData) == 'id')]
aidData =  aidData[-grep('<', aidData$Sender, fixed = T),]
aidData =  aidData[-grep('{', aidData$Sender, fixed = T),]
aidData =  aidData[-grep('&', aidData$Sender, fixed = T),]

 
################################################################

################################################################
# Cleaning country names and adding ccodes
senders=char(unique(aidData$Sender))
senders=data.frame(cntry=senders,cnameS=cname(senders), stringsAsFactors = F)
senders[is.na(senders[,2]),1] # check to make sure all NAs are IGOs
senders$ccodeS=num(panel$ccode[match(senders$cnameS,panel$cname)])
senders[is.na(senders[,3]),1] # check to make sure all NAs are IGOs

receivers=char(unique(aidData$Receiver))
receivers=data.frame(cbind(cntry=receivers,cnameR=cname(receivers)), stringsAsFactors = F)
receivers$cnameR[receivers$cntry=='Cabo Verde'] = 'CAPE VERDE'
receivers$cnameR[receivers$cntry=='Congo'] = "CONGO, REPUBLIC OF"
receivers$cnameR[receivers$cntry=='Democratic Republic of the Congo'] = "CONGO, THE DEMOCRATIC REPUBLIC OF"
receivers$cnameR[receivers$cntry=='Libya'] = "LIBYAN ARAB JAMAHIRIYA"
receivers$cnameR[receivers$cntry=='Viet Nam'] = 'VIETNAM'
receivers[is.na(receivers[,2]),1] # check to make sure all NAs are IGOs/regions
receivers$ccodeR=num(panel$ccode[match(receivers$cnameR,panel$cname)])
receivers[is.na(receivers[,3]),1] # check to make sure all NAs are IGOs/regions


# Adding back into major dataframe
aidData=merge(aidData,senders,by.x='Sender',by.y='cntry',all.x=T)
aidData=merge(aidData,receivers,by.x='Receiver',by.y='cntry',all.x=T)

# Dealing with NAs
aidData=aidData[which(!is.na(aidData$cnameR)),] # gets rid of region/group recipients
aidData=aidData[which(!is.na(aidData$ccodeR)),] # gets rid of small countries
aidData=aidData[which(!is.na(aidData$cnameS)),] # gets rid of IGO sending cases
aidData=aidData[which(!is.na(aidData$cnameR)),] # gets rid of region/group recipients

  
################################################################

################################################################
# Build Aid adjacency matrices
aidMats=DyadBuild(variable='commitUSD13', dyadData=aidData,
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
save(aidData, aidMats, file='aidDataQwids.rda')
################################################################






