if(Sys.info()['user']=='s7m' | Sys.info()['user']=='janus829'){ source('~/Research/ForeignAid/RCode/setup.R') }

################################################################
setwd(paste0(pathData, '/AidData'))
aidData = read.csv('aiddata2_1_donor_recipient_year.csv')
aidData = aidData[,c('donor','recipient','year','commitment_usd_constant_sum')]
# aidData=read.csv('aidDataAgg.csv')
colnames(aidData)=c('Sender','Receiver','year','commitUSD09')
################################################################

################################################################
# Cleaning country names and adding ccodes
senders=char(unique(aidData$Sender))
senders=data.frame(cbind(cntry=senders,cnameS=cname(senders)))
senders[is.na(senders[,2]),1] # check to make sure all NAs are IGOs
senders$ccodeS=num(panel$ccode[match(senders$cnameS,panel$cname)])
senders[is.na(senders[,3]),1] # check to make sure all NAs are IGOs

receivers=char(unique(aidData$Receiver))
receivers=data.frame(cbind(cntry=receivers,cnameR=cname(receivers)))
receivers$cnameR[receivers$cntry=='CZECHOSLOVAKIA'] = 'CZECH REPUBLIC'
receivers[is.na(receivers[,2]),1] # check to make sure all NAs are IGOs/regions
receivers$ccodeR=num(panel$ccode[match(receivers$cnameR,panel$cname)])
receivers[is.na(receivers[,3]),1] # check to make sure all NAs are IGOs/regions

# Adding back into major dataframe
aidData=merge(aidData,senders,by.x='Sender',by.y='cntry',all.x=T)
aidData=merge(aidData,receivers,by.x='Receiver',by.y='cntry',all.x=T)

# Dealing with NAs
aidData=aidData[which(!is.na(aidData$cnameS)),] # gets rid of IGO sending cases
aidData=aidData[which(!is.na(aidData$cnameR)),] # gets rid of region/group recipients
aidData=aidData[which(!is.na(aidData$ccodeR)),] # gets rid of small countries
aidData=aidData[which(!is.na(aidData$ccodeS)),] # gets rid of small countries
aidData=aidData[which(!is.na(aidData$year)),] # weird cases with taiwan as donor but no year and no commit figure
################################################################

################################################################
# Var mods
aidData$commitUSD09=num(aidData$commitUSD09)
aidData$year=num(aidData$year)
aidData$ccodeR=num(aidData$ccodeR)
aidData$ccodeS=num(aidData$ccodeS)
################################################################

################################################################
# Build Aid adjacency matrices
aidMats=DyadBuild(variable='commitUSD09', dyadData=aidData,
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
save(aidData, aidMats, file='aidData.rda')
################################################################