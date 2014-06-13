if(Sys.info()["user"]=="janus829"){source("~/Desktop/Research/ForeignAid/RCode/setup.R")}

################################################################
setwd(paste0(pathData, '/AidData'))
aidData=read.csv('aidDataAgg.csv')
colnames(aidData)=c('Sender','Receiver','year','commitUSD09')
################################################################

# Cleaning country names and adding ccodes
senders=char(unique(aidData$Sender))
senders=data.frame(cbind(cntry=senders,cnameS=cname(senders)))
senders[is.na(senders[,2]),1] # check to make sure all NAs are IGOs
senders$ccodeS=panel$ccode[match(senders$cnameS,panel$cname)]
senders[is.na(senders[,3]),1] # check to make sure all NAs are IGOs

receivers=char(unique(aidData$Receiver))
receivers=data.frame(cbind(cntry=receivers,cnameR=cname(receivers)))
receivers[is.na(receivers[,2]),1] # check to make sure all NAs are IGOs/regions
receivers$ccodeR=panel$ccode[match(receivers$cnameR,panel$cname)]
receivers[is.na(receivers[,3]),1] # check to make sure all NAs are IGOs/regions

# Adding back into major dataframe
aidData=merge(aidData,senders,by.x='Sender',by.y='cntry',all.x=T)
aidData=merge(aidData,receivers,by.x='Receiver',by.y='cntry',all.x=T)

# Dealing with NAs
aidData=aidData[which(!is.na(aidData$cnameS)),] # gets rid of IGO sending cases
aidData=aidData[which(!is.na(aidData$cnameR)),] # gets rid of region/group recipients
aidData=aidData[which(!is.na(aidData$ccodeR)),] # gets rid of small countries


setwd(pathData)
save(aidData, file='aidData.rda')