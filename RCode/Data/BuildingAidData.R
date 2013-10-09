source("/Users/janus829/Desktop/Research/ButheProjects/ForeignAid/RCode/setup.R")

setwd(paste(pathData, '/AidData', sep=''))
fnames=list.files(); fnames=fnames[-which(fnames=='aidData.rda')]
aidList = list()
for(ii in 1:length(fnames)){ aidList[[ii]] = read.csv(fnames[ii]) }
names(aidList) = gsub('.csv','',fnames)

# Cleaning aid dataset
aidData=NULL
for(ii in 1:length(aidList)){
	temp=aidList[[10]]; temp=temp[-1,
		-which(names(temp) %in% c('Time.Period', 'X.1') ) ]
	aid=melt( temp[,c(1,3:(ncol(temp)-1) ) ], id='X' )
	names(aid)=c('Country', 'year', 'aid')
	aid$year=num(substr(aid$year,2,5))
	aid$aid=as.character(aid$aid)
	aid$aid[aid$aid==".."]=NA
	aid$aid=num(aid$aid)
	aidData=rbind(aidData, cbind(Sender=names(aidList)[ii], aid)) }

# Cleaning sender names
senderNames = data.frame(cbind(Sender=as.character(unique(aidData$Sender)), 
	SenderClean=as.character(unique(aidData$Sender))))
senderNames$SenderClean = as.character(senderNames$SenderClean)

senderNames$SenderClean[7] = 'Czech Republic'
senderNames$SenderClean[28] = 'New Zealand'
senderNames$SenderClean[34] = 'Saudi Arabia'
senderNames$SenderClean[35] = 'Slovakia'
senderNames$SenderClean[42] = 'United Kingdom'
senderNames$SenderClean[43] = 'United States'

senderNames$cname = cname(senderNames$SenderClean)
aidData$cnameS = senderNames$cname[match(aidData$Sender, senderNames$Sender)]
aidData$ccodeS = panel$ccode[match(aidData$cnameS, panel$cname)]

# Cleaning up Receiver labels
aidData$cnameR = cname(aidData$Country)
# Dropping following receiver cases:
# East African Community Mekong Delta Project   Sts Ex-Yugo. Unspec. 
aidData = aidData[!is.na(aidData$cnameR),]

# Removing dupes
aidData$cnameR[aidData$Country=="Korea, Dem. Rep."]="KOREA, DEMOCRATIC PEOPLE'S REPUBLIC OF"
aidData=aidData[aidData$Country!='Chinese Taipei',]

# Dealing with duplicate labels
aidData$cnameRyr=paste(aidData$cnameR, aidData$year, sep='')
table(aidData$cnameRyr)[table(aidData$cnameRyr)>43]

aidData$ccodeR = panel$ccode[match(aidData$cnameR, panel$cname)]
# Dropping following small country cases
#  [1] Anguilla               Aruba                  Bermuda               
#  [4] Cayman Islands         Cook Islands           Falkland Islands      
#  [7] French Polynesia       Gibraltar              Hong Kong, China      
# [10] Macao                  Mayotte                Montserrat            
# [13] Netherlands Antilles   New Caledonia          Niue                  
# [16] Northern Marianas      St. Helena             Tokelau               
# [19] Turks & Caicos Islands
aidData = aidData[!is.na(aidData$ccodeR),]

# Dealing with duplicate codes
aidData$ccodeRyr=paste(aidData$ccodeR, aidData$year, sep='')
table(aidData$ccodeRyr)[table(aidData$ccodeRyr)>43]

# Account for country existence
# To do this we will essentially drop all 
# receiver country cases that are not in the panel dataset
aidData=aidData[which(aidData$ccodeRyr %in% panel$ccodeYear),]

setwd(pathData)
save(aidData, file='aidData.rda')