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

senderNames$cname = countrycode(senderNames$SenderClean, 'country.name', 
	'country.name')

aidData$cname = senderNames$cname[match(aidData$Sender, senderNames$Sender)]
aidData$ccode = panel$ccode[match(aidData$cname, panel$cname)]

setwd(pathData)
save(aidData, file='aidData.rda')