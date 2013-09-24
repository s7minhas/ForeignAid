source("/Users/janus829/Desktop/Research/ButheProjects/ForeignAid/RCode/setup.R")

setwd(paste(pathData, '/AidData', sep=''))
fnames=list.files()
aidList = list()
for(ii in 1:length(fnames)){ aidList[[ii]] = read.csv(fnames[ii]) }
names(aidList) = gsub('.csv','',fnames)

# Collapsing to panel format
aidData = NULL
for(ii in 1:length(fnames)){
	temp=aidList[[ii]]
	temp=temp[-1,-c(ncol(temp), 2)]
	temp2=data.frame(apply(temp[,2:ncol(temp)], 2, function(x) FUN=numSM(x)))
	temp=cbind(Country=as.character(temp[,1]), temp2)
	names(temp) = c('Country', 1960:2012)
	cleaned=cleanWbData(temp, 'aid')
	aidData = rbind(aidData, cbind(Sender=names(aidList)[ii], cleaned)) }

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