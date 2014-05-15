if(Sys.info()["user"]=="janus829"){source("~/Desktop/Research/ButheProjects/ForeignAid/RCode/setup.R")}

setwd(paste(pathData, '/AidData', sep=''))
fnames=list.files()
aidList = list()
for(ii in 1:length(fnames)){ aidList[[ii]] = read.csv(fnames[ii]) }
names(aidList) = gsub('.csv','',fnames)

# Cleaning aid dataset
aidData=NULL
for(ii in 1:length(aidList)){
	temp=aidList[[ii]]; temp=temp[-1,
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

senderNames$SenderClean[26] = 'New Zealand'
senderNames$SenderClean[40] = 'United Arab Emirates'

senderNames$cname = cname(senderNames$SenderClean)
aidData$cnameS = senderNames$cname[match(aidData$Sender, senderNames$Sender)]
aidData$ccodeS = panel$ccode[match(aidData$cnameS, panel$cname)]

# Cleaning up Receiver labels
aidData$Country=char(aidData$Country)
aidData$Country[aidData$Country=='Chinese Taipei']='Taiwan'
aidData$cnameR = cname(aidData$Country)

# Checking for  duplicate labels
aidData$cnameRyr=paste(aidData$cnameR, aidData$year, sep='')
table(aidData$cnameRyr)[table(aidData$cnameRyr)>42]

# Adding ccode from panel dataset
aidData$ccodeR = panel$ccode[match(aidData$cnameR, panel$cname)]
# Dropping following small country cases
#  [1] "Anguilla"               "Aruba"                  "Bermuda"               
#  [4] "Cayman Islands"         "Cook Islands"           "Falkland Islands"      
#  [7] "French Polynesia"       "Gibraltar"              "Hong Kong, China"      
# [10] "Macao"                  "Mayotte"                "Montserrat"            
# [13] "Netherlands Antilles"   "New Caledonia"          "Niue"                  
# [16] "Northern Marianas"      "St. Helena"             "Tokelau"               
# [19] "Turks & Caicos Islands" "Virgin Islands (UK)"    "Wallis & Futuna"       
# [22] "West Bank & Gaza Strip"
aidData = aidData[!is.na(aidData$ccodeR),]

# Account for country existence
# To do this I  drop all 
# receiver country cases not in the panel dataset
aidData$ccodeRyr=paste0(aidData$ccodeR, aidData$year)
aidData=aidData[which(aidData$ccodeRyr %in% panel$ccodeYear),]

# Subsetting to cases past 1970 due to missingness issues
aidData=aidData[aidData$year>1970,]

setwd(pathData)
save(aidData, file='aidData.rda')