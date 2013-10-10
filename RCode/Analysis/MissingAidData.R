source("/Users/janus829/Desktop/Research/ButheProjects/ForeignAid/RCode/setup.R")

setwd(pathData)
load('aidData.rda')

lengthMiss=function(x){length(x[!is.na(x)])}
availData=summaryBy(aid ~ cnameS + year, data=aidData, FUN=lengthMiss)
write.csv(availData, file='NumberofCountriesbySender.csv')


pdf(file=)
plot(jitter(availData$year), availData$aid.lengthMiss)

