setwd("/Users/cindycheng/Dropbox/ForeignAid/data")
load('aidData.rda')

library(dplyr)
library(magrittr)

pdf("foreignAidYrTrend.pdf")
plot(1962:2010, tapply(aidData$commitUSD09, aidData$year, sum, na.rm = T), type = "l", xlab = "year", ylab = "Foreign Aid Disbursement")
dev.off()