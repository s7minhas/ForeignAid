
if(Sys.info()['user']=='cindycheng'){
	source('~/Documents/Papers/ForeignAid/RCode/setup.R')
	setwd("/Users/cindycheng/Dropbox/Documents/Papers/ForeignAid/data")
}
if(Sys.info()['user']=='s7m'){
	source('~/Research/ForeignAid/RCode/setup.R')
	setwd(paste0(pathData))
}

library(dplyr)


# sender = 'United States'
# receiver = 'Iran'


# unique(aidDataRaw$donor)

sender = 'United States'
receiver = 'China'

# ---------- All sector disaggregation ---------


aidDataRaw= read.csv(paste0(pathData, '/components/AidDataCore_ResearchRelease_Level1_v3/AidDataCoreDonorRecipientYearPurpose_ResearchRelease_Level1_v3.1.csv'), stringsAsFactors = FALSE)

subAidDataRaw = aidDataRaw[which(aidDataRaw$recipient == receiver & aidDataRaw$donor == sender),]

subAidDataRaw  = subAidDataRaw [which(subAidDataRaw$year > 1973),]
subAidDataRaw$commitment_amount_usd_constant_sum = subAidDataRaw$commitment_amount_usd_constant_sum/1000000

dim(subAidDataRaw)
 
 
ggplot(subAidDataRaw, aes(x = year, y = commitment_amount_usd_constant_sum, group= coalesced_purpose_name, shape = coalesced_purpose_name))+
	geom_jitter(size = 3)+
	geom_line(aes( color = coalesced_purpose_name), size = 1.5)+
	theme(axis.title = element_text(size = 12),
		  axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
		  axis.text.y = element_text(size = 10),
		  legend.position = 'none')  + 
	scale_x_continuous(breaks = c(2002:2013))+
	labs( y = "Committed Amount (USD Constant Dollars, millions)", x = 'Year', shape = "Sector" , color = "Sector")
	#guides(shape = guide_legend(title.position = "top", ncol = 2), color = guide_legend(title.position = "top", ncol = 2)
	)
 

 
pdf(paste0(pathGraphics, '/US_Iran_aid.pdf'))
ggplot(subAidDataRaw, aes(x = year, y = commitment_amount_usd_constant_sum, group= coalesced_purpose_name, shape = coalesced_purpose_name))+
	geom_jitter(size = 3)+
	geom_line(aes( color = coalesced_purpose_name), size = 1.5)+
	theme(axis.title = element_text(size = 12),
		  axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
		  axis.text.y = element_text(size = 10),
		  legend.position = 'bottom' ,
		  legend.text = element_text(size = 8),
		  	legend.title = element_text(size = 10)) + 
	scale_color_manual(values=c(rep("#999999", 4), "#E69F00", rep("#999999", 4), "#56B4E9", rep("#999999", 4), "#DB4811", rep("#999999", 1)  ))+
	scale_shape_manual(values=c(0, 1, 2, 3, 16, 4, 5, 6, 7, 16, 8, 9, 10, 11, 16, 12))+
	scale_x_continuous(breaks = c(2002:2013))+
	scale_x_continuous(breaks = c(2002:2013))+
	labs( y = "Committed Amount (USD Constant Dollars, millions)", x = 'Year', shape = "Sector" , color = "Sector")+
	guides(shape = guide_legend(title.position = "top", ncol = 2), color = guide_legend(title.position = "top", ncol = 2))
dev.off()




dim(subAidDataRaw)


# ---------- Humanitarian Disaggregation --------------
load('noImputationDataAidDisagg.rda')
load('aidDataDisagg.rda')
library(ggplot2)
library(reshape)


unique(df$Sender)

df = aidData

sum(df$humanitarianTotal)/sum(df$commitment_amount_usd_constant_sum)


sum(df$civSocietyTotal)/sum(df$commitment_amount_usd_constant_sum)


sum(df$developTotal)/sum(df$commitment_amount_usd_constant_sum)




subData = df[which(df$Sender == sender & df$Receiver == receiver ),]
unique(subData$year) %>% sort()
subData = subData[which(subData$year > 1980),]

aidVars =  c("commitment_amount_usd_constant_sum",
								"emergencyResponse",
								 "humanitarianAid" ,
								 "disasterPreventionRelief" ,
								 "humanitarianTotal" ,
								  "notHumanitarianTotal")

subDataLong = reshape(subData[, c('year', aidVars)] ,
					varying = aidVars,
					v.names = 'aidAmt',
					timevar  = 'aidType',
					times = aidVars,
					#new.row.names = NULL,
					direction = 'long')
 
ggplot(subDataLong) +
	geom_line(aes(x = year, y = aidAmt, color = aidType))

pdf("foreignAidYrTrend.pdf")


pdf("foreignAidYrTrend.pdf")
plot(1962:2010, tapply(aidData$commitUSD09, aidData$year, sum, na.rm = T), type = "l", xlab = "year", ylab = "Foreign Aid Disbursement")
dev.off()


library(foreign)
news = read.dta(paste0(pathData, '/components/newspressure_1968_2013.dta'))

