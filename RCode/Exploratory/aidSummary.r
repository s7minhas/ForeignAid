 

if(Sys.info()['user']=='cindycheng'){
	source('~/Documents/Papers/ForeignAid/RCode/setup.R')
	setwd("/Users/cindycheng/Dropbox/Documents/Papers/ForeignAid/data")
}
if(Sys.info()['user']=='s7m'){
	source('~/Research/ForeignAid/RCode/setup.R')
	setwd(paste0(pathData))
}
 
library(dplyr)

 
 
sender = 'United States'
receiver = 'Iran'


# ---------- All sector disaggregation ---------
# Create Figure 1
load(paste0(pathData, '/aidDataDisagg.rda'))
aidDataDisaggLong = reshape(aidData[, c('Receiver', 'Sender', 'year', names(aidData)[grep('Total', names(aidData))][1:3])],
							varying = names(aidData)[grep('Total', names(aidData))][1:3],
							v.names = 'aidAmt',
							timevar = 'aidType',
							times = names(aidData)[grep('Total', names(aidData))][1:3],
							direction = 'long' )

subAidDataRaw = aidDataDisaggLong[which(aidDataDisaggLong$Receiver == receiver & aidDataDisaggLong$Sender == sender),]

subAidDataRaw = subAidDataRaw[which(subAidDataRaw$year > 1974),]
subAidDataRaw$aidAmt = subAidDataRaw$aidAmt/1000000

subAidDataRaw$aidLabel = as.factor(subAidDataRaw$aidType) 
levels(subAidDataRaw$aidLabel) = c('Civil Society Aid', 'Development Aid', 'Humanitarian Aid')
subAidDataRaw$aidLabel = factor(subAidDataRaw$aidLabel, levels(subAidDataRaw$aidLabel)[c(3,1,2)])

# viz 
usiran = ggplot(subAidDataRaw, aes(x = year, y = aidAmt, group= aidLabel)) +
	geom_line(aes( linetype = aidLabel), size = 1.5, alpha = .65, position=position_jitter(w=0.00, h=.05))+
	geom_point()+
	guides(linetype=guide_legend(title=""))+
	theme(
		axis.title = element_text(size = 16),
		axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
		axis.text.y = element_text(size = 12),
		legend.position = 'bottom' ,
		legend.text = element_text(size = 12),
		legend.title = element_text(size = 14),
		panel.border = element_blank(),
		axis.ticks = element_blank()
		) +
	scale_linetype_manual(values=c("solid", "dashed", "dotted")) +
	labs(
		y = "Committed Amount\n(USD Constant Dollars, millions)",
		x = 'Year', shape = "Sector" , color = "Sector")
ggsave(usiran, file=paste0(pathGraphics, '/US_Iran_aid.pdf'), width=12, height=8)
pathGraphics = '~/Research/10739895rngwzsfjtzbw/graphics'
ggsave(usiran, file=paste0(pathGraphics, '/US_Iran_aid.pdf'), width=12, height=8)


# ---------- Humanitarian Disaggregation --------------
load('noImputationDataAidDisagg.rda')
load('aidDataDisagg.rda')
library(ggplot2)
library(reshape)


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
plot(1962:2010, tapply(aidData$commitUSD09, aidData$year, sum, na.rm = T), type = "l", xlab = "year", ylab = "Foreign Aid Disbursement")
dev.off()


library(foreign)
news = read.dta(paste0(pathData, '/components/newspressure_1968_2013.dta'))

