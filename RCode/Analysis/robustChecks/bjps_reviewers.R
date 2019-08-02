if(Sys.info()['user']=='s7m' | Sys.info()['user']=='janus829'){ source('~/Research/ForeignAid/RCode/setup.R') }
if(Sys.info()['user']=='cindycheng'){ source('~/Documents/Papers/ForeignAid/RCode/setup.R') }

# ---------------------------------------------

# ---------------------------------------------

# get some descriptive statistics on IGO data to respond to reviewer 2's comments about not knowing enough about the IGOs in the dataset


igo = read.csv(paste0(pathData, '/components/COW_IGO/igounit_v2.3.csv'), stringsAsFactors = FALSE)
unique(igo$ioname) %>% length()
unique(igo$longorgname)



# ---------------------------------------------

# ---------------------------------------------
# get/analyze info on somalia to respond to reviewer 1's comments about the need for more anecdotal evidence for H2

somalia = read.csv(paste0(pathData, '/AidData/USAID/somalia.csv'), stringsAsFactors = FALSE)
somalia$fiscal_year = as.numeric(somalia$fiscal_year)

 
somalia_sub = somalia[which(somalia$dac_purpose_name %in% c( 
	#"Security system management and reform"  ,
	#'Civilian peace-building, conflict prevention and resolution',
	'Legislatures and political parties',
	'Legal and judicial development',
	'Democratic participation and civil society',
	'Elections',
	'Human rights',
	"Anti-corruption organisations and institutions",
	 "Reintegration and SALW control" ,
	 "Radio/television/print media",

	 "Decentralisation and support to subnational government",
	 "Media and free flow of information",
	 "Women's equality organisations and institutions")),]

 
somalia_subAgg = somalia_sub %>% group_by(fiscal_year, dac_purpose_name) %>% summarise(constant_amount = sum(constant_amount, na.rm = TRUE), 
																				current_amount = sum(current_amount, na.rm = TRUE))

ggplot(somalia_subAgg, aes(x=fiscal_year, y = constant_amount))+
geom_line(aes(color =dac_purpose_name))+
scale_x_continuous(breaks =2001:2018 )+
theme(axis.text.x = element_text(angle=90) )

somalia_subAgg[which(somalia_subAgg$fiscal_year == 2004),]
somalia_subAgg[which(somalia_subAgg$fiscal_year == 2007),]

somaliaAggAll = somalia_sub %>% group_by(fiscal_year) %>% summarise(constant_amount = sum(constant_amount, na.rm = TRUE), 
																				current_amount = sum(current_amount, na.rm = TRUE)) 
ggplot(somaliaAggAll, aes(x=fiscal_year, y = constant_amount))+
geom_line( )+
scale_x_continuous(breaks =1992:2018 )+
theme(axis.text.x = element_text(angle=90) )

data[which(data$dac_purpose_name == "Civilian peace-building, conflict prevention and resolution"  ),]


# ---------------------------------------------

# ---------------------------------------------
# get/analyze info on indonesia to respond to reviewer 1's comments about the need for more anecdotal evidence for H2

indonesia = read.csv(paste0(pathData, '/AidData/USAID/indonesia.csv'), stringsAsFactors = FALSE)
indonesia$fiscal_year = as.numeric(indonesia$fiscal_year)

 
indonesia_sub = indonesia[which(indonesia$dac_purpose_name %in% c( 
	#"Security system management and reform"  ,
	#'Civilian peace-building, conflict prevention and resolution',
	'Legislatures and political parties',
	'Legal and judicial development',
	'Democratic participation and civil society',
	'Elections',
	'Human rights',
	"Anti-corruption organisations and institutions",
	 "Reintegration and SALW control" ,
	 "Radio/television/print media",

	 "Decentralisation and support to subnational government",
	 "Media and free flow of information",
	 "Women's equality organisations and institutions")),]

 
indonesia_subAgg = indonesia_sub %>% group_by(fiscal_year, dac_purpose_name) %>% summarise(constant_amount = sum(constant_amount, na.rm = TRUE), 
																				current_amount = sum(current_amount, na.rm = TRUE))
ggplot(indonesia_subAgg, aes(x=fiscal_year, y = constant_amount))+
geom_line(aes(color =dac_purpose_name))+
scale_x_continuous(breaks =1992:2018 )+
theme(axis.text.x = element_text(angle=90) )


# ---------------------------------------------

# ---------------------------------------------
# missigness of no killed, total affected and total damages

load(file=paste0(pathData, '/noImputationDataAidDisagg_v3.rda'))

summary(regData$Lno_killed)
summary(regData$Lno_killed)
summary(regData$Ltotal_affected)

 

# ---------------------------------------------

# ---------------------------------------------
# strategic labelling? 

library(tidyr)
setwd(pathData)
load(file=paste0(pathData, '/noImputationDataAidDisagg_v3.rda'))
regData$Lno_disastersDum = ifelse(regData$Lno_disasters == 0, 0, 1)
 
plotData = regData[, c('cnameS', 'cnameR', 'year', 'Lno_disasters', 'Lno_disastersDum', 'LstratMu', 'education', 'health', 'waterSanitation', 'otherSocialInfrastructureAndServices', 
		'economicInfrastructureAndServices', 'industryMiningConstruction', 
		'environmentalProtection', 'otherDevelopmentAid', 'debtRelief')]

 

plotData = gather(plotData, aidType, aidValue, education:debtRelief)
plotData$Lno_disastersDum = as.factor(plotData$Lno_disastersDum) 
plotData$LstratMuQuintile = ifelse(plotData$LstratMu < quantile(plotData$LstratMu, .1, na.rm = TRUE), 10,
							ifelse(plotData$LstratMu >= quantile(plotData$LstratMu, .1, na.rm = TRUE) & plotData$LstratMu < quantile(plotData$LstratMu, .2, na.rm = TRUE), 20,  
							ifelse(plotData$LstratMu >= quantile(plotData$LstratMu, .2, na.rm = TRUE) & plotData$LstratMu < quantile(plotData$LstratMu, .3, na.rm = TRUE), 30,
							ifelse(plotData$LstratMu >= quantile(plotData$LstratMu, .3, na.rm = TRUE) & plotData$LstratMu < quantile(plotData$LstratMu, .4, na.rm = TRUE), 40,
							ifelse(plotData$LstratMu >= quantile(plotData$LstratMu, .4, na.rm = TRUE) & plotData$LstratMu < quantile(plotData$LstratMu, .5, na.rm = TRUE), 50,
							ifelse(plotData$LstratMu >= quantile(plotData$LstratMu, .5, na.rm = TRUE) & plotData$LstratMu < quantile(plotData$LstratMu, .6, na.rm = TRUE), 60,
							ifelse(plotData$LstratMu >= quantile(plotData$LstratMu, .6, na.rm = TRUE) & plotData$LstratMu < quantile(plotData$LstratMu, .7, na.rm = TRUE), 70,
							ifelse(plotData$LstratMu >= quantile(plotData$LstratMu, .7, na.rm = TRUE) & plotData$LstratMu < quantile(plotData$LstratMu, .8, na.rm = TRUE), 80,
							ifelse(plotData$LstratMu >= quantile(plotData$LstratMu, .8, na.rm = TRUE) & plotData$LstratMu < quantile(plotData$LstratMu, .9, na.rm = TRUE), 90, 100)))))))))
plotData = plotData[-which(is.na(plotData$LstratMu)),]


# restrict sample so that there are about the same number of observations of countries that have and have not experienced natural disasters
prop.table(table(plotData$Lno_disasters))
# .24+.13+.06
plotData = plotData[which(plotData$Lno_disasters<4),]
plotData$Lno_disasters[which(plotData$Lno_disasters %in% c(1,2,3))] = 1

plotDataAgg = plotData %>% group_by(aidType, Lno_disasters, LstratMuQuintile) %>% summarise(aidValue = sum(aidValue, na.rm = TRUE)) %>% data.frame()

 

disast_labels = c('0 Disasters', 
				 '1 - 3 Disasters' )
names(disast_labels) = 0:1
v <- ggplot(plotDataAgg, aes(x = LstratMuQuintile, y = aidValue, fill = aidType)) + 
			geom_area()+
			labs(x = 'Strategic Distance', y = 'Value of Aid Committments')+
			theme(legend.position="bottom")+ 
			guides(fill=guide_legend(nrow=3,byrow=TRUE, title.position = 'top'))+
			scale_fill_discrete(name="Type of Development Aid Committment",
                         labels=c("Debt Relief", 
                         			"Economic Infrastructure and Services", 
                         			"Education",
                         			"Environmental Protection",
                         			"Health",
                         			"Industry, Mining and Construct.",
                         			"Other Development Aid",
                         			"Other Social Infrastructure and Services",
                         			"Water Sanitation"))
 
 
v+facet_grid(.~Lno_disasters, labeller = labeller(Lno_disasters = disast_labels ))
ggsave(
	file = paste0(pathGraphics, '/developmentAidValueByType.pdf'),
	width=6, height=4
	)
 
p <- ggplot(plotDataAgg, aes(x = LstratMuQuintile, y = prop, fill = aidType)) + 
			geom_area()+
			labs(x = 'Strategic Distance', y = 'Proportion of Aid Committments')+
			theme(legend.position="bottom")+ 
			guides(fill=guide_legend(nrow=3,byrow=TRUE, title.position = 'top'))+
			scale_fill_discrete(name="Type of Development Aid Committment",
                         labels=c("Debt Relief", 
                         			"Economic Infrastructure and Services", 
                         			"Education",
                         			"Environmental Protection",
                         			"Health",
                         			"Industry, Mining and Construct.",
                         			"Other Development Aid",
                         			"Other Social Infrastructure and Services",
                         			"Water Sanitation"))
p+facet_grid(.~Lno_disasters, labeller = labeller(Lno_disasters = disast_labels ) )
ggsave(
	file = paste0(pathGraphics, '/developmentAidValueByTypeProportion.pdf'),
	width=6, height=4
	)


##################################)
 
