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
