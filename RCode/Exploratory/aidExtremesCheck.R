# checks extreme values of foreign aid for gut checks
if(Sys.info()['user']=='cindycheng'){ source('~/Documents/Papers/ForeignAid/RCode/setup.R') }



load(paste0(pathResults, '/humanitarianTotal_fullSamp_gaussian_re_LstratMu_interaction.rda'))
load(paste0(pathData, '/iDataDisagg.rda'))

 
regData = iData[[1]]
head(panel)
regData$Sender = panel$CNTRY_NAME[match(regData$ccodeS,panel$ccode)]
regData$Receiver = panel$CNTRY_NAME[match(regData$ccodeR,panel$ccode)]

vars = c('Sender', 'Receiver', 'year', 'LstratMu', 'Lno_disasters', 'humanitarianTotal', 'civSocietyTotal', 'developTotal')


checkAidCommittments = function(noDisasters){
	check = regData[regData$LstratMu > 4 & regData$Lno_disasters == noDisasters, vars]

	print('strategic interest')
	print(tapply(check$LstratMu, check$Receiver, mean) %>% sort())

	print('Humanitarian Disbursements')
	print(tapply(check$humanitarianTotal, check$Receiver, mean) %>% sort())


	print('Civil Society Disbursements')
	print(tapply(check$civSocietyTotal, check$Receiver, mean) %>% sort())

	print('Development Disbursements')
	print(tapply(check$developTotal, check$Receiver, mean) %>% sort())

}


summary(regData$LstratMu)

checkAidCommittments(8)
checkAidCommittments(7)
checkAidCommittments(6)
checkAidCommittments(5)
checkAidCommittments(4)
checkAidCommittments(1)