if(Sys.info()['user']=='s7m' | Sys.info()['user']=='janus829'){ source('~/Research/ForeignAid/RCode/setup.R') }
if(Sys.info()['user']=='cindycheng'){ source('~/Documents/Papers/ForeignAid/RCode/setup.R') }
################################################################

################################################################
# load data
load(paste0(pathData, '/noImputationData.rda'))
load(paste0(pathData, '/iData_v2.rda'))

ggplot(
	regData[regData$ccodeS==2 & regData$ccodeR==630,],
	aes(x=year, y=commitUSD13)) +
	geom_line()

countrycode('Cuba', 'country.name', 'cown')
countrycode('Iran', 'country.name', 'cown')

loadPkg('dplyr')
regData %>% 
	filter(ccodeS==230 & ccodeR==652) %>%
	select(year, commitUSD13, Lno_disasters) %>% data.frame()
################################################################

################################################################
fullSampPath = paste0(pathResults, '/fullSamp_gaussian_re_')
load(paste0(fullSampPath, 'LstratMu.rda')) ; stratMuMods = mods
load(paste0(fullSampPath, 'LstratMu_interaction.rda')) ; stratMuIntMods = mods ; rm(mods)


# in samp preds
mod = stratMuIntMods[[1]]
preds = predict(mod)

predDF = cbind(
	iData[[1]][,c('ccodeS','ccodeR','year','commitUSD13','Lno_disasters')],
	pred=predict(mod) )
predDF$dv = log(predDF$commitUSD13 + 1)
predDF$resid = abs(predDF$dv - predDF$pred )
predDF$dyad = paste0(predDF$ccodeS, '_', predDF$ccodeR)

predDF = predDF %>% group_by(dyad) %>% 
	mutate(mdv = mean(dv, na.rm=TRUE))

tmp = predDF %>% group_by(dyad) %>% 
	summarise(mresid = mean(resid)) %>% data.frame()
tmp = tmp[order(tmp$mresid),]

tmp$ccodeS = unlist(lapply(strsplit(tmp$dyad, '_'), function(x){x[1]}))
tmp$ccodeR = unlist(lapply(strsplit(tmp$dyad, '_'), function(x){x[2]}))

tmp$cnameS = panel$cname[match(tmp$ccodeS, panel$ccode)]
tmp$cnameR = panel$cname[match(tmp$ccodeR, panel$ccode)]
tmp$mean_dv = predDF$mdv[match(tmp$dyad, predDF$dyad)]

tmpSubset = tmp[tmp$mean_dv>=.1,]

tmpSubset[1:10,-1]
################################################################	