if(Sys.info()['user']=='s7m' | Sys.info()['user']=='janus829'){ source('~/Research/ForeignAid/RCode/setup.R') }
if(Sys.info()['user']=='cindycheng'){ source('~/Documents/Papers/ForeignAid/RCode/setup.R') }
 
list.files(pathResults)


toLoad=list.files(pathResults)[grepl('LunIdPt_interaction|Ligo_interaction|LallyWt_interaction|LstratMu_interaction', list.files(pathResults))]
toLoad = toLoad[-grep('notHuman', toLoad)]
for(out in toLoad){ load(paste0(pathResults, '/', out)) ; assign(gsub('.rda','',out), mods) ; rm(list='mods') }

 
results = lapply(gsub('.rda', '', toLoad), function(x){
	rubinCoef(get(x))
})

resultsAIC = lapply(gsub('.rda', '', toLoad), function(x){
	 lapply(get(x), function(y){
		AIC(y)
		}) %>% unlist() %>% mean() 
})



names(results) = gsub('.rda', '', toLoad)
names(resultsAIC) = gsub('.rda', '', toLoad)
resultsAIC