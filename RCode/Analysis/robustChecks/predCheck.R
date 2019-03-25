if(Sys.info()['user']=='s7m' | Sys.info()['user']=='janus829'){
  source('~/Research/ForeignAid/RCode/setup.R') }
if(Sys.info()['user']=='cindycheng'){
  source('~/Documents/Papers/ForeignAid/RCode/setup.R') }

#
loadPkg(c('lme4'))
################################################################

################################################################
# add extra vars for reviewers
## the .rda below is created in latVarUncertainty.R
load( paste0(pathData, 'iData_for_r3.rda') )

# results similar across imputed datasets
iData = iData[[length(iData)]]
################################################################

################################################################
# specs
dvs = c(
  'humanitarianTotal',
  'developTotal',
  'civSocietyTotal'
  )
dvNames = paste0(
  c('Humanitarian', 'Development', 'Civil Society'), ' Aid')
baseSpec = paste(
  c(
    'LstratMu', 'Lno_disasters', 
    'LstratMu * Lno_disasters', 'colony', 
    'Lpolity2', 'LlnGdpCap', 'LlifeExpect',
    'Lcivwar'
  ), collapse=' + ' )
reStruc = '+ (1|id) + (1|year)'

# set up formulas
reModSpecs = lapply(dvs, function(y){
  formula(paste0(y, '~', baseSpec, reStruc)) })

# spec using ally variable
altSpec = paste(
  c(
    'LallyWt', 'Lno_disasters', 
    'LallyWt * Lno_disasters', 'colony', 
    'Lpolity2', 'LlnGdpCap', 'LlifeExpect',
    'Lcivwar'
  ), collapse=' + ' )
# set up formulas
reModSpecsAlt = lapply(dvs, function(y){
  formula(paste0(y, '~', altSpec, reStruc)) })

# combine specs
allSpecs = unlist(list(reModSpecs, reModSpecsAlt))
specNames = c(
  paste0('orig_', dvs),
  paste0('alt_', dvs)
  )
################################################################

################################################################
# run models
# if(!file.exists(
#   paste0(pathResults, '/reMods_latVarUncert.rda')
#   )){

# out of sample set up for comparison
nFolds = 40
iData$fold = sample(1:nFolds, nrow(iData), replace=TRUE)
rmseStats = matrix(NA,nrow=nFolds,ncol=length(allSpecs))

cl=makeCluster(8) ; registerDoParallel(cl)

rmseStats = foreach(f=1:nFolds, .packages=c('lme4')) %dopar% {
  # divide into train and test
  train = iData[iData$fold != f,]
  test = iData[iData$fold == f,]

  # run train models
  mods = lapply(allSpecs, function(spec){
    return(lmer(spec, data=iData)) }) 
  names(mods) = specNames

  # gen predictions in test
  rmseFold = lapply(1:length(mods), function(i){
    modSpec = names(mods)[i]
    dv = unlist(lapply(strsplit(modSpec, '_'),function(x){x[2]}))
    m = mods[[modSpec]]
    preds = predict(m, newdata=test)
    obs = test[,dv]
    rmse = sqrt( mean( (preds-obs)^2 ) )
    return(rmse) })
  rmseFold = unlist(rmseFold)
  names(rmseFold) = specNames

  # store
  return(rmseFold) }

#
stopCluster(cl)

# 
rmseStats = do.call('rbind', rmseStats)
apply(rmseStats, 2, mean)

  ## humanitarian model
  humModRE= lmer(reModSpecs[[1]], data=iData)
  humRMSE = sqrt(mean( (predict(humModRE)-iData$humanitarianTotal)^2 ))
  humAltModRE= lmer(reModSpecsAlt[[1]], data=iData)
  humAltRMSE = sqrt(mean( (predict(humAltModRE)-iData$humanitarianTotal)^2 ))

  ## dev model
  devModRE = lmer(reModSpecs[[2]], data=iData)
  devRMSE = sqrt(mean( (predict(devModRE)-iData$developTotal)^2 ))
  devAltModRE = lmer(reModSpecsAlt[[2]], data=iData)
  devAltRMSE = sqrt(mean( (predict(devAltModRE)-iData$developTotal)^2 ))

  ## civil society model
  civModRE = lmer(reModSpecs[[3]], data=iData)
  civRMSE = sqrt(mean( (predict(civModRE)-iData$civSocietyTotal)^2 ))
  civAltModRE = lmer(reModSpecsAlt[[3]], data=iData)
  civAltRMSE = sqrt(mean( (predict(civAltModRE)-iData$civSocietyTotal)^2 ))

  # org rmse
  rmseMat = matrix(NA,nrow=3,ncol=2,dimnames=list(dvs, c('Original','Alternative')))
  rmseMat[,1] = c(humRMSE,devRMSE,civRMSE)
  rmseMat[,2] = c(humAltRMSE,devAltRMSE,civAltRMSE)
  rmseMat

  # save(reMods, 
  #   file=paste0(pathResults, '/reMods_latVarUncert.rda')
  #   )
# } else {
#   load(paste0(pathResults, '/reMods_latVarUncert.rda'))
# }
################################################################