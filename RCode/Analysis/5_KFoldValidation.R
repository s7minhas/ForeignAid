if(Sys.info()['user']=='s7m' | Sys.info()['user']=='janus829'){
  source('~/Research/ForeignAid/RCode/setup.R') }
if(Sys.info()['user']=='cindycheng'){
  source('~/Documents/Papers/ForeignAid/RCode/setup.R') }
################################################################

################################################################
# load models
foldPath = paste0(pathResults, '/5-Fold_ccodeR_gaussian_re_')
load(paste0(foldPath, 'LallyDist.rda')) ; allyModsR = mods
load(paste0(foldPath, 'LigoDist.rda')) ; igoModsR = mods
load(paste0(foldPath, 'LunDist.rda')) ; unModsR = mods
load(paste0(foldPath, 'LstratMu.rda')) ; stratMuModsR = mods

foldPath = paste0(pathResults, '/5-Fold_ccodeS_gaussian_re_')
load(paste0(foldPath, 'LallyDist.rda')) ; allyModsS = mods
load(paste0(foldPath, 'LigoDist.rda')) ; igoModsS = mods
load(paste0(foldPath, 'LunDist.rda')) ; unModsS = mods
load(paste0(foldPath, 'LstratMu.rda')) ; stratMuModsS = mods

foldPath = paste0(pathResults, '/5-Fold_year_gaussian_re_')
load(paste0(foldPath, 'LallyDist.rda')) ; allyModsY = mods
load(paste0(foldPath, 'LigoDist.rda')) ; igoModsY = mods
load(paste0(foldPath, 'LunDist.rda')) ; unModsY = mods
load(paste0(foldPath, 'LstratMu.rda')) ; stratMuModsY = mods ; rm(list=c('mods','foldPath'))

# load data
load(paste0(pathData, '/iData_v2.rda'))
iData = lapply(iData, function(x){
  x$id = paste(x$ccodeS, x$ccodeR, sep='_') ; x$id = factor(x$id) # add dyadic id
  x$commitUSD13 = log(x$commitUSD13 + 1) # log aid flow
  return(x) })
################################################################

################################################################
# helper functions
xKout = function(
  cores=detectCores(), 
  dataList=iData, modPartitionLevel='ccodeS'){
  
  cl=makeCluster(cores) ; registerDoParallel(cl)
  fold = foreach(ii=1:length(dataList)) %:%
    foreach(k = 1:5, .packages=c( 'lme4')) %dopar% { # hardcode for 5-fold model -- can generalize later if need be
      
      regData = dataList[[ii]] # Subset to relevant data list
      regData$Partition = as.numeric(as.character(regData[, modPartitionLevel]))
      partitions = sort(levels(regData[, modPartitionLevel]))
      
      # randomly shuffle partition levels
      set.seed(2)
      partitions = partitions[sample(length(partitions))]
      
      # make k-folds
      folds = cut(seq(1, length(partitions)), breaks = 5, labels = F)
      
      
      # partition data by k-folds
      partitionIndex = which(folds == k)
      slice = regData[regData$Partition %in% partitions[partitionIndex],]
      
      return(slice)
    }
  stopCluster(cl)
 return(fold) } 

getRMSE_wRE = function(m, k, mod, data){
  rmseL = lapply(1:m, function(mm) {
    impMod = mod[[mm]]
    impData = data[[mm]]
      rmse0 = unlist(lapply(1:k, function(kk){
        fromfn = predict(object=impMod[[kk]], newdata=impData[[kk]], allow.new.levels=TRUE, re.form=~(1|id)+(1|year))
         c(fromfn - impData[[kk]]$commitUSD13)^2 %>% mean(.)
        }))
      rmse = sqrt(sum(rmse0)/k) })
  return(unlist(rmseL)) }
################################################################  
 
################################################################
# get dataset
xKout_ccodeS = xKout()
xKout_ccodeR = xKout(modPartitionLevel='ccodeR')
xKout_year = xKout(modPartitionLevel='year')

# rmse
folds_ccodeR = list( ally=allyModsR, igo=igoModsR,
  un=unModsR, stratMu=stratMuModsR )
folds_ccodeS = list( ally=allyModsS, igo=igoModsS,
  un=unModsS, stratMu=stSatMuModsS )
folds_year = list( ally=allyModsY, igo=igoModsY,
  un=unModsY, stratMu=stSatMuModsY )

#
rmse_ccodeR = lapply(folds_ccodeR, getRMSE_wRE)
rmse_ccodeS = lapply(folds_ccodeS, getRMSE_wRE)
rmse_year = lapply(folds_year, getRMSE_wRE)
################################################################