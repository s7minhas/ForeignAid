################################################################

################################################################
# validate models
# http://stackoverflow.com/questions/20428742/select-first-element-of-nested-list-in-r


# Load model results
toLoad=list.files(pathResults)[grepl('10-Fold', list.files(pathResults))]
for(out in toLoad){ load(paste0(pathResults, '/', out)) ; assign(gsub('.rda','',out), mods) ; rm(list='mods') }

 
load(paste0(pathData, '/noImputationData.rda'))
regData$commitUSD13 = log(regData$commitUSD13 + 1)
xOut = regData[,c('commitUSD13', 
  'colony' ,'Lpolity2','LlnGdpCap','LlifeExpect', 'Lno_disasters','Lcivwar',
  'LstratMu', 'LallyWt', 'LunIdPt', 'Ligo',
  'ccodeS', 'cnameS', 'ccodeR', 'cnameR','year'
  )] 
xOut$id = paste(xOut$ccodeS, xOut$ccodeR, sep='_')
xOut$id = factor(xOut$id)
xOut$ccodeS = factor(xOut$ccodeS)
xOut$year = factor(xOut$year)
# check to make sure unimputed and imputed data are in the same order
# which(xOut$id != iData[[1]]$id)
 


################################################################
## lm models 

# Meld parameter estimates from each and calc out of sample perf
rubinCoefList = function(mod, k){ # k for k-fold

modCoefList = lapply(1:k, function(x){
 lapply(lapply(mod, '[[', x), function(x){
    beta = fixef(x)
    se = sqrt(diag(vcov(x)))
    return( cbind(beta, se) )
  })%>% do.call('rbind',.) 
  }) 

  modSummList = lapply(modCoefList, function(x){
    modSumm = mi.meld(q=matrix(x[,1],ncol=length(unique(rownames(x))), byrow=TRUE), 
    se=matrix(x[,2],ncol=length(unique(rownames(x))), byrow=TRUE), 
    byrow=TRUE) %>% lapply(., t) %>% do.call('cbind',.) %>% data.frame(.)
    names(modSumm) = c('beta', 'se')
    modSumm$t = modSumm$beta/modSumm$se
    modSumm$var = unique(rownames(x))
    return(modSumm)
})

 return(modSummList)
}

 
xPartitionList = function(data, k, modPartitionLevel){
  data$Partition = as.numeric(as.character(data[, modPartitionLevel]))
  partitions = sort(levels(data[, modPartitionLevel]))
        
  # randomly shuffle partition levels
  set.seed(2) # make sure this is the same seed as in runModelParallel()
  partitions = partitions[sample(length(partitions))]

 folds = cut(seq(1, length(partitions)), breaks = 10, labels = F)
           
 # partition data by k-folds
  xSliceList = foreach(K = 1:k) %do%{
    partitionIndex = which(folds == K)
    xSlice = data[data$Partition %in% partitions[partitionIndex],] %>% na.omit
    return(xSlice)
  }

  return(xSliceList)

}

getRMSE = function( coef, data){
  pred = t(coef$beta %*% t(cbind(1, data[,coef$var[-1]])))
  c(pred - data$commitUSD13)^2 %>% mean(.) %>% sqrt(.) 
}

getRMSE_KFold = function(rcoefList, xList){
 mapply(getRMSE, coef = rcoefList, data = xList)
}


mean(getRMSE_KFold(rcoefList = rubinCoefList(mod = `10-Fold_ccodeS_gaussian_re_LstratMu`, k = 10), xList =xPartitionList(xOut, 10, 'ccodeS' ) ))
mean(getRMSE_KFold(rcoefList = rubinCoefList(mod = `10-Fold_year_gaussian_re_LstratMu`, k = 10), xList =xPartitionList(xOut, 10, 'year' ) ))

mean(getRMSE_KFold(rcoefList = rubinCoefList(mod = `10-Fold_ccodeS_gaussian_re_LallyWt`, k = 10), xList =xPartitionList(xOut, 10, 'ccodeS' ) ))
mean(getRMSE_KFold(rcoefList = rubinCoefList(mod = `10-Fold_year_gaussian_re_LallyWt`, k = 10), xList =xPartitionList(xOut, 10, 'year' ) ))

mean(getRMSE_KFold(rcoefList = rubinCoefList(mod = `10-Fold_ccodeS_gaussian_re_Ligo`, k = 10), xList =xPartitionList(xOut, 10, 'ccodeS' ) ))
mean(getRMSE_KFold(rcoefList = rubinCoefList(mod = `10-Fold_year_gaussian_re_Ligo`, k = 10), xList =xPartitionList(xOut, 10, 'year' ) ))

mean(getRMSE_KFold(rcoefList = rubinCoefList(mod = `10-Fold_ccodeS_gaussian_re_LunIdPt`, k = 10), xList =xPartitionList(xOut, 10, 'ccodeS' ) ))
mean(getRMSE_KFold(rcoefList = rubinCoefList(mod = `10-Fold_year_gaussian_re_LunIdPt`, k = 10), xList =xPartitionList(xOut, 10, 'year' ) ))

mean(getRMSE_KFold(rcoefList = rubinCoefList(mod = `10-Fold_ccodeS_gaussian_re_LallyWtLunIdPtLigo`, k = 10), xList =xPartitionList(xOut, 10, 'ccodeS' ) ))
mean(getRMSE_KFold(rcoefList = rubinCoefList(mod = `10-Fold_year_gaussian_re_LallyWtLunIdPtLigo`, k = 10), xList =xPartitionList(xOut, 10, 'year' ) ))
  

################################################################
## random effects models


# create tests set for each imputed dataset
iPartitionList = function(idataList, k, modPartitionLevel){
  m = length(idataList)

  iPartition = lapply(1:m, function(x){
    idata = idataList[[x]]
    idata$Partition =as.numeric(as.character(idata[, modPartitionLevel]))
    partitions = sort(levels(idata[, modPartitionLevel]))

    # randomly shuffle partition levels
    set.seed(2) # make sure this is the same seed as in runModelParallel()
    partitions = partitions[sample(length(partitions))]

    folds = cut(seq(1, length(partitions)), breaks = 10, labels = F)
      

     # partition data by k-folds
    iSliceList = foreach(K = 1:k) %do%{
    partitionIndex = which(folds == K)
    iSlice = idata[idata$Partition %in% partitions[partitionIndex],] %>% na.omit
    return(iSlice)
  }

  return(iSliceList)})

return(iPartition)

}

# get predicted values
predictREList = function(k, modList, dataList){
  m = length(modList)
  mCheck = length(dataList)

  if ( m != mCheck){
    return(paste0('Number of models and number of datasets do not match'))}

  else{
   pImpFold = lapply(1:m, function(x){
      pFold = lapply(1:k, function(y){
        p =  predict(object = modList[[x]][[y]] , newdata = dataList[[x]][[y]], allow.new.levels=TRUE)
        return(p)})
      return(pFold) })
  return(pImpFold)}

} 

# then calculate the RMSE for each imputation-fold
getRMSE_wRE_KFold = function(k, predREList, dataList) {

# helper function
  getRMSE_wRE_List = function(K, pred, dv){
  (pred[[k]] - dv[[k]])^2 %>% mean(.) %>% sqrt(.)}

 # rearrange predicted values from a list of m imputations within which are a list of k cross folds to a list of k crossfolds within which are a list of m imputations
pList = lapply(1:k, function(x){
lapply(predREList, '[[', x)})

# rearrange partitioned imputed data from a list of m imputations within which are a list of k cross folds to a list of k crossfolds within which are a list of m imputations
dList = lapply(1:k, function(x){
lapply(dataList, '[[', x)})
m = length(dataList)

# extract DV
DVList = lapply(1:k, function(x){
  dSlice = dList[[x]]
  dvList = lapply(1:m, function(y){
    dv = dSlice[[y]]$commitUSD13
    })
  return(dvList)
  })

# extract RMSE
RmsePerImpPerFold = lapply(1:k, function(x){
  pFold = pList[[x]]
  DVFold = DVList[[x]]
  mapply(getRMSE_wRE_List, K = k, pred = pFold, dv = DVFold)
})

return(RmsePerImpPerFold)
 
}

######
# partition imputed data into test sets by ccode
iDataPartitionC = iPartitionList(iData, 10, 'ccodeS')
iDataPartitionCR = iPartitionList(iData, 10, 'ccodeR')
iDataPartitionY = iPartitionList(iData, 10, 'year')

# strat
(rmseStratCCode = predictREList(10, `10-Fold_ccodeS_gaussian_re_LstratMu`, iDataPartitionC) %>% getRMSE_wRE_KFold(10, ., iDataPartitionC)  %>%  lapply(., mean))
mean(unlist(rmseStratCCode)); var(unlist(rmseStratCCode))

(rmseStratCCodeR = predictREList(10, `10-Fold_ccodeR_gaussian_re_LstratMu`, iDataPartitionCR) %>% getRMSE_wRE_KFold(10, ., iDataPartitionCR)  %>%  lapply(., mean))
mean(unlist(rmseStratCCodeR)); var(unlist(rmseStratCCodeR))

(rmseStratYear = predictREList(10, `10-Fold_year_gaussian_re_LstratMu`, iDataPartitionY) %>% getRMSE_wRE_KFold(10, ., iDataPartitionY)  %>%  lapply(., mean))
mean(unlist(rmseStratYear)); var(unlist(rmseStratYear))



# ally
(rmseAllyCCode = predictREList(10, `10-Fold_ccodeS_gaussian_re_LallyWt`, iDataPartitionC) %>% getRMSE_wRE_KFold(10, ., iDataPartitionC)  %>%  lapply(., mean))
mean(unlist(rmseAllyCCode)); var(unlist(rmseAllyCCode))

(rmseAllyCCodeR = predictREList(10, `10-Fold_ccodeR_gaussian_re_LallyWt`, iDataPartitionCR) %>% getRMSE_wRE_KFold(10, ., iDataPartitionCR)  %>%  lapply(., mean))
mean(unlist(rmseAllyCCodeR)); var(unlist(rmseAllyCCodeR))


(rmseAllyYear = predictREList(10, `10-Fold_year_gaussian_re_LallyWt`, iDataPartitionY) %>% getRMSE_wRE_KFold(10, ., iDataPartitionY)  %>%  lapply(., mean))
mean(unlist(rmseAllyYear)); var(unlist(rmseAllyYear))

# igo
(rmseIGOCCode = predictREList(10, `10-Fold_ccodeS_gaussian_re_Ligo`, iDataPartitionC) %>% getRMSE_wRE_KFold(10, ., iDataPartitionC)  %>%  lapply(., mean))
mean(unlist(rmseIGOCCode)); var(unlist(rmseIGOCCode))

(rmseIGOCCodeR = predictREList(10, `10-Fold_ccodeR_gaussian_re_Ligo`, iDataPartitionCR) %>% getRMSE_wRE_KFold(10, ., iDataPartitionCR)  %>%  lapply(., mean))
mean(unlist(rmseIGOCCodeR)); var(unlist(rmseIGOCCodeR))

(rmseIGOYear = predictREList(10, `10-Fold_year_gaussian_re_Ligo`, iDataPartitionY) %>% getRMSE_wRE_KFold(10, ., iDataPartitionY)  %>%  lapply(., mean))
mean(unlist(rmseIGOYear)); var(unlist(rmseIGOYear))

# un
(rmseUNCCode = predictREList(10, `10-Fold_ccodeS_gaussian_re_LunIdPt`, iDataPartitionC) %>% getRMSE_wRE_KFold(10, ., iDataPartitionC)  %>%  lapply(., mean))
mean(unlist(rmseUNCCode)); var(unlist(rmseUNCCode))

(rmseUNCCodeR = predictREList(10, `10-Fold_ccodeR_gaussian_re_LunIdPt`, iDataPartitionCR) %>% getRMSE_wRE_KFold(10, ., iDataPartitionCR)  %>%  lapply(., mean))
mean(unlist(rmseUNCCodeR)); var(unlist(rmseUNCCodeR))

(rmseUNYear = predictREList(10, `10-Fold_year_gaussian_re_LunIdPt`, iDataPartitionY) %>% getRMSE_wRE_KFold(10, ., iDataPartitionY)  %>%  lapply(., mean))
mean(unlist(rmseUNYear)); var(unlist(rmseUNYear))

# all
(rmseALLCCode = predictREList(10, `10-Fold_ccodeS_gaussian_re_LallyWtLunIdPtLigo`, iDataPartitionC) %>% getRMSE_wRE_KFold(10, ., iDataPartitionC)  %>%  lapply(., mean))
mean(unlist(rmseALLCCode)); var(unlist(rmseALLCCode))

(rmseALLCCodeR = predictREList(10, `10-Fold_ccodeR_gaussian_re_LallyWtLunIdPtLigo`, iDataPartitionCR) %>% getRMSE_wRE_KFold(10, ., iDataPartitionCR)  %>%  lapply(., mean))
mean(unlist(rmseALLCCodeR)); var(unlist(rmseALLCCodeR))

(rmseALLYear = predictREList(10, `10-Fold_year_gaussian_re_LallyWtLunIdPtLigo`, iDataPartitionY) %>% getRMSE_wRE_KFold(10, ., iDataPartitionY)  %>%  lapply(., mean))
mean(unlist(rmseALLYear)); var(unlist(rmseALLYear))


lapply(lapply(list(rmseStratCCode, rmseAllyCCode,   rmseIGOCCode,  rmseUNCCode ,rmseALLCCode), function(x){unlist(x)}), function(x){ m = mean(x); v =var(x); return(c(m, v))})
lapply(lapply(list(rmseStratCCodeR, rmseAllyCCodeR, rmseIGOCCodeR,  rmseUNCCodeR,  rmseALLCCodeR), function(x){unlist(x)}), function(x){ m = mean(x); v =var(x); return(c(m, v))})
lapply(lapply(list(rmseStratYear, rmseAllyYear, rmseIGOYear,   rmseUNYear, rmseALLYear), function(x){unlist(x)}), function(x){ m = mean(x); v =var(x); return(c(m, v))})
 
 


 

################################################################

