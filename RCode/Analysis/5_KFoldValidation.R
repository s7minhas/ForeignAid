if(Sys.info()['user']=='s7m' | Sys.info()['user']=='janus829'){ source('~/Research/ForeignAid/RCode/setup.R') }
if(Sys.info()['user']=='cindycheng'){ source('~/Documents/Papers/ForeignAid/RCode/setup.R') }
################################################################

################################################################
# validate k-fold models

# Load model results
toLoad=list.files(pathResults)[grepl('5-Fold', list.files(pathResults))]

# load model
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
load(paste0(foldPath, 'LstratMu.rda')) ; stratMuModsY = mods

# Load imputed data
load(paste0(pathData, '/iData.rda'))
# Add dyad random effect
iData = lapply(iData, function(x){
  # add dyadic id
  x$id = paste(x$ccodeS, x$ccodeR, sep='_') ; x$id = factor(x$id)
  # log aid flow
  x$commitUSD13 = log(x$commitUSD13 + 1) ; return(x) })

# load unimputed data
load(paste0(pathData, '/noImputationData.rda'))
regData$commitUSD13 = log(regData$commitUSD13 + 1)
xOut = regData[,c('commitUSD13', 
  'colony' ,'Lpolity2','LlnGdpCap','LlifeExpect', 'Lno_disasters','Lcivwar',
  'LstratMu', 'LallyWt', 'LunIdPt', 'Ligo',
  'ccodeS', 'cnameS', 'ccodeR', 'cnameR','year' )]
xOut$id = paste(xOut$ccodeS, xOut$ccodeR, sep='_')
xOut$id = factor(xOut$id); xOut$ccodeS = factor(xOut$ccodeS)
xOut$year = factor(xOut$year); xOut$ccodeR = factor(xOut$ccodeR)

## check to make sure unimputed and imputed data are in the same order
# which(xOut$id != iData[[1]]$id)
############################################################

############################################################

# create tests set for imputed datasets or non-imputed datasets
PartitionList = function(dataList, k, modPartitionLevel){

  if (class(dataList) == 'list'){
     m = length(dataList)}
 
  else if (class(dataList) == 'data.frame'){
     m = 1}

  pList = lapply(1:m, function(x){

    if (class(dataList) == 'list'){
    data = dataList[[x]]}

    else if (class(dataList) == 'data.frame'){
      data = dataList}

    data$Partition =as.numeric(as.character(data[, modPartitionLevel]))
    partitions = sort(levels(data[, modPartitionLevel]))

    # randomly shuffle partition levels
    set.seed(2) # make sure this is the same seed as in runModelParallel()
    partitions = partitions[sample(length(partitions))]

    folds = cut(seq(1, length(partitions)), breaks = 10, labels = F)
      
     # partition data by k-folds
    iSliceList = foreach(K = 1:k) %do%{
    partitionIndex = which(folds == K)
    iSlice = data[data$Partition %in% partitions[partitionIndex],] %>% na.omit
    return(iSlice)
  }

  return(iSliceList)})

  if (class(dataList) == 'list'){
    return(pList)}
  
  else if (class(dataList) == 'data.frame'){
    return(unlist(pList, recursive=FALSE))
  }
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



# calculate the RMSE for each imputation-fold
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


################################################################

#################################################################
## evaluate performance using imputed data

# partition imputed data into test sets by ccode
iDataPartitionC = PartitionList(iData, 5, 'ccodeS')
iDataPartitionCR = PartitionList(iData, 5, 'ccodeR')
iDataPartitionY = PartitionList(iData, 5, 'year')

# strat
rmseStratCCodeS = predictREList(5, stratMuModsS, iDataPartitionC) %>% getRMSE_wRE_KFold(5, ., iDataPartitionC)  %>%  lapply(., mean)
rmseStratCCodeR = predictREList(5, stratMuModsR, iDataPartitionCR) %>% getRMSE_wRE_KFold(5, ., iDataPartitionCR)  %>%  lapply(., mean)
rmseStratYear = predictREList(5, stratMuModsY, iDataPartitionY) %>% getRMSE_wRE_KFold(5, ., iDataPartitionY)  %>%  lapply(., mean)

# ally
rmseAllyCCodeS = predictREList(5, allyModsS, iDataPartitionC) %>% getRMSE_wRE_KFold(5, ., iDataPartitionC)  %>%  lapply(., mean)
rmseAllyCCodeR = predictREList(5, allyModsR, iDataPartitionCR) %>% getRMSE_wRE_KFold(5, ., iDataPartitionCR)  %>%  lapply(., mean)
rmseAllyYear = predictREList(5, allyModsY, iDataPartitionY) %>% getRMSE_wRE_KFold(5, ., iDataPartitionY)  %>%  lapply(., mean)
 
# igo
rmseIGOCCodeS = predictREList(5, igoModsS, iDataPartitionC) %>% getRMSE_wRE_KFold(5, ., iDataPartitionC)  %>%  lapply(., mean)
rmseIGOCCodeR = predictREList(5, igoModsR, iDataPartitionCR) %>% getRMSE_wRE_KFold(5, ., iDataPartitionCR)  %>%  lapply(., mean)
rmseIGOYear = predictREList(5, igoModsY, iDataPartitionY) %>% getRMSE_wRE_KFold(5, ., iDataPartitionY)  %>%  lapply(., mean)
 
# un
rmseUNCCode = predictREList(5, unModsS, iDataPartitionC) %>% getRMSE_wRE_KFold(5, ., iDataPartitionC)  %>%  lapply(., mean)
rmseUNCCodeR = predictREList(5, unModsR, iDataPartitionCR) %>% getRMSE_wRE_KFold(5, ., iDataPartitionCR)  %>%  lapply(., mean)
rmseUNYear = predictREList(5, unModsY, iDataPartitionY) %>% getRMSE_wRE_KFold(5, ., iDataPartitionY)  %>%  lapply(., mean)
 
# # all
# rmseALLCCode = predictREList(5, `10-Fold_ccodeS_gaussian_re_LallyWtLunIdPtLigo`, iDataPartitionC) %>% getRMSE_wRE_KFold(5, ., iDataPartitionC)  %>%  lapply(., mean)
# rmseALLCCodeR = predictREList(5, `10-Fold_ccodeR_gaussian_re_LallyWtLunIdPtLigo`, iDataPartitionCR) %>% getRMSE_wRE_KFold(5, ., iDataPartitionCR)  %>%  lapply(., mean)
# rmseALLYear = predictREList(5, `10-Fold_year_gaussian_re_LallyWtLunIdPtLigo`, iDataPartitionY) %>% getRMSE_wRE_KFold(5, ., iDataPartitionY)  %>%  lapply(., mean)
 

#### plot rmse by different ivs for ccodeS folds
rmseCcode =  data.frame(rmse = unlist(lapply(list(Strat = rmseStratCCode, Ally = rmseAllyCCode,   IGO= rmseIGOCCode,  UN= rmseUNCCode ,'Ally + IGO + UN' = rmseALLCCode), function(x){unlist(x)})))
rmseCcode$variable = gsub('\\d', '', rownames(rmseCcode))
 
rmseCcodeSummary = summarySE(rmseCcode, measurevar = 'rmse', groupvars = c('variable'))
rmseCcodeSummary$id = factor(rmseCcodeSummary$variable, levels = levels(factor(rmseCcodeSummary$variable))[c(4, 1,3,5, 2)])
rmseCcodeSummary$variable = factor(rmseCcodeSummary$variable, levels = levels(factor(rmseCcodeSummary$variable))[c(4, 1,3,5, 2)])
 
pdf(paste0(setwd(pathGraphics), '/rmsePlotCcodeS.pdf'))
rC = ggplot(rmseCcodeSummary, aes (x = id, y = rmse, ci ,colour = variable))
rC + geom_errorbar(aes(ymin = rmse - ci, ymax = rmse + ci), width = 0.2)+ geom_point()
dev.off()


#### plot rmse by different ivs for ccodeR folds
rmseCCodeR =  data.frame(rmse = unlist(lapply(list(Strat= rmseStratCCodeR, Ally = rmseAllyCCodeR,   IGO = rmseIGOCCodeR,  UN = rmseUNCCodeR ,'Ally + IGO + UN' = rmseALLCCodeR), function(x){unlist(x)})))
rmseCCodeR$variable = gsub('\\d', '', rownames(rmseCCodeR))
 
rmseCCodeRSummary = summarySE(rmseCCodeR, measurevar = 'rmse', groupvars = c('variable'))
rmseCCodeRSummary$id = factor(rmseCCodeRSummary$variable, levels = levels(factor(rmseCCodeRSummary$variable))[c(4, 1,3,5, 2)])
rmseCCodeRSummary$variable = factor(rmseCCodeRSummary$variable, levels = levels(factor(rmseCCodeRSummary$variable))[c(4, 1,3,5, 2)])
 
pdf(paste0(setwd(pathGraphics), '/rmsePlotCcodeR.pdf'))
rCR = ggplot(rmseCCodeRSummary, aes (x = id, y = rmse, ci ,colour = variable))
rCR + geom_errorbar(aes(ymin = rmse - ci, ymax = rmse + ci), width = 0.2)+ geom_point()
dev.off()


#### plot rmse by different ivs for year folds
rmseYear =  data.frame(rmse = unlist(lapply(list(Strat = rmseStratYear, Ally = rmseAllyYear,   IGO = rmseIGOYear,  UN = rmseUNYear ,'Ally + IGO + UN' = rmseALLYear), function(x){unlist(x)})))
rmseYear$variable = gsub('\\d|rmse|Year', '', rownames(rmseYear))

rmseYearSummary = summarySE(rmseYear, measurevar = 'rmse', groupvars = c('variable'))
rmseYearSummary$id = factor(rmseYearSummary$variable, levels = levels(factor(rmseYearSummary$variable))[c(4, 1,3,5, 2)])
rmseYearSummary$variable = factor(rmseYearSummary$variable, levels = levels(factor(rmseYearSummary$variable))[c(4, 1,3,5, 2)])

pdf(paste0(setwd(pathGraphics), '/rmsePlotYear.pdf'))
rY= ggplot(rmseYearSummary, aes (x = id, y = rmse, ci ,colour = variable))
rY + geom_errorbar(aes(ymin = rmse - ci, ymax = rmse + ci), width = 0.2)+ geom_point()
dev.off()


################################################################

#################################################################

## evaluate performance using non-imputed data
xPartitionC= rep(list(PartitionList(xOut, 10, 'ccodeS' )), 5)
xPartitionCR= rep(list(PartitionList(xOut, 10, 'ccodeR')), 5)
xPartitionY= rep(list(PartitionList(xOut, 10, 'year' )), 5)


# strat
rmseStratCCodeX = predictREList(5, `10-Fold_ccodeS_gaussian_re_LstratMu`, xPartitionC) %>% getRMSE_wRE_KFold(5, ., xPartitionC)  %>%  lapply(., mean)
rmseStratCCodeRX = predictREList(5, `10-Fold_ccodeR_gaussian_re_LstratMu`, xPartitionCR) %>% getRMSE_wRE_KFold(5, ., xPartitionCR)  %>%  lapply(., mean)
rmseStratYearX = predictREList(5, `10-Fold_year_gaussian_re_LstratMu`, xPartitionY) %>% getRMSE_wRE_KFold(5, ., xPartitionY)  %>%  lapply(., mean)

# ally
rmseAllyCCodeX = predictREList(5, `10-Fold_ccodeS_gaussian_re_LallyWt`, xPartitionC) %>% getRMSE_wRE_KFold(5, ., xPartitionC)  %>%  lapply(., mean)
rmseAllyCCodeRX = predictREList(5, `10-Fold_ccodeR_gaussian_re_LallyWt`, xPartitionCR) %>% getRMSE_wRE_KFold(5, ., xPartitionCR)  %>%  lapply(., mean)
rmseAllyYearX = predictREList(5, `10-Fold_year_gaussian_re_LallyWt`, xPartitionY) %>% getRMSE_wRE_KFold(5, ., xPartitionY)  %>%  lapply(., mean)

# igo
rmseIGOCCodeX = predictREList(5, `10-Fold_ccodeS_gaussian_re_Ligo`, xPartitionC) %>% getRMSE_wRE_KFold(5, ., xPartitionC)  %>%  lapply(., mean)
rmseIGOCCodeRX = predictREList(5, `10-Fold_ccodeR_gaussian_re_Ligo`, xPartitionCR) %>% getRMSE_wRE_KFold(5, ., xPartitionCR)  %>%  lapply(., mean)
rmseIGOYearX = predictREList(5, `10-Fold_year_gaussian_re_Ligo`, xPartitionY) %>% getRMSE_wRE_KFold(5, ., xPartitionY)  %>%  lapply(., mean)
 
# un
rmseUNCCodeX = predictREList(5, `10-Fold_ccodeS_gaussian_re_LunIdPt`, xPartitionC) %>% getRMSE_wRE_KFold(5, ., xPartitionC)  %>%  lapply(., mean)
rmseUNCCodeRX = predictREList(5, `10-Fold_ccodeR_gaussian_re_LunIdPt`, xPartitionCR) %>% getRMSE_wRE_KFold(5, ., xPartitionCR)  %>%  lapply(., mean)
rmseUNYearX = predictREList(5, `10-Fold_year_gaussian_re_LunIdPt`, xPartitionY) %>% getRMSE_wRE_KFold(5, ., xPartitionY)  %>%  lapply(., mean)
 
# all
rmseALLCCodeX = predictREList(5, `10-Fold_ccodeS_gaussian_re_LallyWtLunIdPtLigo`, xPartitionC) %>% getRMSE_wRE_KFold(5, ., xPartitionC)  %>%  lapply(., mean)
rmseALLCCodeRX = predictREList(5, `10-Fold_ccodeR_gaussian_re_LallyWtLunIdPtLigo`, xPartitionCR) %>% getRMSE_wRE_KFold(5, ., xPartitionCR)  %>%  lapply(., mean)
rmseALLYearX = predictREList(5, `10-Fold_year_gaussian_re_LallyWtLunIdPtLigo`, xPartitionY) %>% getRMSE_wRE_KFold(5, ., xPartitionY)  %>%  lapply(., mean)
 

#### plot rmse by different ivs for ccodeS folds
rmseCcodeX =  data.frame(rmse = unlist(lapply(list(Strat = rmseStratCCodeX, 
                              Ally = rmseAllyCCodeX,   
                              IGO = rmseIGOCCodeX,  
                              UN= rmseUNCCodeX ,
                             'Ally + IGO + UN' = rmseALLCCodeX), function(x){unlist(x)})))
 
rmseCcodeX$variable = gsub('\\d', '', rownames(rmseCcodeX))
rmseCcodeXSummary = summarySE(rmseCcodeX, measurevar = 'rmse', groupvars = c('variable'))
rmseCcodeXSummary$id = factor(rmseCcodeXSummary$variable, levels = levels(factor(rmseCcodeXSummary$variable))[c(4, 1,3,5, 2)])
rmseCcodeXSummary$variable = factor(rmseCcodeXSummary$variable, levels = levels(factor(rmseCcodeXSummary$variable))[c(4, 1,3,5, 2)])
 
pdf(paste0(setwd(pathGraphics), '/rmsePlotCcodeSnoImpute.pdf'))
rCx = ggplot(rmseCcodeXSummary, aes (x = id, y = rmse, ci ,colour = variable))
rCx + geom_errorbar(aes(ymin = rmse - ci, ymax = rmse + ci), width = 0.2)+ geom_point()
dev.off()


#### plot rmse by different ivs for ccodeR folds
rmseCCodeRX =  data.frame(rmse = unlist(lapply(list(Strat = rmseStratCCodeX, 
                              Ally = rmseAllyCCodeRX,   
                              IGO = rmseIGOCCodeRX,  
                              UN= rmseUNCCodeRX ,
                             'Ally + IGO + UN' = rmseALLCCodeRX), function(x){unlist(x)})))
rmseCCodeRX$variable = gsub('\\d', '', rownames(rmseCCodeRX))
 
rmseCCodeRXSummary = summarySE(rmseCCodeRX, measurevar = 'rmse', groupvars = c('variable'))
rmseCCodeRXSummary$id = factor(rmseCCodeRXSummary$variable, levels = levels(factor(rmseCCodeRXSummary$variable))[c(4, 1,3,5, 2)])
rmseCCodeRXSummary$variable = factor(rmseCCodeRXSummary$variable, levels = levels(factor(rmseCCodeRXSummary$variable))[c(4, 1,3,5, 2)])
 
 
pdf(paste0(setwd(pathGraphics), '/rmsePlotCcodeRnoImpute.pdf'))
rCRx = ggplot(rmseCCodeRXSummary, aes (x = id, y = rmse, ci ,colour = variable))
rCRx + geom_errorbar(aes(ymin = rmse - ci, ymax = rmse + ci), width = 0.2)+ geom_point()
dev.off()


#### plot rmse by different ivs for year folds
rmseYearX =  data.frame(rmse = unlist(lapply(list(Strat = rmseStratYearX, 
                                                Ally = rmseAllyYearX,   
                                                IGO= rmseIGOYearX,  
                                                UN= rmseUNYearX ,
                                                'Ally + IGO + UN' = rmseALLYearX), function(x){unlist(x)})))
rmseYearX$variable = gsub('\\d', '', rownames(rmseYearX))
 
rmseYearXSummary = summarySE(rmseYearX, measurevar = 'rmse', groupvars = c('variable'))
rmseYearXSummary$id = factor(rmseYearXSummary$variable, levels = levels(factor(rmseYearXSummary$variable))[c(4, 1,3,5, 2)])
rmseYearXSummary$variable = factor(rmseYearXSummary$variable, levels = levels(factor(rmseYearXSummary$variable))[c(4, 1,3,5, 2)])

pdf(paste0(setwd(pathGraphics), '/rmsePlotYearNoImpute.pdf'))
rYx= ggplot(rmseYearXSummary, aes (x = id, y = rmse, ci ,colour = variable))
rYx + geom_errorbar(aes(ymin = rmse - ci, ymax = rmse + ci), width = 0.2)+ geom_point()
dev.off()


# ################################################################
# ## lm models 

# # Meld parameter estimates from each and calc out of sample perf
# rubinCoefList = function(mod, k){ # k for k-fold

# modCoefList = lapply(1:k, function(x){
#  lapply(lapply(mod, '[[', x), function(x){
#     beta = fixef(x)
#     se = sqrt(diag(vcov(x)))
#     return( cbind(beta, se) )
#   })%>% do.call('rbind',.) 
#   }) 

#   modSummList = lapply(modCoefList, function(x){
#     modSumm = mi.meld(q=matrix(x[,1],ncol=length(unique(rownames(x))), byrow=TRUE), 
#     se=matrix(x[,2],ncol=length(unique(rownames(x))), byrow=TRUE), 
#     byrow=TRUE) %>% lapply(., t) %>% do.call('cbind',.) %>% data.frame(.)
#     names(modSumm) = c('beta', 'se')
#     modSumm$t = modSumm$beta/modSumm$se
#     modSumm$var = unique(rownames(x))
#     return(modSumm)
# })

#  return(modSummList)
# }
 
# getRMSE = function( coef, data){
#   pred = t(coef$beta %*% t(cbind(1, data[,coef$var[-1]])))
#   c(pred - data$commitUSD13)^2 %>% mean(.) %>% sqrt(.) 
# }

# getRMSE_KFold = function(rcoefList, xList){
#  mapply(getRMSE, coef = rcoefList, data = xList)
# }

# mean(getRMSE_KFold(rcoefList = rubinCoefList(mod = `10-Fold_ccodeS_gaussian_re_LstratMu`, k = 10), xList =PartitionList(xOut, 10, 'ccodeS' ) ))
# mean(getRMSE_KFold(rcoefList = rubinCoefList(mod = `10-Fold_year_gaussian_re_LstratMu`, k = 10), xList =PartitionList(xOut, 10, 'year' ) ))

# mean(getRMSE_KFold(rcoefList = rubinCoefList(mod = `10-Fold_ccodeS_gaussian_re_LallyWt`, k = 10), xList =PartitionList(xOut, 10, 'ccodeS' ) ))
# mean(getRMSE_KFold(rcoefList = rubinCoefList(mod = `10-Fold_year_gaussian_re_LallyWt`, k = 10), xList =PartitionList(xOut, 10, 'year' ) ))

# mean(getRMSE_KFold(rcoefList = rubinCoefList(mod = `10-Fold_ccodeS_gaussian_re_Ligo`, k = 10), xList =PartitionList(xOut, 10, 'ccodeS' ) ))
# mean(getRMSE_KFold(rcoefList = rubinCoefList(mod = `10-Fold_year_gaussian_re_Ligo`, k = 10), xList =PartitionList(xOut, 10, 'year' ) ))

# mean(getRMSE_KFold(rcoefList = rubinCoefList(mod = `10-Fold_ccodeS_gaussian_re_LunIdPt`, k = 10), xList =PartitionList(xOut, 10, 'ccodeS' ) ))
# mean(getRMSE_KFold(rcoefList = rubinCoefList(mod = `10-Fold_year_gaussian_re_LunIdPt`, k = 10), xList =PartitionList(xOut, 10, 'year' ) ))

# mean(getRMSE_KFold(rcoefList = rubinCoefList(mod = `10-Fold_ccodeS_gaussian_re_LallyWtLunIdPtLigo`, k = 10), xList =PartitionList(xOut, 10, 'ccodeS' ) ))
# mean(getRMSE_KFold(rcoefList = rubinCoefList(mod = `10-Fold_year_gaussian_re_LallyWtLunIdPtLigo`, k = 10), xList =PartitionList(xOut, 10, 'year' ) ))
  

# ################################################################
