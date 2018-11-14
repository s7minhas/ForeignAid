if(Sys.info()['user']=='s7m' | Sys.info()['user']=='janus829'){ source('~/Research/ForeignAid/RCode/setup.R') }
if(Sys.info()['user']=='cindycheng'){ source('~/Documents/Papers/ForeignAid/RCode/setup.R') }
 
################################################################
# Load reg data
setwd(pathData)
load('iDataDisagg.rda')
load(paste0(pathData, '/components/EUgene.rda'))
load(paste0(pathData, '/Components/VoetenData/un.rda'))
 
library(stringr)
library(dplyr)
library(wesanderson)


dvs = c('humanitarianTotal', 'developTotal', 'civSocietyTotal', 'notHumanitarianTotal')
ids = names(iData[[1]])[c(1:3,25)]
ivs = names(iData[[1]])[9:24]


iData = lapply(iData, function(x){
  # add in s-scores
  x$s_un_glo = data$s_un_glo[match(paste0(x$ccodeS, x$ccodeR), paste0(data$ccode1, data$ccode2))]
  x$s_wt_glo = data$s_un_glo[match(paste0(x$ccodeS, x$ccodeR), paste0(data$ccode1, data$ccode2))]

  # add in un data
  x$agree3un = unDataFINAL$agree3un[match(paste0(x$ccodeS, x$ccodeR), paste0(unDataFINAL$ccode_1, unDataFINAL$ccode_2))]

  x$id = paste0(x$ccodeS, 999, x$ccodeR)
  x$idYr = paste0(x$ccodeS, 9999, x$ccodeR, x$year)
  x = lagData(x, 'idYr', 'id', c('s_un_glo', 's_wt_glo', 'agree3un'))
 
 
  #add dyadic id
  x$id = paste(x$ccodeS, x$ccodeR, sep='_')
  x$id = factor(x$id)
  # create total aid
  x$aidTotal = x$notHumanitarianTotal + x$humanitarianTotal
  
  # log dvs
  for(dv in c('aidTotal',dvs)){ x[,dv] = log(x[,dv] + 1) }
  return(x)
   })



################################################################

################################################################
# RE model

## mod formula
disVar = 'Lno_disasters'
cntrlVars=c(
  'colony' # Colonial variable
  ,'Lpolity2' # Institutions
  ,'LlnGdpCap' # Macroecon controls
  ,'LlifeExpect' # Humanitarian
  ,'Lcivwar' # Civil war,
  ,disVar
  )



################################################################

################################################################
# RE model

# model spec gen
genModelForm = function(var, type, struc, dv){
  if(type=='re'){ strucChar=paste0(' + ', paste('(1|',struc, ')', collapse=' + ')) }
  if(type=='fe'){ strucChar=paste0(' + ', paste('factor(',struc,')',collapse=' + '), ' - 1') }
  if(type=='none'){ strucChar=NULL }
  form = formula(
    paste0(  dv, ' ~ ',  # DV
             paste(var, collapse=' + '), ' + ', # add key var
             paste(cntrlVars, collapse=' + '), # add control vars
             strucChar )  )
  return(form)
}

 
# filename gen
genFileName = function(partitionLevel, mod, type, keyVar, dv){
  a = '10-Fold'
  b = partitionLevel
  c = mod ; d = type ; e = paste(keyVar, collapse='')
  f = dv
  g = paste0(paste(a,b,c,d,e,f ,sep='_'), '.rda')
  return( gsub('__','_',g) )
}


# Run k (10) cross val models in parallel across imputed datasets
runModelParallel = function(
  cores=detectCores(), 
  dataList=iData, modPartitionLevel='ccodeS', 
  modType='re', modFamily='gaussian',
  keyRegVar='LstratMu', modStruc=c('id','year'),
  depVar = 'humanitarianTotal'
){
  
  modForm = genModelForm(var=keyRegVar, type=modType, struc=modStruc, dv = depVar)
  modName = genFileName(partitionLevel = modPartitionLevel,  mod=modFamily, type=modType, keyVar=keyRegVar, dv = depVar)
  
  print(paste0('Running model: ', Reduce(paste, deparse(modForm))))
  print(paste0('Saving to: ', modName))
  
  cl=makeCluster(cores) ; registerDoParallel(cl)
  mods = foreach(ii=1:length(dataList)) %:%
        foreach(k = 1:10, .packages=c( 'lme4')) %dopar% { # hardcode for 10-fold model -- can generalize later if need be
        
        regData = dataList[[ii]] # Subset to relevant data list
        regData$Partition = as.numeric(as.character(regData[, modPartitionLevel]))
        partitions = sort(levels(regData[, modPartitionLevel]))
        
        # randomly shuffle partition levels
        set.seed(2)
        partitions = partitions[sample(length(partitions))]

        # make k-folds
        folds = cut(seq(1, length(partitions)), breaks = 10, labels = F)
        
      
        # partition data by k-folds
        partitionIndex = which(folds == k)
        slice = regData[!regData$Partition %in% partitions[partitionIndex],]
        
        if(modType=='re'){
          m=lmer(modForm, data=slice)
        }
        
        if(modType=='fe'){
          stopifnot(modFamily=='gaussian')
          m=lm(modForm, data=slice)
        }
        
        return(m)   
  
      }
  stopCluster(cl)
  save(mods, file=paste0(pathResults, '/', modName)) # Save output	
}
 

################################################################

# ################################################################
# run 10-fold models


## Strat
# # LstratMu, cross validation --- 10-Fold crossval using ccodeS as the partition
partitions = c('ccodeS', 'ccodeR', 'year')
dvs = c('humanitarianTotal', 'developTotal', 'civSocietyTotal')
ivs = c('LstratMu', 'LallyWt', 'LunIdPt', 'Ligo', 'Lagree3un', 'Ls_un_glo', 'Ls_wt_glo')
 
for (p in partitions ){
  for (d in dvs){
    for ( i in ivs){
 runModelParallel( modPartitionLevel=p, 
                    depVar = d,
                     modType='re', 
                    keyRegVar=  paste0(i,  '*Lno_disasters') )}}}

  
################################################################

################################################################
# validate models
# http://stackoverflow.com/questions/20428742/select-first-element-of-nested-list-in-r

# Load model results
toLoad=list.files(pathResults)[grepl('10-Fold', list.files(pathResults))]
for(out in toLoad){ load(paste0(pathResults, '/', out)) ; assign(gsub('.rda','',out), mods) ; rm(list='mods') }

vars = c(dvs, ivs, cntrlVars,
    'ccodeS',   'ccodeR', 'year')
 
iDataOut = lapply(1:5, function(x){
            data = iData[[x]]
            data = data[, vars]
            data$`LallyWt:Lno_disasters` = data$LallyWt * data$Lno_disasters
            data$`LstratMu:Lno_disasters` = data$LstratMu * data$Lno_disasters
            data$`LunIdPt:Lno_disasters` = data$LunIdPt * data$Lno_disasters
            data$`Ligo:Lno_disasters` = data$Ligo * data$Lno_disasters
            data$`Lagree3un:Lno_disasters` = data$Lagree3un * data$Lno_disasters
            data$`Ls_un_glo:Lno_disasters` = data$Ls_un_glo * data$Lno_disasters
            data$`Ls_wt_glo:Lno_disasters` = data$Ls_wt_glo * data$Lno_disasters
           
            data$id = paste(data$ccodeS, data$ccodeR, sep='_')
            data$id = factor(data$id)
            data$ccodeS = factor(data$ccodeS)
            data$year = factor(data$year)
            return(data)
})


################################################################

################################################################



# get data
xPartitionList = function(dataList, k, modPartitionLevel){

  # partition each imputed data by k folds
  xSliceListCrossVal = lapply(1:length(dataList), function(x) {

  data = dataList[[x]]
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
  })

  # get the average for each fold for each imputed dataset
  xSliceListCrossValAvg = lapply(1:k, function(x){

    dataK = lapply(xSliceListCrossVal, '[[', x)
    dataKAvg = Reduce("+", lapply(dataK, function(y){y[, -which(names(y) %in% c('ccodeR', 'ccodeS', 'year', 'id'))]  }))/length(xSliceListCrossVal)
    dataId =  dataK[[1]][, c('ccodeR', 'ccodeS', 'year', 'id')]

    dataKAll = cbind(dataKAvg, dataId)

    return(dataKAll)
  })
  return( xSliceListCrossValAvg)
}


# get average test set partitions for each partition type
xListccodeR = xPartitionList(iDataOut, k = 10, 'ccodeR' )
xListccodeS = xPartitionList(iDataOut, k = 10, 'ccodeS' )
xListyear = xPartitionList(iDataOut, k = 10, 'year' )


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

# get rmse
getRMSE = function( coef, data, dv){
  pred = t(coef$beta %*% t(cbind(1, data[,coef$var[-1]])))
  c(pred - data[,dv])^2 %>% mean(.) %>% sqrt(.) 
}

# get rmse for each 
getRMSE_KFold = function(rcoefList, xList, depVar){
 mapply(getRMSE, coef = rcoefList, data = xList, dv = depVar)
}



# extract RMSES for all models
 
modelNames = gsub('.rda', '',toLoad)
modDv = sub(".*Lno_disasters_", "", modelNames)
modPartition0 = sub("10-Fold_", "", modelNames)
modPartition = sub("_gaussian.*", "", modPartition0)


rmses = list() 
for ( i in 1:length(modelNames)){
  print(i) 
  rmses[[i]] =   getRMSE_KFold(rcoefList = rubinCoefList(mod = get(modelNames[i]), k = 10), 
                     xList =get(paste0('xList', modPartition[i])),
                     depVar = modDv[i]
                      )

}

# put rmses into a data frame
names(rmses) = gsub('10-Fold_|gaussian_re_|\\*Lno_disasters', '', modelNames)
rmsesDf = unlist(rmses) %>% data.frame()
rmsesDf$mod = gsub('\\d', '',rownames(rmsesDf))
rmsesDf$mod = gsub('s_un_glo', 'sscoreUnWeighted', rmsesDf$mod)
rmsesDf$mod = gsub('s_wt_glo', 'sscoreWeighted', rmsesDf$mod)
  
rmsesDf = cbind(rmsesDf, data.frame(do.call(rbind, str_split(  rmsesDf$mod, '\\_'))))
rownames(rmsesDf) = NULL
names(rmsesDf) = c('rmse', 'mod', 'partition', 'iv', 'dv')

# clean up dv names
rmsesDf$dvName = factor(rmsesDf$dv) 
levels(rmsesDf$dvName) = c("Civil Society Aid", "Development Aid", "Humanitarian Aid")
rmsesDf$dvName = factor(rmsesDf$dvName , levels(rmsesDf$dvName)[c(3, 1,2)])

# clean up iv names
rmsesDf$ivName = factor(rmsesDf$iv) 
levels(rmsesDf$ivName)= c('Raw UN Votes', 'Alliances', 'IGO Membership', 'S-Score, Unweighted', 'S-Score Weighted', 'Strategic Interest', 'UN Ideal Point') 
rmsesDf$ivName = factor(rmsesDf$ivName , levels(rmsesDf$ivName)[c(6, 1, 2, 3, 7, 4, 5)])


save(rmsesDf, file = paste0(pathData, '/rmsesCrossVal.rda'))

# aggregate rmses by partition, iv and dv 
rmsesDfAgg = rmsesDf %>% dplyr:::group_by(partition, ivName, dvName) %>% dplyr:::summarise(mean = mean(rmse), sd = sd(rmse)) %>% data.frame()
rmsesDfAgg_ccodeS = rmsesDfAgg[grep('ccodeS', rmsesDfAgg$partition),]
rmsesDfAgg_ccodeR = rmsesDfAgg[grep('ccodeR', rmsesDfAgg$partition),] 
rmsesDfAgg_year = rmsesDfAgg[grep('year', rmsesDfAgg$partition),]
head(rmsesDfAgg)


################################################################

################################################################
dodge = position_dodge(0.9)
 
# make and save plots with 90 and 95% confidence intervals

pdf(paste0(pathGraphics,'/rmse_10FoldCrossVal_ccodeS.pdf'))
ggplot(rmsesDfAgg_ccodeS, aes(x = dvName , y = mean))+
  geom_errorbar( aes(color = ivName , ymin = mean-1.96*sd, ymax = mean+1.96*sd), position = dodge)+
 geom_errorbar( aes(color = ivName , ymin = mean-1.64*sd, ymax = mean+1.64*sd), position = dodge, alpha = .5)+
  geom_point(aes(color = ivName, y = mean), position = dodge)  +
  labs( y = 'Root Mean Squared Error',
        x= 'Dependent Variable')+ 
  guides(color=guide_legend(title="Independent Variable"))+
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 0.5),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(size = 16),
      legend.position = 'right' ,
      legend.text = element_text(size = 11),
        legend.title = element_text(size = 16)) 
  dev.off()

pdf(paste0(pathGraphics,'/rmse_10FoldCrossVal_ccodeR.pdf'))
ggplot(rmsesDfAgg_ccodeR, aes(x = dvName , y = mean))+
  geom_errorbar( aes(color = ivName , ymin = mean-1.96*sd, ymax = mean+1.96*sd), position = dodge)+
 geom_errorbar( aes(color = ivName , ymin = mean-1.64*sd, ymax = mean+1.64*sd), position = dodge, alpha = .5)+
  geom_point(aes(color = ivName, y = mean), position = dodge)  +
  labs(
     y = 'Root Mean Squared Error',
        x= 'Dependent Variable')+ 
  guides(color=guide_legend(title="Independent Variable"))+
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 0.5),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(size = 16),
      legend.position = 'right' ,
      legend.text = element_text(size = 11),
        legend.title = element_text(size = 16)) 

  dev.off()


pdf(paste0(pathGraphics,'/rmse_10FoldCrossVal_year.pdf'))
ggplot(rmsesDfAgg_year, aes(x = dvName , y = mean))+
  geom_errorbar( aes(color = ivName , ymin = mean-1.96*sd, ymax = mean+1.96*sd), position = dodge)+
 geom_errorbar( aes(color = ivName , ymin = mean-1.64*sd, ymax = mean+1.64*sd), position = dodge, alpha = .5)+
  geom_point(aes(color = ivName, y = mean), position = dodge)  +
  labs(
     y = 'Root Mean Squared Error',
        x= 'Dependent Variable')+ 
  guides(color=guide_legend(title="Independent Variable"))+
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 0.5),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(size = 16),
      legend.position = 'right' ,
      legend.text = element_text(size = 11),
        legend.title = element_text(size = 16)) 
  dev.off()
   
