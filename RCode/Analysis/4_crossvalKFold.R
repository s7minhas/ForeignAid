if(Sys.info()['user']=='s7m' | Sys.info()['user']=='janus829'){ source('~/Research/ForeignAid/RCode/setup.R') }
if(Sys.info()['user']=='cindycheng'){ source('~/Dropbox/Documents/Papers/ForeignAid1/RCode/setup.R') }
 
################################################################
# Load reg data
setwd(pathData)
load('iData.rda')
# Add dyad random effect
iData = lapply(iData, function(x){
  # add dyadic id
  x$id = paste(x$ccodeS, x$ccodeR, sep='_')
  x$id = factor(x$id)
  # log aid flow
  x$commitUSD13 = log(x$commitUSD13 + 1)
  return(x)
})
################################################################

################################################################
# RE model

## mod formula
cntrlVars=c(
  'colony' # Colonial variable
  ,'Lpolity2' # Institutions
  ,'LlnGdpCap' # Macroecon controls
  ,'LlifeExpect', 'Lno_disasters' # Humanitarian
  ,'Lcivwar' # Civil war
)

# model spec gen
genModelForm = function(var, type, struc){
  if(type=='re'){ strucChar=paste0(' + ', paste('(1|',struc, ')', collapse=' + ')) }
  if(type=='fe'){ strucChar=paste0(' + ', paste('factor(',struc,')',collapse=' + '), ' - 1') }
  if(type=='none'){ strucChar=NULL }
  form = formula(
    paste0(  'commitUSD13 ~ ',  # DV
             paste(var, collapse=' + '), ' + ', # add key var
             paste(cntrlVars, collapse=' + '), # add control vars
             strucChar )  )
  return(form)
}

 
# filename gen
genFileName = function(partitionLevel, mod, type, keyVar ){
  a = '10-Fold'
  b = partitionLevel
  c = mod ; d = type ; e = paste(keyVar, collapse='')
  f = paste0(paste(a,b,c,d,e, sep='_'), '.rda')
  return( gsub('__','_',f) )
}


# Run LOO models in parallel across imputed datasets
runModelParallel = function(
  cores=detectCores(), 
  dataList=iData, modPartitionLevel='ccodeS', 
  modType='re', modFamily='gaussian',
  keyRegVar='LstratMu', modStruc=c('id','year')
){
  
  modForm = genModelForm(var=keyRegVar, type=modType, struc=modStruc)
  modName = genFileName(partitionLevel = modPartitionLevel,  mod=modFamily, type=modType, keyVar=keyRegVar)
  
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
runModelParallel( modPartitionLevel='ccodeS', modType='re', keyRegVar='LstratMu')

# # LstratMu, cross validation --- 10-Fold crossval using ccodeR as the partition
runModelParallel( modPartitionLevel='ccodeR', modType='re', keyRegVar='LstratMu')

# # LstratMu, cross validation --- 10-Fold crossval using year as the partition
runModelParallel( modPartitionLevel='year', modType='re', keyRegVar='LstratMu')



## Ally
# # LallyWt, cross validation --- 10-Fold crossval using ccodeS as the partition
runModelParallel( modPartitionLevel='ccodeS', modType='re', keyRegVar='LallyWt')

# # LallyWt, cross validation --- 10-Fold crossval using ccodeR as the partition
runModelParallel( modPartitionLevel='ccodeR', modType='re', keyRegVar='LallyWt')

# # LallyWt, cross validation --- 10-Fold crossval using year as the partition
runModelParallel( modPartitionLevel='year', modType='re', keyRegVar='LallyWt')
 

### UN
# # LunIdPt, cross validation --- 10-Fold crossval using ccodeS as the partition
runModelParallel( modPartitionLevel='ccodeS', modType='re', keyRegVar='LunIdPt')

# # LunIdPt, cross validation --- 10-Fold crossval using ccodeS as the partition
runModelParallel( modPartitionLevel='ccodeR', modType='re', keyRegVar='LunIdPt')

# # LunIdPt, cross validation --- 10-Fold crossvalusing year as the partition
runModelParallel( modPartitionLevel='year', modType='re', keyRegVar='LunIdPt')
 

## IGO
 # # Ligo, cross validation --- 10-Fold crossval using ccodeS as the partition
runModelParallel( modPartitionLevel='ccodeS', modType='re', keyRegVar='Ligo')

 # # Ligo, cross validation --- 10-Fold crossval using ccodeS as the partition
runModelParallel( modPartitionLevel='ccodeR', modType='re', keyRegVar='Ligo')

# # Ligo, cross validation --- 10-Fold crossvalusing year as the partition
runModelParallel(modPartitionLevel='year', modType='re', keyRegVar='Ligo')


### ALL
# # LallyWt + LunIdPt + Ligo, cross validation --- 10-Fold crossval using ccodeS as the partition
runModelParallel(modPartitionLevel='ccodeS', modType='re', keyRegVar=c('LallyWt', 'LunIdPt','Ligo'))

# # LallyWt + LunIdPt + Ligo, cross validation --- 10-Fold crossval using ccodeS as the partition
runModelParallel(modPartitionLevel='ccodeR', modType='re', keyRegVar=c('LallyWt', 'LunIdPt','Ligo'))

# # LallyWt + LunIdPt + Ligo, cross validation --- 10-Fold crossval using year as the partition
runModelParallel(modPartitionLevel='year', modType='re', keyRegVar=c('LallyWt', 'LunIdPt','Ligo'))
 
