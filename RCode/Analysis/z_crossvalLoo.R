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
genFileName = function(LooType, mod, type, keyVar ){
  a = 'LOO'
  b = LooType
  c = mod ; d = type ; e = paste(keyVar, collapse='')
  f = paste0(paste(a,b,c,d,e, sep='_'), '.rda')
  return( gsub('__','_',f) )
}


# Run LOO models in parallel across imputed datasets
runModelParallel = function(
  cores=detectCores(), 
  dataList=iData, modLooType='ccodeS', 
  modType='re', modFamily='gaussian',
  keyRegVar='LstratMu', modStruc=c('id','year')
){
  
  modForm = genModelForm(var=keyRegVar, type=modType, struc=modStruc)
  modName = genFileName(LooType = modLooType,  mod=modFamily, type=modType, keyVar=keyRegVar)
  
  print(paste0('Running model: ', Reduce(paste, deparse(modForm))))
  print(paste0('Saving to: ', modName))
  
  cl=makeCluster(cores) ; registerDoParallel(cl)
  mods = foreach(ii=1:length(dataList)) %:%
        foreach(loo = 1:length(levels(dataList[[1]][, modLooType])), .packages=c( 'lme4')) %dopar% {
    
        regData = dataList[[ii]] # Subset to relevant data list
    
        regData$LOO = as.numeric(as.character(regData[, modLooType]))
        partitions = sort(levels(regData[, modLooType]))
        
        slice = regData[regData$LOO != partitions[loo],]
        
        if(modType=='re'){
          m=lmer(modForm, data=slice)
        }
        
        if(modType=='fe'){
          stopifnot(modFamily=='gaussian')
          m=lm(modForm, data=slice)
        }
        
        return(m)
     
        return(slice)
      }
  stopCluster(cl)
  save(mods, file=paste0(pathResults, '/', modName)) # Save output	
}


################################################################

# ################################################################
# run models


# # LstratMu, cross validation --- Leave one out using ccodeS as the partition
runModelParallel( modLooType='ccodeS', modType='re', keyRegVar='LstratMu')

# # LstratMu, cross validation --- Leave one out using year as the partition
runModelParallel( modLooType='ccodeS', modType='re', keyRegVar='LstratMu')

# # LallyWt, cross validation --- Leave one out using ccodeS as the partition
runModelParallel( modLooType='ccodeS', modType='re', keyRegVar='LallyWt')

# # LallyWt, cross validation --- Leave one out using year as the partition
runModelParallel( modLooType='ccodeS', modType='re', keyRegVar='LallyWt')
 

# # LunIdPt, cross validation --- Leave one out using ccodeS as the partition
runModelParallel( modLooType='ccodeS', modType='re', keyRegVar='LunIdPt')

# # LunIdPt, cross validation --- Leave one out using year as the partition
runModelParallel( modLooType='ccodeS', modType='re', keyRegVar='LunIdPt')
 
 # # Ligo, cross validation --- Leave one out using ccodeS as the partition
runModelParallel( modLooType='ccodeS', modType='re', keyRegVar='Ligo')

# # Ligo, cross validation --- Leave one out using year as the partition
runModelParallel( modLooType='ccodeS', modType='re', keyRegVar='Ligot')
 

 # # LallyWt + LunIdPt + Ligo, cross validation --- Leave one out using ccodeS as the partition
runModelParallel( modLooType='ccodeS', modType='re', keyRegVar='LallyWt', 'LunIdPt','Ligo')

# # LallyWt + LunIdPt + Ligo, cross validation --- Leave one out using year as the partition
runModelParallel( modLooType='ccodeS', modType='re', keyRegVar='LallyWt', 'LunIdPt','Ligo')
 

################################################################