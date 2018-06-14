if(Sys.info()['user']=='s7m' | Sys.info()['user']=='janus829'){ source('~/Research/ForeignAid/RCode/setup.R') }
if(Sys.info()['user']=='cindycheng'){ source('~/Documents/Papers/ForeignAid/RCode/setup.R') }
 
################################################################
# Load reg data
setwd(pathData)
load('iDataDisagg.rda')


dvs = c('humanitarianTotal', 'developTotal', 'civSocietyTotal', 'notHumanitarianTotal')
ids = names(iData[[1]])[c(1:3,25)]
ivs = names(iData[[1]])[9:24]

iData = lapply(iData, function(x){
  # add dyadic id
  x$id = paste(x$ccodeS, x$ccodeR, sep='_')
  x$id = factor(x$id)
  # create total aid
  x$aidTotal = x$notHumanitarianTotal + x$humanitarianTotal
  # log dvs
  for(dv in c('aidTotal',dvs)){ x[,dv] = log(x[,dv] + 1) }
  return(x) })


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
ivs = c('LstratMu', 'LallyWt', 'LunIdPt', 'Ligo')


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

################################################################

################################################################

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


# get data
xPartitionList = function(mod){
lapply(mod[[1]], function(x){
  model.matrix(x)
  })
}

# get dependent variable
getDepVar = function(mod){
lapply(mod[[1]], function(x){
  model.frame(x)[,1]
  })
}


# get rmse
getRMSE = function( coef, data, dv){
  pred = t(coef$beta %*% t(cbind(1, data[,coef$var[-1]])))
  c(pred - dv)^2 %>% mean(.) %>% sqrt(.) 
}

# get rmse for each 
getRMSE_KFold = function(rcoefList, xList, depVar){
 mapply(getRMSE, coef = rcoefList, data = xList, dv = depVar)
}


# extract RMSES for all models
rmses = list()
modelNames = gsub('.rda', '',toLoad)
for ( i in 1:length(modelNames)){
  print(i)
  rmses[[i]] = getRMSE_KFold(rcoefList = rubinCoefList(mod = get(modelNames[i]), k = 10), 
                     xList =xPartitionList(get(modelNames[i]) ),
                     depVar = getDepVar(get(modelNames[i])) 
                      )}


 
# put rmses into a data frame
names(rmses) = gsub('10-Fold_|gaussian_re_|\\*Lno_disasters', '', modelNames)
rmsesDf = unlist(rmses) %>% data.frame()
rmsesDf$mod = gsub('\\d', '',rownames(rmsesDf))
rmsesDf = cbind(rmsesDf, data.frame(do.call(rbind, str_split(  rmsesDf$mod, '\\_'))))
rownames(rmsesDf) = NULL
names(rmsesDf) = c('rmse', 'mod', 'partition', 'iv', 'dv')

# clean up dv names
rmsesDf$dvName = factor(rmsesDf$dv) 
levels(rmsesDf$dvName) = c("Civil Society Aid", "Development Aid", "Humanitarian Aid")
rmsesDf$dvName = factor(rmsesDf$dvName , levels(rmsesDf$dvName)[c(3, 1,2)])

# clean up iv names
rmsesDf$ivName = factor(rmsesDf$iv) 
levels(rmsesDf$ivName) = c('Alliances', 'IGO Membership', 'Strategic Interest', 'UN Ideal Point') 
rmsesDf$ivName = factor(rmsesDf$ivName , levels(rmsesDf$ivName)[c(3, 1, 2, 4)])

# aggregate rmses by partition, iv and dv 
rmsesDfAgg = rmsesDf %>% group_by(partition, ivName, dvName) %>% summarise(mean = mean(rmse), sd = sd(rmse)) %>% data.frame()
rmsesDfAgg_ccodeS = rmsesDfAgg[grep('ccodeS', rmsesDfAgg$partition),]
rmsesDfAgg_ccodeR = rmsesDfAgg[grep('ccodeR', rmsesDfAgg$partition),] 
rmsesDfAgg_year = rmsesDfAgg[grep('year', rmsesDfAgg$partition),]

################################################################

################################################################
# make and save plots with 90 and 95% confidence intervals

pdf(paste0(pathGraphics,'/rmse_10FoldCrossVal_ccodeS.pdf'))
ggplot(rmsesDfAgg_ccodeS, aes(x = dvName , y = mean))+
  geom_errorbar( aes(color = ivName , ymin = mean-1.96*sd, ymax = mean+1.96*sd), position = dodge)+
 geom_errorbar( aes(color = ivName , ymin = mean-1.64*sd, ymax = mean+1.64*sd), position = dodge, alpha = .5)+
  geom_point(aes(color = ivName, y = mean), position = dodge)  +
  labs(title = "10 Fold Cross Validation, partioned by Donor Country",
     y = 'Root Mean Squared Error',
        x= 'Dependent Variable')+ 
  guides(color=guide_legend(title="Independent Variable"))%>% print()
  dev.off()

pdf(paste0(pathGraphics,'/rmse_10FoldCrossVal_ccodeR.pdf'))
ggplot(rmsesDfAgg_ccodeR, aes(x = dvName , y = mean))+
  geom_errorbar( aes(color = ivName , ymin = mean-1.96*sd, ymax = mean+1.96*sd), position = dodge)+
 geom_errorbar( aes(color = ivName , ymin = mean-1.64*sd, ymax = mean+1.64*sd), position = dodge, alpha = .5)+
  geom_point(aes(color = ivName, y = mean), position = dodge)  +
  labs(title = "10 Fold Cross Validation, partioned by Recipient Country",
     y = 'Root Mean Squared Error',
        x= 'Dependent Variable')+ 
  guides(color=guide_legend(title="Independent Variable"))
 %>% print()
  dev.off()


pdf(paste0(pathGraphics,'/rmse_10FoldCrossVal_year.pdf'))
ggplot(rmsesDfAgg_year, aes(x = dvName , y = mean))+
  geom_errorbar( aes(color = ivName , ymin = mean-1.96*sd, ymax = mean+1.96*sd), position = dodge)+
 geom_errorbar( aes(color = ivName , ymin = mean-1.64*sd, ymax = mean+1.64*sd), position = dodge, alpha = .5)+
  geom_point(aes(color = ivName, y = mean), position = dodge)  +
  labs(title = "10 Fold Cross Validation, partioned by Year",
     y = 'Root Mean Squared Error',
        x= 'Dependent Variable')+ 
  guides(color=guide_legend(title="Independent Variable"))
 %>% print()
  dev.off()
   
