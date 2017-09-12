library(Amelia)
rubinCoef = function(mod, matrixFormat=FALSE){
  modCoef = lapply(mod, function(x){
    beta = fixef(x)
    se = sqrt(diag(vcov(x)))
    return( cbind(beta, se) )
    }) %>% do.call('rbind',.) 

  modSumm = mi.meld(
    q=matrix(modCoef[,1],ncol=length(unique(rownames(modCoef))), byrow=TRUE), 
    se=matrix(modCoef[,2],ncol=length(unique(rownames(modCoef))), byrow=TRUE), 
    byrow=TRUE) %>% lapply(., t) %>% do.call('cbind',.) %>% data.frame(.)

  names(modSumm) = c('beta', 'se')
  modSumm$t = modSumm$beta/modSumm$se
  modSumm$var = unique(rownames(modCoef))

  if(matrixFormat){
    names(modSumm) = c('Estimate', 'Std. Error', 't value', 'var')
    rownames(modSumm) = modSumm$var
    modSumm = data.matrix(modSumm[,-ncol(modSumm)]) }

  return(modSumm)
}