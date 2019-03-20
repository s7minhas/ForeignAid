library(Amelia)
rubinCoef <-  function(
  mod, modType='re', matrixFormat=FALSE
  ){
  
  # pull out results from re model
  if(modType=='re'){
    modCoef = lapply(mod, function(x){
      beta = fixef(x)
      se = sqrt(diag(vcov(x)))
      return( cbind(beta, se) )
    }) %>% do.call('rbind',.) 
  }

  # pull out results from re model
  if(modType=='fe'){
    modCoef = lapply(mod, function(x){
      summ = summary(x)$'coefficients'
      summ = summ[!grepl('factor',rownames(summ)),]
      summ = summ[,c('Estimate','Std. Error')]
      colnames(summ) = c('beta','se')
      return(summ)
    }) %>% do.call('rbind',.) 
  }

  # meld together
  modSumm = mi.meld(
    q=matrix(
      modCoef[,1],ncol=length(unique(rownames(modCoef))), 
      byrow=TRUE), 
    se=matrix(
      modCoef[,2],ncol=length(unique(rownames(modCoef))), 
      byrow=TRUE)
    ) %>%
    lapply(., t) %>% do.call('cbind',.) %>% data.frame(.)

  # organize output
  names(modSumm) = c('beta', 'se')
  modSumm$t = modSumm$beta/modSumm$se
  modSumm$var = unique(rownames(modCoef))

  # matrix option
  if(matrixFormat){
    names(modSumm) = c('Estimate', 'Std. Error', 't value', 'var')
    rownames(modSumm) = modSumm$var
    modSumm = data.matrix(modSumm[,-ncol(modSumm)]) }

  # return
  return(modSumm)
}