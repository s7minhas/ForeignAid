# create save object
storeResults = function(modsOrig, type='re'){
  out = lapply(modsOrig, function(modsImp){
    lapply(modsImp, function(mod){
      varcov = vcov(mod)
      summ = cbind(
        'Estimate' = fixef(mod),
        'Std. Error' = sqrt(diag(varcov))
      )
      return(list(
        varcov=varcov,
        summ=summ
      ))
    })
  })  
  return(out)
}

# from Amelia
mi.meld = function (q, se, byrow = TRUE) 
{
  if (!byrow) {
    q <- t(q)
    se <- t(se)
  }
  if (is.data.frame(q)) {
    q <- as.matrix(q)
  }
  if (is.data.frame(se)) {
    se <- as.matrix(se)
  }
  am.m <- nrow(q)
  ones <- matrix(1, nrow = 1, ncol = am.m)
  imp.q <- (ones %*% q)/am.m
  ave.se2 <- (ones %*% (se^2))/am.m
  diff <- q - matrix(1, nrow = am.m, ncol = 1) %*% imp.q
  sq2 <- (ones %*% (diff^2))/(am.m - 1)
  imp.se <- sqrt(ave.se2 + sq2 * (1 + 1/am.m))
  return(list(q.mi = imp.q, se.mi = imp.se))
}

#
rubinCoef <-  function(
  mod, modType='reSmall', matrixFormat=FALSE
  ){
  
  # pull out results from re model
  if(modType=='re'){
    modCoef = lapply(mod, function(x){
      beta = fixef(x)
      se = sqrt(diag(vcov(x)))
      return( cbind(beta, se) )
    }) %>% do.call('rbind',.) 
  }

  # pull out results from shortened re model
  if(modType=='reSmall'){
    modCoef = lapply(mod, function(summ){
      summ = summ$summ
      summ = summ[!grepl('factor',rownames(summ)),]
      summ = summ[,c('Estimate','Std. Error')]
      colnames(summ) = c('beta','se')
      return(summ)
    }) %>% do.call('rbind',.) 
  }  
  
  # pull out results from re model
  if(modType=='fe'){
    modCoef = lapply(mod, function(summ){
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