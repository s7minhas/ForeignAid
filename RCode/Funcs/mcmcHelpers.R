#######################
# fns for extracting data from mcmc chains

extractFromCoda <- function(coda,vname){
  dnames <- dimnames(coda[[1]])[[2]]
  theString <- paste("^",vname,sep="")
  cat(paste("searching for",theString,"in MCMC output\n"))
  theOnes <- grep(theString,
                  dnames)
  cat(paste("found",length(theOnes),"matching columns\n"))
  as.matrix(coda[[1]][,theOnes])
}

extractFromCodaNChains<-function(coda,vname){
  dnames <- dimnames(coda[[1]])[[2]]
  theString <- paste("^",vname,sep="")
  cat(paste("searching for",theString,"in MCMC output\n"))
  theOnes <- grep(theString,
                  dnames)
  cat(paste("found",length(theOnes),"matching columns\n"))
  do.call(cbind, lapply(coda, function(x){
  	return(x[, theOnes])
  	}))


}

extractCI <- function(x)c(mean(x),
                       quantile(x,c(.025,.975)))

#######################