################################################################
# This takes a dataset, a variable country year which is
# a concatenation of the country identifier and year
# Then a country variable
# The varsTOlag should be inputted as a vector
# And lag is just a numeric specifying how many years to lag the variable
lagTS <- function(x,l){
  cuts <- (length(x)-(l-1)):length(x)
  c(rep(NA,l), x[ -cuts ] )
}

lagData <- function(data, country_year, country, varsTOlag, lag=1)
{
  data[,country_year] = num(data[,country_year])
  data[,country] = num(data[,country])  
  data <- data[order(data[,country_year]),]
  lagData <- apply(data[,varsTOlag], 2,
    function(x){
      unlist(by(x, data[,country], function(y) lagTS(y,lag) ) )
    } )
  colnames(lagData) <- paste('L' , varsTOlag, sep='')
  cbind(data, lagData)
}
################################################################

################################################################
## Expand panel dataset to account for all years
panelyear<-function(dataset, styear, endyear){
fulldata<-list()
for ( i in 1:length(dataset[,1])){
	fulldata[[i]] <- cbind(dataset[i,], year=styear[i]:endyear[i], row.names = NULL)
}
fulldata1 <- do.call(rbind, fulldata)
return(fulldata1)
}
################################################################