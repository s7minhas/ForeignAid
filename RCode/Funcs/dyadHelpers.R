################################################################

## Turns data that is in country-year format into an edgelist: country-country-year format aggregated by year

# The combinations of the dyadComb and makeDyad below make country-year data into country-country-year data with potential replicates in year

dyadComb  = function(id, name, num){
	if (!is.na(name) & !is.na(num)){
   		dyads = cbind(id, t(combn(name, 2)), t(combn(num, 2)))
	} else if (is.na(name)){
		dyads = cbind(id, t(combn(num, 2)))	
	} else if (is.na(num)){
		dyads = cbind(id, t(combn(name, 2)))	
	}
	return(dyads)
} 

makeDyad = function(data, unit){
uniqueUnit = unique(data[, unit])
rawDyad = NULL
for (ii in 1:length(uniqueUnit)){
    slice = data[which(data[, unit] == uniqueUnit[ii]),]
    if( dim(slice)[1] ==1 ){
        sList2 = cbind(slice[,1], slice[,4], NA, slice[,3], NA )} 
    else if ( dim(slice)[1] > 1 ){
            sList2 = dyadComb(unique(slice[, unit]),slice[,4], slice[,3]) }
    rawDyad = rbind(rawDyad, sList2)
}
rawDyad = data.frame(rawDyad, stringsAsFactors = F)
names(rawDyad) = c(unit, "cname_1", "cname_2", "ccode1", "ccode2")
rawDyad2 = rawDyad[-which(is.na(rawDyad$cname_2)),]
rawDyad2$year = substring(rawDyad2$yrRcid, 1, 4)
rawDyad2$dname = paste(rawDyad2$cname_1, rawDyad2$cname_2, sep = "_")
return(rawDyad2)
}

# aggDyad aggregates output from makeDyad by year
aggDyad = function(data, year, name){
  yrs = sort(unique(data[,year]))
  aggD = NULL
  for (jj in 1:length(yrs)){
    slice = data[which(data[, year] == yrs[jj]),]
    sList2 = data.frame(tapply(slice$year, slice$dyadID, length), year = yrs[jj])
    aggD = rbind(aggD, data.frame(id = row.names(sList2), sList2))
  }
  names(aggD) = c("dyadID", name, "year" )
  aggD$dyadID = as.numeric(as.character(aggD$dyadID))
  aggD$year = as.numeric(as.character(aggD$year))
  return(aggD)
}

### Functions for plotting dyads


 
plotSub=function(csk, data, ylab){
  countrynames = countrycode(csk, 'cown', 'country.name')
  countrynames[which(csk==731)] = 'NORTH KOREA'
  dyadComb = t(combS(countrynames, 2))
  dyad = data.frame(paste(dyadComb[,1], dyadComb[,2], sep = "-"), dyadComb)
  names(dyad) = c("dyadName", "cname_1", "cname_2")
  
  triad=data[which(data$ccode1 %in% csk & data$ccode2 %in% csk), c('dyadID', 'cname_1', 'cname_2', 'year', 'PCAStd')]
  triad = merge(triad, dyad, by = c("cname_1", "cname_2"))

  yrLabels = seq(min(triad$year), max(triad$year), 7)  
  ggplot(triad, aes(x=year, y=PCAStd, color=dyadName)) +
    geom_line() +
    geom_point() + 
    scale_y_continuous(ylab) +
    scale_x_continuous('', breaks=yrLabels, labels=yrLabels) +
    theme(
      axis.ticks=element_blank(),
      legend.position="bottom",
      legend.title=element_blank(),
      panel.border=element_blank()
      )
}
  
  
combS <- function (x, m, FUN = NULL, simplify = TRUE, ...) 
  {
    if (m < 0) 
      stop("m < 0", domain = NA)
    if (is.numeric(x) && length(x) == 1L && x > 0 && trunc(x) == 
          x) 
      x <- seq_len(x)
    n <- length(x)
    if (n < m) 
      stop("n < m", domain = NA)
    m <- as.integer(m)
    e <- 0
    h <- m
    a <- seq_len(m)
    nofun <- is.null(FUN)
    if (!nofun && !is.function(FUN)) 
      stop("'FUN' must be a function or NULL")
    len.r <- length(r <- if (nofun) x[a] else FUN(x[a], ...))
    count <- as.integer(round(choose(n, m)))
    if (simplify) {
      dim.use <- if (nofun) 
        c(m, count)
      else {
        d <- dim(r)
        if (length(d) > 1L) 
          c(d, count)
        else if (len.r > 1L) 
          c(len.r, count)
        else c(d, count)
      }
    }
    if (simplify) {
      out <- matrix(r, nrow = len.r, ncol = count)
    }
    else {
      out <- vector("list", count)
      out[[1L]] <- r
    }
    if (m > 0) {
      i <- 2L
      nmmp1 <- n - m + 1L
      while (a[1L] != nmmp1) {
        if (e < n - h) {
          h <- 1L
          e <- a[m]
          j <- 1L
        }
        else {
          e <- a[m - h]
          h <- h + 1L
          j <- 1L:h
        }
        a[m - h + j] <- e + j
        r <- if (nofun) 
          x[a]
        else FUN(x[a], ...)
        if (simplify) 
          out[, i] <- r
        else out[[i]] <- r
        i <- i + 1L
      }
    }
    if (simplify) 
      array(out, dim.use)
    else out
  }


#######################