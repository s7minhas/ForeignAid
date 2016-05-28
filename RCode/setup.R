# Setting working directory
rm(list=ls())
if(
Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"
){pathMain="~/Dropbox/Research/ForeignAid";
  pathGraphics="~/Dropbox/Research/ForeignAid/Graphics";
  # pathGraphics='~/Research/ForeignAid/Presentations/Graphics';  
  pathResults='~/Dropbox/Research/ForeignAid/Results';
  pathCode='~/Research/ForeignAid/RCode'
  pathData="~/Dropbox/Research/ForeignAid/Data"
  } else if  (Sys.info()["user"]=="cindycheng") {
  pathMain="~/Dropbox/Documents/Papers/ForeignAid";
  pathGraphics="~/Dropbox/Documents/Papers/ForeignAid/graphics";
  pathData="~/Dropbox/Documents/Papers/ForeignAid/data";
  pathCode="~/Dropbox/Documents/Papers/ForeignAid1/RCode";
  pathFunctions="~/Dropbox/Documents/Methods/R Functions"
  pathResults = '~/Dropbox/Documents/Papers/ForeignAid/Results'}

# Loading libraries and functions
toLoad=c('snow', 'mcmcplots', 'foreign', 'cshapes', 'countrycode', 'reshape', 
	'gtools', 'ggplot2', 'doBy', 'Amelia', 'tikzDevice', 
  'foreach', 'doParallel',
	'igraph', 'bipartite', 'lme4', 'glmmADMB','MASS', 'grid')
for(lib in toLoad){
  if(!(lib %in% installed.packages()[,1])){ 
  	install.packages(lib, repos='http://cran.rstudio.com/') }
  library(lib, character.only=TRUE)
}

## gg theme
theme_set(theme_bw())

### PACKAGE VERIONS
## Must have countrycode version 0.16 installed

# Helper dataset
setwd(pathData)
load('panel.rda')
panel$cnameYear<-toupper(panel$cnameYear) 
panel$cname<-toupper(panel$cname) # not for some reason German Democratic Republic is notcapitalized
 
# Setting seed
set.seed(6886)
setwd(pathMain)

# Source scripts
setwd(pathCode)
source('vizResults.R')
################################################################
# Helper functions
trim = function (x) gsub("^\\s+|\\s+$", "", x)

substrRight = function(x, n){
  substr(x, nchar(x)-n+1, nchar(x)) }

num = function(x){ as.numeric(as.character(x)) }

char = function(x){as.character(x)}

pasteVec = function(x,y){ as.vector(outer(x,y,paste0)) }
################################################################

################################################################
# Log transformations for vars with negative values
logNeg = function(z){
	x = z[!is.na(z)]; y = x
	y[x>0] = log(x[x>0]); y[x<0] = -log(abs(x[x<0])); y[x==0] = 0
	z[!is.na(z)] = y; z
}

# Rescaling variables
rescale = function(x,new_max,new_min){
 xResc = (new_max - new_min) / (max(x,na.rm=T) - min(x,na.rm=T))*(x - min(x,na.rm=T)) + new_min
 xResc }
 ################################################################

################################################################
# Convert to cname
cname = function(x){
	x = as.character(x)
	toupper(countrycode(x, 'country.name', 'country.name')) }
################################################################

################################################################
### Fx for Melting/Cleaning WB Data for Merge
cleanWbData = function(data, variable){
	var = variable
	mdata = melt(data, id=c('Country.Name', 'Country.Code'))
	names(mdata)[4] = var
	mdata$year =  as.numeric(as.character(substring(mdata$variable,2)))
	mdata = mdata[,c(1,2,5,4)]

	# Remove non-country observations and small islands/territories
	drop = c('Arab World', 'Caribbean small states',
		'East Asia & Pacific (all income levels)',
		'East Asia & Pacific (developing only)', 'Euro area',
		'Europe & Central Asia (all income levels)',
		'Europe & Central Asia (developing only)',
		'European Union', 'Heavily indebted poor countries (HIPC)',
		'High income', 'High income: nonOECD', 'High income: OECD',
		'Latin America & Caribbean (all income levels)',
		'Latin America & Caribbean (developing only)',
		'Least developed countries: UN classification',
		'Low & middle income', 'Low income', 'Lower middle income',
		'Middle East & North Africa (all income levels)',
		'Middle East & North Africa (developing only)', 'Middle income',
		'North America', 'Not classified', 'OECD members',
		'Other small states', 'Pacific island small states',
		'Small states', 'South Asia',
		'Sub-Saharan Africa (all income levels)',
		'Sub-Saharan Africa (developing only)', 'Upper middle income',
		"East Asia and the Pacific (IFC classification)",
		"Europe and Central Asia (IFC classification)",
		"Latin America and the Caribbean (IFC classification)",
		"Middle East and North Africa (IFC classification)",
		"South Asia (IFC classification)", "Sub-Saharan Africa (IFC classification)",
		'World',
		 "American Samoa",            "Aruba",
		 "Bermuda",                   "Cayman Islands", "Channel Islands",
		 "Curacao",                   "Faeroe Islands",
		 "French Polynesia",          "Greenland",
		 "Guam",                      "Hong Kong SAR, China",
		 "Isle of Man",               "Macao SAR, China",
		 "New Caledonia",             "Northern Mariana Islands",
		 "Puerto Rico",               "Sint Maarten (Dutch part)",
		 "St. Martin (French part)",  "Turks and Caicos Islands",
		 "Virgin Islands (U.S.)",     "West Bank and Gaza")
	mdata = mdata[which(!mdata$Country.Name %in% drop),]

	# Setting standardized countryname for WB data
	mdata$Country.Name = as.character(mdata$Country.Name)
	mdata$Country.Name[mdata$Country.Name=='Korea, Dem. Rep.'] = 'North Korea'
	mdata$Country.Name[mdata$Country.Name=='Korea, Rep.'] = 'South Korea'
	mdata$cname = cname(mdata$Country.Name)
	mdata$cnameYear = paste(mdata$cname, mdata$year, sep='')

	# Adding in codes from panel
	mdata$ccode = panel$ccode[match(mdata$cname,panel$cname)]
	mdata$cyear = paste(mdata$ccode, mdata$year, sep='')
	mdata }
################################################################

################################################################
# Build adjacency matrices from dyadic data
# Dyad data must identify countries by variables
# ccode_1 & ccode_2 and the time aspect by a variable called year
# time is a simple vector of years
# panel is a dataset with country codes
DyadBuild <- function(variable, dyadData, cntry1, cntry2, cntryYear1 = NA, cntryYear2 = NA, time, pd, panel=panel, directed=FALSE){
		if ( is.na(cntryYear1) ==T){
			panelMatch <- panel 
		 }else  { 
			panelMatch <- panel[-which(panel$cnameYear 
				%in% intersect(setdiff(panel$cnameYear, dyadData[,cntryYear1]),
							setdiff(panel$cnameYear, dyadData[,cntryYear2]))       ),] }
			
	countryList <- lapply(pd, function(x) FUN=panelMatch[panelMatch$year==x,'ccode'])
	names(countryList) <- pd
 
 	Mats <- list()
	for(ii in 1:length(pd)){
	  countries <- countryList[[ii]]
	  yearMatrix <- matrix(0, nrow=length(countries), ncol=length(countries))
	  rownames(yearMatrix) <- colnames(yearMatrix) <- countries

	  dyadData <- dyadData[,c(cntry1,cntry2,time,variable)]
	  dyadData <- data.matrix(dyadData)
	  data <- matrix(dyadData[which(dyadData[,time] %in% pd[ii]),], ncol=4,
	                 dimnames=list(NULL, c(cntry1,cntry2,time,variable)))

	  for(jj in 1:nrow(yearMatrix)){
	    slice <- matrix(data[which(data[,cntry1] %in% countries[jj]), c(cntry2,variable)], ncol=2,
	                    dimnames=list(NULL, c(cntry2,variable)))
	    rownames(slice) <- slice[,cntry2]
	    x <- intersect(countries, as.vector(slice[,cntry2]))
	    slice2 <- matrix(slice[as.character(x),], ncol=2,
	                     dimnames=list(NULL, c(cntry2,variable)))
	    rownames(slice2) <- slice2[,cntry2]

	    yearMatrix[as.character(countries[jj]), rownames(slice2)] <- slice2[,variable]
	    if(directed==FALSE){yearMatrix[rownames(slice2), as.character(countries[jj])] <- slice2[,variable]}
	  }

	  Mats[[ii]] <- yearMatrix
	  print(pd[ii])
	}

	names(Mats) <- pd
	Mats
}
################################################################

################################################################
# Calculate moving average or sum for network data
matrixMatcher=function(matToMatch, toAdd){
		addCntr=setdiff(toAdd,rownames( matToMatch ))
		addRows=matrix(NA, nrow=length(addCntr),ncol=nrow(matToMatch), 
			dimnames=list(c(addCntr), NULL))
		matToMatch=rbind(matToMatch, addRows)
		addCols=matrix(NA, ncol=length(addCntr),nrow=nrow(matToMatch), 
			dimnames=list(NULL, c(addCntr)))
		cbind(matToMatch, addCols) }

mvaStatMat=function(years, wdow, mats, avg=TRUE){

	matListStat=list()

	for(ii in 1:length(years) ){

		sy1=years[ii]-wdow+1; sy2=years[ii]
		ys=as.character(sy1:sy2)
		matList=mats[ys]
		namL=names(matList); namL=namL[!is.na(namL)]
		matList=mats[namL]
		kmat=mats[as.character(sy2)][[1]]
		cntries=rownames(kmat); lcnt=length(cntries)

		matList=lapply(matList, function(x) FUN=matrixMatcher(x, cntries))
		matList2=lapply(matList, function(x) FUN=x[cntries,cntries])

		if(avg){matStat=rowMeans( 
			array(unlist(matList2), dim = c(lcnt,lcnt,length(matList2))),
			 dims=2, na.rm=T) }
		if(!avg){matStat=rowSums( 
			array(unlist(matList2), dim = c(lcnt,lcnt,length(matList2))),
			 dims=2, na.rm=T) }		

		matStat=matrix(matStat, nrow=lcnt, ncol=lcnt, dimnames=list(cntries, cntries))
		matListStat[[ii]]=matStat; print(years[ii])
		}
	names(matListStat)=years; matListStat	
}
################################################################

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
  dyadComb = t(combS(countrynames, 2))
  dyad = data.frame(paste(dyadComb[,1], dyadComb[,2], sep = "-"),
                    dyadComb)
  names(dyad) = c("dyadName", "cname_1", "cname_2")
  
  
  triad=data[which(data$ccode1 %in% csk & data$ccode2 %in% csk), c('dyadID', 'cname_1', 'cname_2', 'year', 'PCAStd')]
  triad = merge(triad, dyad, by = c("cname_1", "cname_2"))
  ggplot(triad, aes(x=year, y=PCAStd, color=dyadName))+geom_line()+
    theme(legend.position="bottom")+ ylab(ylab)
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

#######################

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
    library(plyr)

    # New version of length which can handle NA's: if na.rm==T, don't count them
    length2 <- function (x, na.rm=FALSE) {
        if (na.rm) sum(!is.na(x))
        else       length(x)
    }
 
    # This does the summary. For each group's data frame, return a vector with
    # N, mean, and sd
    datac <- ddply(data, groupvars, .drop=.drop,
      .fun = function(xx, col) {
        c(N    = length2(xx[[col]], na.rm=na.rm),
          mean = mean   (xx[[col]], na.rm=na.rm),
          sd   = sd     (xx[[col]], na.rm=na.rm)
        )
      },
      measurevar
    )

    # Rename the "mean" column    
    datac <- rename(datac, c("mean" = measurevar))

    datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval: 
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, datac$N-1)
    datac$ci <- datac$se * ciMult

    return(datac)
}
