#######################################################################
# Clearing workspace
rm(list=ls())

# Setting working directory
if(Sys.info()["user"]=="janus829")
{pathMain="~/Dropbox/Research/ButheProjects/ForeignAid/AlesinaDollarReplication";
	pathGraphics="~/Dropbox/Research/ButheProjects/ForeignAid/AlesinaDollarReplication/Graphics";
	pathData="~/Dropbox/Research/ButheProjects/ForeignAid/AlesinaDollarReplication/Data";
	pathResults="~/Dropbox/Research/ButheProjects/ForeignAid/AlesinaDollarReplication/Results";
	pathFunctions="~/Desktop/Prog Notes/R Functions";
	pathCode="~/Desktop/Research/ButheProjects/ForeignAid/AlesinaDollarReplication/RCode" }

# Loading libraries and functions
require(foreign)
require(reshape)
require(doBy)
require(plyr)
require(sbgcop)
require(scapeMCMC)

require(countrycode)
require(cshapes)
require(gpclib)

require(ggplot2)
theme_set(theme_bw())
require(RColorBrewer)

require(qgraph)
require(network)

# Setting seed
set.seed(6886)
setwd(pathMain)

# Additional functions
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

# Log transformations for vars with negative values
logNeg <- function(z){
	x <- z[!is.na(z)]; y <- x
	y[x>0] <- log(x[x>0]); y[x<0] <- -log(abs(x[x<0])); y[x==0] <- 0
	z[!is.na(z)] <- y; z
}

# Rescaling variables
rescale <- function(x,new_max,new_min){
 xResc <- (new_max - new_min) / (max(x,na.rm=T) - min(x,na.rm=T))*(x - min(x,na.rm=T)) + new_min
 xResc }

# turn variables into numeric
numSM <- function(x){ as.numeric(as.character(x)) }

# The function to use is lagDataSM
# This takes a dataset, a variable country year which is 
# a concatenation of the country identifier and year
# Then a country variable
# The varsTOlag should be inputted as a vector
# And lag is just a numeric specifying how many years to lag the variable
lagTS <- function(x,l){
  cuts <- (length(x)-(l-1)):length(x)
  c(rep(NA,l), x[ -cuts ] )
}

lagDataSM <- function(data, country_year, country, varsTOlag, lag)
{
  data <- data[order(data[,country_year]),]
  lagData <- apply(data[,varsTOlag], 2, 
    function(x){
      unlist(by(x, data[,country], function(y) lagTS(y,lag) ) ) 
    } )
  colnames(lagData) <- paste('lag', lag, '_', varsTOlag, sep='')
  cbind(data, lagData)
}

# Calculate cumulative sum of var
cumulTS <- function(
	data=srData, cntry_var='cname', time_var='year', key='cyear', start=1960, end=2013, var){
	print(paste('Progress in calculating cumulative sum for ', var))
	cum_var <- paste('c',var,sep='')
	temp <- srData[data[,time_var]>=start & data[,time_var]<end,c(key,cntry_var,time_var,var)]
	temp <- cbind(temp, 0)
	names(temp) <- c('cyear', 'cntry', 'year', var, cum_var)
	
	countries <- unique(data[,cntry_var]); years <- start:end; fullData <- NULL

	for(ii in 1:length(countries)){
		slice <- temp[temp$cntry==countries[ii],]
		years <- min(slice$year):max(slice$year)
			for(jj in 2:length(years)){
				slice[slice$year==years[jj],cum_var] <- 
					slice[slice$year==years[jj],var] + 
						slice[slice$year==(years[jj]-1),cum_var] }
			fullData <- rbind(fullData, slice) 
			if(ii==1 | ii%%20==0 | ii==length(countries)){
				cat(paste(round(100*ii/length(countries),0),'% ',sep=''))}
		}
	print(' Completed '); fullData[,c(key, var, cum_var)]
}

# Build undirected dyad dataset from dyadic data
# Dyad data must identify countries by variables  
# ccode_1 & ccode_2 and the time aspect by a variable called year
# time is a simple vector of years
# countryList is a list containing ccodes for each year
DyadBuild <- function(variable, dyadData, time, countryList, directed=FALSE){
	Mats <- list()
	for(ii in 1:length(time)){
	  countries <- countryList[[ii]]
	  yearMatrix <- matrix(0, nrow=length(countries), ncol=length(countries))
	  rownames(yearMatrix) <- colnames(yearMatrix) <- countries
	  
	  dyadData <- dyadData[,c('ccode_1','ccode_2','year',variable)]
	  dyadData <- data.matrix(dyadData)
	  data <- matrix(dyadData[which(dyadData[,'year'] %in% time[ii]),], ncol=4, 
	                 dimnames=list(NULL, c('ccode_1','ccode_2','year',variable)))
	  
	  for(jj in 1:nrow(yearMatrix)){
	    slice <- matrix(data[which(data[,'ccode_1'] %in% countries[jj]), c('ccode_2',variable)], ncol=2, 
	                    dimnames=list(NULL, c('ccode_2',variable)))
	    rownames(slice) <- slice[,'ccode_2']
	    x <- intersect(countries, as.vector(slice[,'ccode_2']))
	    slice2 <- matrix(slice[as.character(x),], ncol=2, 
	                     dimnames=list(NULL, c('ccode_2',variable)))
	    rownames(slice2) <- slice2[,'ccode_2']
	    
	    yearMatrix[as.character(countries[jj]), rownames(slice2)] <- slice2[,variable]
	    if(directed==FALSE){yearMatrix[rownames(slice2), as.character(countries[jj])] <- slice2[,variable]}
	  }
	  
	  Mats[[ii]] <- yearMatrix
	  print(time[ii])
	}

	names(Mats) <- time
	Mats
}

# Build undirected dyad dataset from monadic data
# This only works if the units listed in countrylist exactly
# match the units listed in the monad dataset
# Monad data must identify country by a variable called 
# ccode and the time aspect by a variable called by year
# time is a simple vector of years
# countryList is a list containing ccodes for each year
undirDyadBuild_fMonad <- function(variable, monadData, time, countryList){
	monadData <- monadData[,c('ccode','year',variable)]
	monadData <- data.matrix(monadData)
	rownames(monadData) <- monadData[,'ccode']

	undirectMats <- list()

	for(ii in 1:length(time)){
		countries <- countryList[[ii]]
		yearMatrix <- matrix(0, nrow=length(countries), ncol=length(countries))
		rownames(yearMatrix) <- colnames(yearMatrix) <- countries

		data <- monadData[which(monadData[,'year'] %in% time[ii]), ]

		for(jj in 1:nrow(yearMatrix)){
			cntryRating <- data[as.character(countries[jj]),variable]
			others <- data[as.character(countries),variable]
			diffs <- abs(cntryRating-others)
			yearMatrix[jj,] <- diffs
		}

		undirectMats[[ii]] <- yearMatrix
		print(time[ii]) 
	}
		names(undirectMats) <- time
		undirectMats
}
#######################################################################