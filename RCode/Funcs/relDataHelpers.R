# Build adjacency matrices from dyadic data
# Dyad data must identify countries by variables  
# ccode_1 & ccode_2 and the time aspect by a variable called year
# time is a simple vector of years
# panel is a dataset with country codes
DyadBuild <- function(variable, dyadData, time, panel=panel, directed=FALSE){

	countryList <- lapply(time, function(x) FUN=panel[panel$year==x,'ccode'])
	names(countryList) <- time

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

	  # Remove NAs
	  yearMatrix[is.na(yearMatrix)] = 0
	  
	  Mats[[ii]] <- yearMatrix
	  print(time[ii])
	}

	names(Mats) <- time
	Mats
}

# Build dyad dataset from monadic data
# This only works if the units listed in countrylist exactly
# match the units listed in the monad dataset
# Monad data must identify country by a variable called 
# ccode and the time aspect by a variable called by year
# time is a simple vector of years
# countryList is a list containing ccodes for each year
DyadBuild_fMonad <- function(variable, oper,
	monadData, time, countryList){
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
			if(oper=='absdiff'){relates <- abs(cntryRating-others)}
			if(oper=='same'){relates <- as.numeric(cntryRating==others)}
			yearMatrix[jj,] <- relates
		}

		diag(yearMatrix) <- 0
		undirectMats[[ii]] <- yearMatrix
		print(time[ii]) 
	}
		names(undirectMats) <- time
		undirectMats
}

# Create spatially weighted variables
# Requires access to SM created panel dataset
# In the dataset with the variables to be weighted it is 
# necessary to have country identifier given by ccode and
# time identifier given by year
spatialBuild <- function(spatList, varData, years, variable, sp_suffix, invert=FALSE){
	spatData <- NULL

	for(i in 1:length(years)){
		spatMat <- spatList[[i]]
		# rownames for matrices
		distNames <- as.numeric(rownames(spatMat))
		ndistNames <- panel$ccode[match(distNames, panel$GWCODE)]
		rownames(spatMat) <- ndistNames; colnames(spatMat) <- ndistNames

		# Invert
		if(invert){spatMat <- 1/spatMat; spatMat[spatMat==Inf] <- 0}

		# Applying row standardized weights
		dmatDenom <- apply(spatMat,1,sum)
		dmatDenom[dmatDenom==0] <- 1
		spatMat_rowst <- spatMat/dmatDenom
		
		# Bringing in fdi dataset
		spat_vars <- c('ccode', variable)
		dataYear <- varData[varData$year==years[i], spat_vars]
		dataYear <- dataYear[which(dataYear$ccode %in% ndistNames),]
		o <- as.character(dataYear$ccode)
		
		spatMat_rowst <- spatMat_rowst[o,o]
		# data rows with NAs that are in distance matrix
		# this is equivalent to just dropping them from teh 
		# spatial variable calculation
		dataYear[is.na(dataYear)] <- 0
		
		for(j in 1:nrow(spatMat_rowst)){
			row_weights <- NULL
			row_weights <- t(t(dataYear[,c(2:ncol(dataYear))]) %*%  spatMat_rowst[j,])
			row_weights2 <- NULL
			row_weights2 <- cbind(row_weights, years[i], dataYear$ccode[j])
			spatData <- rbind(spatData, row_weights2)
		}
	print(years[i])}
	spatData <- data.frame(spatData, row.names=NULL)
	spatData[,1:length(vars)] = apply(spatData[,1:length(vars)], 2, num)

	names(spatData) <- c(
		paste(sp_suffix,names(spatData)[1:(length(spat_vars)-1)],sep=''),
		'year','ccode')
	spatData$cyear <- paste(spatData$ccode, spatData$year, sep='') 
	spatData
}


##### From project with Cindy
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

# SM: change makeDyad to use lapply instead of for loop
makeDyad = function(data, unit){ 
uniqueUnit = unique(data[, unit])
rawDyad = lapply(1:length(uniqueUnit), function(ii){
    slice = data[which(data[, unit] == uniqueUnit[ii]),]
    if( dim(slice)[1] ==1 ){
        sList2 = cbind(slice[,1], slice[,4], NA, slice[,3], NA )} 
    else if ( dim(slice)[1] > 1 ){
            sList2 = dyadComb(unique(slice[, unit]),slice[,4], slice[,3]) }
    return(sList2) })
rawDyad = do.call('rbind', rawDyad)
rawDyad = data.frame(rawDyad, stringsAsFactors = F)
names(rawDyad) = c(unit, "cname_1", "cname_2", "ccode1", "ccode2")
rawDyad2 = rawDyad[-which(is.na(rawDyad$cname_2)),]
rawDyad2$year = substring(rawDyad2$yrRcid, 1, 4)
rawDyad2$dname = paste(rawDyad2$cname_1, rawDyad2$cname_2, sep = "_")
return(rawDyad2)
}

# aggDyad aggregates output from makeDyad by year
# SM: change makeDyad to use lapply instead of for loop
aggDyad = function(data, year, name){
  yrs = sort(unique(data[,year]))
  aggD = lapply(1:length(yrs), function(jj){
  	    slice = data[which(data[, year] == yrs[jj]),]
	    sList2 = data.frame(tapply(slice$year, slice$dyadID, length), year = yrs[jj])
    	return(data.frame(id = row.names(sList2), sList2)) })
  aggD = do.call('rbind', aggD)
  names(aggD) = c("dyadID", name, "year" )
  aggD$dyadID = as.numeric(as.character(aggD$dyadID))
  aggD$year = as.numeric(as.character(aggD$year))
  return(aggD)
}

# Convert dyadic dataset to list and standardize value
convToList = function(data, brks, brkVar, ids, var, standardize=TRUE, 
	addDyadLabel=FALSE, dLab='ij', iLab = 'ccode1', jLab = 'ccode2'
	){
	# Convert to list by brks, retaining id and var
	## also standardsize value within that year
	dList = lapply(brks, function(ii){
		slice = data[data[,brkVar]==ii,c(ids,var)]
		slice[,var] = slice[,var] %>% num()
		if(standardize){ slice[,var] = stdz( slice[,var] ) }
		if(addDyadLabel){ 
			slice$tmp = paste(slice[,iLab], slice[,jLab], sep='_')
			names(slice)[ncol(slice)] = dLab
		}
		return(slice)
		})
	
	# Cleanup and return	
	names(dList) = brks
	return( dList )	 
}