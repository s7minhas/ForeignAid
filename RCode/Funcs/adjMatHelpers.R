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