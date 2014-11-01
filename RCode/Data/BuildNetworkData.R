 if  (Sys.info()["user"]=="cindycheng") {
  	pathCode="~/Documents/Papers/ForeignAid/RCode"}

##### Load Packages and Functions #####
source(paste0(pathCode, '/setup.R'))
source(paste0(pathCode, '/bipartite.R'))
source(paste0(pathCode, '/ggnet.R'))



toLoad=c('networkDynamic', 'network', 'sna', 'ggplot2', "intergraph")
for(lib in toLoad){
  if(!(lib %in% installed.packages()[,1])){ 
  	install.packages(lib, repos='http://cran.rstudio.com/') }
  library(lib, character.only=TRUE)
}





 
###### Clean Data

convert.magic <- function(obj,types){
    for (i in 1:length(obj)){
        FUN <- switch(types[i],character = as.character, 
                                   numeric = as.numeric, 
                                   factor = as.factor)
        obj[,i] <- FUN(obj[,i])
    }
    obj
}


cleanDyad <- function(data, sender = "", receiver  ="", time = "", value = "" ){ # clean bipartite data
	data.clean <- data[which(!is.na(data[,value])), c(time, receiver, sender, value)]
	names(data.clean)<-c("time", "receiver", "sender", "value")
	data.clean$time <-data.clean$time - min(data.clean$time, na.rm = T)
	data.clean = convert.magic(data.clean , c("numeric", "factor", "factor", "numeric"))
	data.clean$sender = droplevels(data.clean$sender)
	data.clean$receiver = droplevels(data.clean$receiver)
	
	nr = length(levels(data.clean$receiver)) # number of receivers
	ns = length(levels(data.clean$sender))# number of senders
 
 	data.clean$nameR = data.clean$receiver
	data.clean$nameS = data.clean$sender

	levels(data.clean$receiver) = as.character(seq( 1, nr))
	levels(data.clean$sender) = as.character(seq( 1 + nr, ns + nr))
	data.clean$sender <- as.numeric(as.character(data.clean$sender))
	data.clean$receiver <- as.numeric(as.character(data.clean$receiver))
	 
 
	return(data.clean) # first four columns of data is in numeric format and last two are factors
}

########### Put data into adjacency matrix for each year


adjMatTS <- function (data,sender = "sender", receiver = "receiver", time = "time", value = "value", complete = FALSE){

data$time = data[, time]
data$receiver = data[, receiver]
data$sender = data[, sender]

nr = length(unique(aid.nd$receiver)) # number of receivers
ns = length(unique(aid.nd$sender))# number of senders


adjMatList = list()
for ( t in 1:length(unique(data$time))){
	 data.t = data[which(data$time == t), c( 'receiver', 'sender','value')]
	 data.adj = matrix (0, nr, ns)
	 
	 for ( i in 1:NROW(data.t)) {
		data.adj[data.t[i, 1], c(data.t[i,2] - nr )]<-data.t[i,3]}
		
		data.adj<-data.frame(data.adj)
		row.names(data.adj)<-levels(data$nameR)
		names(data.adj)<-levels(data$nameS)
		
		if (complete == FALSE){
			if(length(which(colSums(data.adj)==0))>0) {
				data.adj  = data.adj[ -c(which(rowSums(data.adj) ==0)) ,
										 -c(which(colSums(data.adj) ==0))]
		}
		}
		adjMatList[[t]] = data.adj
		}
		return(adjMatList)
}


###### Put adjacency matrix into a network #####

makeNetwork = function(M, directed){
	require(network)
	if(!is.matrix(M)) M <-as.matrix(M)
	
	n = dim(M)[1]
	 
	net = network.initialize(n, bipartite = F, directed = F)
	net = network.adjacency(M, net, names.eval = list(rownames(M), colnames(M)) )

	network.vertex.names(net) = row.names(M)
	return(net)
}


#### Tweaks DyadBuild so that row names and column names are country names, not country codes


DyadBuildNames = function(variable, dyadData, cntry1, cntry2, cntryYear = NA, time, pd, panel=panel, directed=FALSE){
		if ( is.na(cntryYear) ==T){
			panelMatch <- panel 
		 }else  { 
			panelMatch <- panel[-which(panel$cnameYear %in% setdiff(panel$cnameYear, 	
			dyadData[,cntryYear])),] }
			
	countryList <- lapply(pd, function(x) FUN=panelMatch[panelMatch$year==x,'ccode'])
	countryNames <- lapply(pd, function(x) FUN=panelMatch[panelMatch$year==x,'CNTRY_NAME'])
	names(countryList) <- pd
 head(panel)
 	Mats <- list()
	for(ii in 1:length(pd)){
	  countries <- countryList[[ii]]
	  countriesNames <-countryNames[[ii]]
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
	  rownames(yearMatrix) <- colnames(yearMatrix) <- countriesNames
	  Mats[[ii]] <- yearMatrix
	  print(pd[ii])
	}

	names(Mats) <- pd
	Mats
}



