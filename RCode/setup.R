# Clearing workspace
rm(list=ls())

# Setting working directory
if(
Sys.info()["user"]=="janus829"
){pathMain="~/Google Drive/Research/ForeignAid";
  pathGraphics="~/Google Drive/Research/ForeignAid/Graphics";
  pathData="~/Google Drive/Research/ForeignAid/Data";
  pathCode="~/Desktop/Research/ForeignAid/RCode";
  pathFunctions="~/Desktop/Prog Notes/R Functions"
  } else if  (Sys.info()["user"]=="cindycheng") {
  pathMain="~/Dropbox/ForeignAid";
  pathGraphics="~/Dropbox/ForeignAid/Graphics";
  pathData="~/Dropbox/ForeignAid/Data";
  pathCode="~/Documents/Papers/ForeignAid/RCode";
  pathFunctions="~/Documents/Methods/R Functions"} 


# Loading libraries and functions
require(foreign)
require(cshapes)
require(countrycode)

require(reshape)
require(gtools)
require(ggplot2)
theme_set(theme_bw())
require(tikzDevice)

require(doBy)
require(sbgcop)

# Helper dataset
setwd(pathData)
load('panel.rda')

# Setting seed
set.seed(6886)
setwd(pathMain)

################################################################
# Helper functions
trim = function (x) gsub("^\\s+|\\s+$", "", x)

substrRight = function(x, n){
  substr(x, nchar(x)-n+1, nchar(x)) }

num = function(x){ as.numeric(as.character(x)) }

char = function(x){as.character(x)}
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
	require(countrycode); x = as.character(x)
	y = countrycode(x, 'country.name', 'country.name') }
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
DyadBuild <- function(variable, dyadData, cntry1, cntry2, time, pd, panel=panel, directed=FALSE){

	countryList <- lapply(pd, function(x) FUN=panel[panel$year==x,'ccode'])
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
  data <- data[order(data[,country_year]),]
  lagData <- apply(data[,varsTOlag], 2, 
    function(x){
      unlist(by(x, data[,country], function(y) lagTS(y,lag) ) ) 
    } )
  colnames(lagData) <- paste('L' , varsTOlag, sep='')
  cbind(data, lagData)
}
################################################################