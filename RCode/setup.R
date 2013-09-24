# Clearing workspace
rm(list=ls())

# Setting working directory
if(
Sys.info()["user"]=="janus829"
){pathMain="~/Dropbox/Research/ButheProjects/ForeignAid";
  pathGraphics="~/Dropbox/Research/ButheProjects/ForeignAid/Graphics";
  pathData="~/Dropbox/Research/ButheProjects/ForeignAid/Data";
  pathCode="~/Desktop/Research/ButheProjects/ForeignAid/RCode";
  pathFunctions="~/Desktop/Prog Notes/R Functions"}

# Loading libraries and functions
library(foreign)
library(WDI)
library(cshapes)
library(countrycode)
library(plm)
library(MASS)
library(mvtnorm)
library(reshape)
library(gtools)
library(ggplot2)
theme_set(theme_bw())
library(scales)
library(gridExtra)
library(plyr)
library(tikzDevice)
setwd(pathFunctions)
source("rwish.R")
source("theme_border.R")
library(lme4)
library(arm)
# source("hdr_2d.R")
# source("scatterHist.R")

# Helper dataser
load('~/Desktop/Research/BuildingPanelData/panel.rda')

# Setting seed
set.seed(6886)
setwd(pathMain)

# Helper functions
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

# Fx for Melting/Cleaning WB Data for Merge
cleanWbData <- function(data, variable){
	var <- variable
	mdata <- melt(data, id='Country')
	names(mdata)[3] <- var
	mdata$year <-  numSM(mdata$variable)
	mdata <- mdata[,c(1,3,4)]

	# Remove non-country observations and small islands/territories
	drop <- c('Arab World', 'Caribbean small states', 
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
	mdata <- mdata[which(!mdata$Country %in% drop),]

	# Setting standardized countryname for WB data
	mdata$Country <- as.character(mdata$Country)
	mdata$Country[mdata$Country=='Korea, Dem. Rep.'] <- 'North Korea' 
	mdata$Country[mdata$Country=='Korea, Rep.'] <- 'South Korea' 
	mdata$cname <- countrycode(mdata$Country, 'country.name', 'country.name')
	mdata$cnameYear <- paste(mdata$cname, mdata$year, sep='')

	# Adding in codes from panel
	mdata$ccode <- panel$ccode[match(mdata$cname,panel$cname)]
	mdata$cyear <- paste(mdata$ccode, mdata$year, sep='')
	mdata }