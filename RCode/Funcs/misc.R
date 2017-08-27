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