# Loading workspace
source('/Users/janus829/Desktop/Research/ButheProjects/ForeignAid/RCode/setup.R')
setwd(pathData)

## Loading data from WDI
# need GDP per capita, Population, regional indicator
setwd(paste(pathData,'/Components',sep=''))
list.files()
WBgdpCap <- read.csv('NY.GDP.PCAP.CD_Indicator_MetaData_en_EXCEL.csv')
WBpop <- read.csv("SP.POP.TOTL_Indicator_MetaData_en_EXCEL.csv")


### Other data
# share of Christians mid 2000, from Barrett, Kurian, and Johnson


## UNDP
# HDI, share of pop: $1, $2, below local pov line, HPI, PQLI



## WGI
# reg quality, control of corruption, rule of law
WGIregQual <- read.csv('wgi_regQual.csv')
WGIruleLaw <- read.csv('wgi_ruleLaw.csv')
WGIcorr <- read.csv('wgi_corr.csv')


## ICRG
# corruption, rule of law, bureaucratic quality
icrg <- read.csv('PRS_Melted_Format.csv')


## Transparency index
# corruption perceptions index



## Burnside and Dollar's good policy index



## Polity
# regime type
polity <- read.csv('p4v2011.csv')


## Freedom House
# regime type
fhouse <- read.csv('FHdata.csv')


## Banks
# political instability, conflict index

