# Loading workspace
source('/Users/janus829/Desktop/Research/ButheProjects/ForeignAid/RCode/setup.R')
setwd(paste(pathData,'/Components',sep=''))
list.files()

## DV US bilateral foreign aid
OECDdata <- read.csv('OECD_DAC_Data.csv')

## UNDP
# HDI, share of pop: $1, $2, below local pov line, HPI, PQLI
UNDPdata <- read.csv('UNDPdata.csv')

## Loading data from WDI
# need GDP per capita, Population, regional indicator
WBgdpCap <- read.csv('NY.GDP.PCAP.CD_Indicator_MetaData_en_EXCEL.csv')
WBpop <- read.csv("SP.POP.TOTL_Indicator_MetaData_en_EXCEL.csv")
WBinflDeflator <- read.csv('NY.GDP.DEFL.KD.ZG_Indicator_MetaData_en_EXCEL.csv')
WBgdpDeflator <- read.csv('NY.GDP.DEFL.ZS_Indicator_MetaData_en_EXCEL.csv')

### Other data
# share of Christians mid 2000, from Barrett, Kurian, and Johnson
# Leaving out of dataset for now

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
# Leaving out of dataset for now

## Burnside and Dollar's good policy index
# Leaving out of dataset for now

## Polity
# regime type
polity <- read.csv('p4v2011.csv')

## Freedom House
# regime type
fhouse <- read.csv('FHdata.csv')

## Banks
# political instability, conflict index
# coups, assassinations, general strikes, guerilla warfare
# govt crises, purges, riots, revolutions, anti gov demonstrations
#= bnkv126, bnkv98, bnkv99, bnkv100, bnkv101, bnkv102, bnkv103, bnkv104, bnkv105
load('/Users/janus829/Dropbox/Research/data/4shahryar/Updated Dataset/sort22v3.rda')
vars = c("year", "sftgcode", "sftgname",
	'bnkv126', 'bnkv98', 'bnkv99', 'bnkv100', 'bnkv101', 
	'bnkv102', 'bnkv103', 'bnkv104', 'bnkv105')
banksData <- sort22v3[,vars]
names(vars) <- c("year", "sftgcode", "sftgname",
	'coups', 'assassinations', 'strikes', 'guerilla', 'govtCrises', 'purges',
	'revolutions', 'antigovDemon')

setwd(pathData)
save(OECDdata, UNDPdata,
	WBgdpCap, WBpop, WBinflDeflator, WBgdpDeflator,
	WGIregQual, WGIruleLaw, WGIcorr,
	icrg, polity, fhouse, banksData, 
	file='allData.rda')