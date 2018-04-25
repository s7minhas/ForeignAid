if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/ForeignAid/RCode/setup.R') }
if(Sys.info()["user"]=="cindycheng"){
    source('~/Documents/Papers/ForeignAid/RCode/setup.R') }


library(stringr)

#----------------------------------------------------------------- 
# load and build sipri arms transfers data

sipriFiles = list.files(paste0(pathData, '/tnsrData/sipri'))
sipriFiles = sipriFiles[grep('.csv', sipriFiles)]


dataAll = c()
for ( i in 1:length(sipriFiles)){

  # load csv
  file = read.csv(paste0(pathData, '/tnsrData/sipri/', sipriFiles[i]), stringsAsFactors = FALSE)

  # get importing country
   
  country2 = gsub('TIV.of.arms.exports.to.|..1970.2016', '',colnames(file)[1])
  country2 = gsub('\\.', ' ', country2)
  country2 = str_trim(country2)
  country2 = gsub('  ', ' ', country2)
  
  print(country2)

  # get and clean data
  data = file[11:c(which(file[,1]=='Total')-2),]
  names(data) = file[10,]
  data = data[, -c(which(names(data)== 'Total'):c(which(names(data)== 'Total')+1))]
  names(data)[1] = c('country1')
  dataLong = reshape(data,
                    varying = names(data)[-1],
                    v.names = 'armsTransfers',
                    timevar = 'year',
                    times = names(data)[-1],
                    new.row.names = NULL,
                    direction = 'long')
  dataLong$country2 = country2
  row.names(dataLong) = NULL
  dataLong = dataLong[, c('country1', 'country2', 'year', 'armsTransfers')]

   dataAll = rbind( dataAll, dataLong)
}


sipri = dataAll[-which(is.na(dataAll$armsTransfers)),]
sipri$country1 = as.character(gsub('\\(|\\)', '', sipri$country1))
sipri$country1 = as.character(gsub('\\-', ' ', sipri$country1))
sipri = sipri[-grep('\\*', sipri$country1),]


#----------------------------------------------------------------- 
# Match mid names to panel
cntries = c(sipri$country1, sipri$country2) %>% unique() %>% data.frame(cntry=.)

cntries$cname = panel$cname[match(cntries$cntry, panel$CNTRY_NAME)]
 
 
cntries[which(cntries$cntry =='Bosnia Herzegovina'), 'cname'] = "BOSNIA AND HERZEGOVINA"
cntries[which(cntries$cntry =='Cote d Ivoire'), 'cname'] = "COTE D'IVOIRE"
cntries[which(cntries$cntry =='DR Congo'), 'cname'] = "CONGO, THE DEMOCRATIC REPUBLIC OF"
cntries[which(cntries$cntry =='Germany FRG'), 'cname'] = "GERMANY"
cntries[which(cntries$cntry =='Gambia'), 'cname'] = "GAMBIA"
cntries[which(cntries$cntry =='Guinea Bissau'), 'cname'] = "GUINEA"
cntries[which(cntries$cntry =='East Germany GDR'), 'cname'] = "GERMAN DEMOCRATIC REPUBLIC"
cntries[which(cntries$cntry =='Macedonia FYROM'), 'cname'] = "MACEDONIA, THE FORMER YUGOSLAV REPUBLIC OF"
cntries[which(cntries$cntry =='Marshall Islands'), 'cname'] = "MARSHALL ISLANDS"
cntries[which(cntries$cntry =='Saint Kitts and Nevis'), 'cname'] = "SAINT KITTS AND NEVIS"
cntries[which(cntries$cntry =='Solomon Islands'), 'cname'] = "SOLOMON ISLANDS"
cntries[which(cntries$cntry =='Saint Vincent'), 'cname'] = "SAINT VINCENT AND THE GRENADINES"
cntries[which(cntries$cntry =='Taiwan ROC'), 'cname'] = "TAIWAN, PROVINCE OF CHINA"
cntries[which(cntries$cntry =='UAE'), 'cname'] = "UNITED ARAB EMIRATES"
cntries[which(cntries$cntry =='Soviet Union'), 'cname'] = "RUSSIAN FEDERATION"
cntries[which(cntries$cntry =='Viet Nam'), 'cname'] = "VIETNAM"
cntries[which(cntries$cntry =='South Vietnam'), 'cname'] = "S. VIETNAM"
cntries[which(cntries$cntry =='North Yemen'), 'cname'] = "YEMEN"
cntries[which(cntries$cntry =='South Yemen'), 'cname'] = "S. YEMEN"

 
cntries = cntries[-which(is.na(cntries$cname)),]
 
# Add ccode
cntries$ccode = panel$ccode[match(cntries$cname,panel$cname)]


# check
cntries[which(is.na(cntries$ccode)),]

# Merge updated cname and ccode to sipri
sipri$cname1 = cntries$cname[match(sipri$country1, cntries$cntry)]
sipri$cname2 = cntries$cname[match(sipri$country2, cntries$cntry)]
sipri$ccode1 = cntries$ccode[match(sipri$country1, cntries$cntry)]
sipri$ccode2 = cntries$ccode[match(sipri$country2, cntries$cntry)]

sipri = sipri[-c(unique(c(which(is.na(sipri$ccode1)), which(is.na(sipri$ccode2))))),]

 


#-----------------------------------------------------------------
# Make weighted  data

# get pop and gdp data
library(wbstats)
pop <- wb(indicator = 'SP.POP.TOTL', startdate = 1970, enddate = 2016)
names(pop)[3] = 'pop'
gdp<- wb(indicator = 'NY.GDP.MKTP.KD', startdate = 1970, enddate = 2016)
names(gdp)[3] = 'gdp'

wdi = merge(pop[, c('pop', 'date', 'country')], gdp[, c('gdp', 'date', 'country')], by = c('date', 'country'), all = TRUE)
names(wdi)[1] = 'year'
wdi$cname = cname(wdi$country)

# check
wdi[which(is.na(wdi$cname)), 'country'] %>% unique()

# remove non-countries
wdi = wdi[-which(is.na(wdi$cname)),]

# merge with sipri data
sipri = merge(sipri, wdi, by.x = c('cname1', 'year'), by.y = c('cname', 'year'), all.x = TRUE)
sipri$armsPop = c(sipri$armsTransfers * 1000000)/sipri$pop
sipri$armsGdp = c(sipri$armsTransfers * 1000000)/sipri$gdp

 
armsTrsfrsL = convToList(sipri, yrs, 'year', c('ccode1','ccode2'), 'armsTransfersRev', standardize=FALSE, addDyadLabel=TRUE)
armsPopL = convToList(sipri, yrs, 'year', c('ccode1','ccode2'), 'armsPopRev', standardize=FALSE, addDyadLabel=TRUE)
armsGdpL = convToList(sipri, yrs, 'year', c('ccode1','ccode2'), 'armsGdpRev', standardize=FALSE, addDyadLabel=TRUE)
      
#-----------------------------------------------------------------

save(sipri,
 armsTrsfrsL,armsPopL,armsGdpL,
  file=paste0(pathTnsr, 'sipri.rda'))

 