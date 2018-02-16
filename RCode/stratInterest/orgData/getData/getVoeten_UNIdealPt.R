if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/ForeignAid/RCode/setup.R') }
if(Sys.info()["user"]=="cindycheng"){
    source('~/Dropbox/Documents/Papers/ForeignAid/RCode/setup.R') }

###############################################################
# Downloaded data manually from: https://dataverse.harvard.edu/dataset.xhtml?persistentId=hdl:1902.1/12379
## on July 7, 2015: 5pm
load(paste0(pathTnsr,'Voeten/Idealpointsdyadicdistance_1.RData')); un=x ; rm(list='x')
 
###############################################################

###############################################################
# Match UN names to panel

 
cntries = c(un$countryname1, un$countryname2) %>% char() %>% unique() %>% data.frame(cntry=.)
cntries$cname = cname(cntries$cntry)

# Fix few cnames issue so it matches with panel
cntries$cname[cntries$cntry=="Yemen People's Republic"] = 'S. YEMEN'
cntries$cname[cntries$cntry=="Yemen PDR (South)"] = 'S. YEMEN'
cntries$cname[cntries$cntry=="Yugoslavia"] = 'SERBIA'
cntries$cname[cntries$cntry=="Czechoslovakia"] = 'CZECH REPUBLIC'
cntries$cname[cntries$cntry=="Germany, East"] = 'German Democratic Republic'


cntries$cname[cntries$cntry=="United States of America"] = 'UNITED STATES' 
cntries$cname[cntries$cntry=="Bolivia"] = "BOLIVIA, PLURINATIONAL STATE OF"  
cntries$cname[cntries$cntry=="United Kingdom"] = 'UNITED KINGDOM'
cntries$cname[cntries$cntry=="German Federal Republic"] = 'GERMANY'
cntries$cname[cntries$cntry=="Macedonia"] = "MACEDONIA, THE FORMER YUGOSLAV REPUBLIC OF"
cntries$cname[cntries$cntry=="Moldova"] = "MOLDOVA, REPUBLIC OF"
cntries$cname[cntries$cntry=="Cape Verde"] = "CAPE VERDE"
cntries$cname[cntries$cntry=="Guinea-Bissau"] = "GUINEA-BISSAU"
cntries$cname[cntries$cntry=="Gambia"] = "GAMBIA"
cntries$cname[cntries$cntry=="Ivory Coast"] = "COTE D'IVOIRE"
cntries$cname[cntries$cntry=="Congo"] = "CONGO, REPUBLIC OF"
cntries$cname[cntries$cntry=="Democratic Republic of the Congo"] = "CONGO, THE DEMOCRATIC REPUBLIC OF"
cntries$cname[cntries$cntry=="Tanzania"] = "TANZANIA, UNITED REPUBLIC OF"
cntries$cname[cntries$cntry=="Libya"] = "LIBYAN ARAB JAMAHIRIYA"
cntries$cname[cntries$cntry=="Iran"] = "IRAN, ISLAMIC REPUBLIC OF"
cntries$cname[cntries$cntry=="South Korea"] = "KOREA, REPUBLIC OF"
cntries$cname[cntries$cntry=="North Korea"] = "KOREA, DEMOCRATIC PEOPLE'S REPUBLIC OF"
cntries$cname[cntries$cntry=="Yemen Arab Republic"] = 'YEMEN'
cntries$cname[cntries$cntry=="Vietnam"] = 'VIETNAM'
cntries$cname[cntries$cntry=="Federated States of Micronesia"] = 'MICRONESIA, FEDERATED STATES OF'


# Add ccode
cntries$ccode = panel$ccode[match(cntries$cname,panel$cname)]
cntries[which(cntries$ccode == 971),]


# cowcode2 indicates that Nauru mislabeled as Kiribati
un$countryname2[which(un$countryname2 == 'Nauru' & un$cowcode2 == 946 )] = 'Kiribati'
un$countryname1[which(un$countryname1 == 'Nauru' & un$cowcode1 == 946 )] = 'Kiribati'

 

# Merge updated cname and ccode to un
un$cname1 = cntries$cname[match(un$countryname1, cntries$cntry)]
un$cname2 = cntries$cname[match(un$countryname2, cntries$cntry)]
un$ccode1 = cntries$ccode[match(un$countryname1, cntries$cntry)]
un$ccode2 = cntries$ccode[match(un$countryname2, cntries$cntry)]

length(which(table(un$dyadidyr)>1))
# Check for duplicates
un$dyadidyr = paste(un$ccode1, un$ccode2, un$year, sep='_')
 
stopifnot( length( table(un$dyadidyr)[table(un$dyadidyr)>1] ) == 0 )


un[which(un$ccode1 == 2 & un$year == 1946),][1:10,]
  ###############################################################

###############################################################
# use UN data to set nodes for dyadic dataset
yrs = un$year %>% unique() %>% sort()
unFrame = lapply(yrs, function(yr){
	slice = un[un$year==yr,c('ccode1','ccode2')]
	cntries = c(slice$ccode1,slice$ccode2) %>% unique() %>% num()
	frYr = expand.grid(i=cntries, j=cntries)
	frYr = frYr[frYr$i != frYr$j,]
	frYr$ij = paste(frYr$i, frYr$j, sep='_')
	frYr$ji = paste(frYr$j, frYr$i, sep='_')	
	frYr = cbind(frYr, t=yr)
	return(frYr)
	})
names(unFrame) = yrs


###############################################################

###############################################################
 
# Convert into a list format for object per year
yrs = un$year %>% unique() %>% sort()
idealPtDistL = convToList(un, yrs, 'year', c('ccode1','ccode2'), 'idealpointdistance', standardize=FALSE)

 

# Turn undirected un data into complete frame
idealPtDistfull = lapply(idealPtDistL, function(unSl){
	unDat=data.frame( rbind(
		cbind( ij=paste(unSl$ccode1, unSl$ccode2, sep='_'), unSl ),
		cbind( ij=paste(unSl$ccode2, unSl$ccode1, sep='_'), unSl ) ) )
	unDat[,4] = num( unDat[,4] )
	return(unDat)
})

 
###############################################################

###############################################################
# Save
save(idealPtDistfull, file=paste0(pathTnsr, 'unIdealPt.rda'))
 
###############################################################