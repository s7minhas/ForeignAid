if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
    source('~/Research/ForeignAid/RCode/setup.R') }

if(Sys.info()["user"]=="cindycheng"){
    source('~/Dropbox/Documents/Papers/ForeignAid/RCode/setup.R') }


#install.packages("~/Downloads/countrycode_0.16.tar", repos = NULL, type="source")
############################
# Download file from ICOW site
igoURL = 'http://www.correlatesofwar.org/data-sets/IGOs/IGO_dyadunit_stata_v2.3.zip/at_download/file'
igoName = paste0(pathTnsr, 'igo.zip')
if(!file.exists(igoName)) { download.file(igoURL, igoName) }

igo = unzip(igoName, 
	'IGO_dyadunit_stata_v2.3.dta') %>% read.dta()

file.remove(paste0(getwd(), 'IGO_dyadunit_stata_v2.3.dta'))
unlink(paste0(getwd(), 'version4.1_dta'), recursive=TRUE, force=TRUE)
############################

############################
# Match igo names to panel
igo$ccode1=num(igo$ccode1)
igo$ccode2=num(igo$ccode2)
 
ctyNameA=countrycode(igo$ccode1, "cown", "country.name")
ctyNameB=countrycode(igo$ccode2, "cown", "country.name")

sancIDs=data.frame(unique(cbind(igo$ccode1, igo$ccode2, ctyNameA, ctyNameB)))
 
sancIDs$V1= num(sancIDs$V1)
sancIDs$V2 = num(sancIDs$V2)
sancIDs$ctyNameA =char(sancIDs$ctyNameA)
sancIDs$ctyNameB =char(sancIDs$ctyNameB)

 
#fix time
sancIDs[sancIDs$V1==2,'ctyNameA'] = 'UNITED STATES' 
sancIDs[sancIDs$V2==2,'ctyNameB'] = 'UNITED STATES' 
sancIDs[sancIDs$V1==145,'ctyNameA'] = "BOLIVIA, PLURINATIONAL STATE OF"  
sancIDs[sancIDs$V2==145,'ctyNameB'] = "BOLIVIA, PLURINATIONAL STATE OF"  
sancIDs[sancIDs$V1==200,'ctyNameA'] = 'UNITED KINGDOM'
sancIDs[sancIDs$V2==200,'ctyNameB'] = 'UNITED KINGDOM'
sancIDs[sancIDs$V1==260,'ctyNameA'] = 'GERMANY'
sancIDs[sancIDs$V2==260,'ctyNameB'] = 'GERMANY'
sancIDs[sancIDs$V1==343,'ctyNameA'] = "MACEDONIA, THE FORMER YUGOSLAV REPUBLIC OF"
sancIDs[sancIDs$V2==343,'ctyNameB'] = "MACEDONIA, THE FORMER YUGOSLAV REPUBLIC OF"
sancIDs[sancIDs$V1==359,'ctyNameA'] = "MOLDOVA, REPUBLIC OF"
sancIDs[sancIDs$V2==359,'ctyNameB'] = "MOLDOVA, REPUBLIC OF"
sancIDs[sancIDs$V1==402,'ctyNameA'] = "CAPE VERDE"
sancIDs[sancIDs$V2==402,'ctyNameB'] = "CAPE VERDE"
sancIDs[sancIDs$V1==404,'ctyNameA'] = "GUINEA-BISSAU"
sancIDs[sancIDs$V2==404,'ctyNameB'] = "GUINEA-BISSAU"
sancIDs[sancIDs$V1==420,'ctyNameA'] = "GAMBIA"
sancIDs[sancIDs$V2==420,'ctyNameB'] = "GAMBIA"
sancIDs[sancIDs$V1==437,'ctyNameA'] = "COTE D'IVOIRE"
sancIDs[sancIDs$V2==437,'ctyNameB'] = "COTE D'IVOIRE"
sancIDs[sancIDs$V1==484,'ctyNameA'] = "CONGO, REPUBLIC OF"
sancIDs[sancIDs$V2==484,'ctyNameB'] = "CONGO, REPUBLIC OF"
sancIDs[sancIDs$V1==490,'ctyNameA'] = "CONGO, THE DEMOCRATIC REPUBLIC OF"
sancIDs[sancIDs$V2==490,'ctyNameB'] = "CONGO, THE DEMOCRATIC REPUBLIC OF"
sancIDs[sancIDs$V1==510,'ctyNameA'] = "TANZANIA, UNITED REPUBLIC OF"
sancIDs[sancIDs$V2==510,'ctyNameB'] = "TANZANIA, UNITED REPUBLIC OF"
sancIDs[sancIDs$V1==620,'ctyNameA'] = "LIBYAN ARAB JAMAHIRIYA"
sancIDs[sancIDs$V2==620,'ctyNameB'] = "LIBYAN ARAB JAMAHIRIYA"
sancIDs[sancIDs$V1==630,'ctyNameA'] = "IRAN, ISLAMIC REPUBLIC OF"
sancIDs[sancIDs$V2==630,'ctyNameB'] = "IRAN, ISLAMIC REPUBLIC OF"
sancIDs[sancIDs$V1==732,'ctyNameA'] = "KOREA, REPUBLIC OF"
sancIDs[sancIDs$V2==732,'ctyNameB'] = "KOREA, REPUBLIC OF"
sancIDs[sancIDs$V1==731,'ctyNameA'] = "KOREA, DEMOCRATIC PEOPLE'S REPUBLIC OF"
sancIDs[sancIDs$V2==731,'ctyNameB'] = "KOREA, DEMOCRATIC PEOPLE'S REPUBLIC OF"
sancIDs[sancIDs$V1==678,'ctyNameA'] = 'YEMEN'
sancIDs[sancIDs$V2==678,'ctyNameB'] = 'YEMEN'
sancIDs[sancIDs$V1==680,'ctyNameA'] = 'S. YEMEN' 
sancIDs[sancIDs$V2==680,'ctyNameB'] = 'S. YEMEN' 
sancIDs[sancIDs$V1==816,'ctyNameA'] = 'VIETNAM'
sancIDs[sancIDs$V2==816,'ctyNameB'] = 'VIETNAM'
sancIDs[sancIDs$V1==817,'ctyNameA'] = 'S. VIETNAM'
sancIDs[sancIDs$V2==817,'ctyNameB'] = 'S. VIETNAM'
sancIDs[sancIDs$V1==987,'ctyNameA'] = 'MICRONESIA, FEDERATED STATES OF'
sancIDs[sancIDs$V2==987,'ctyNameB'] = 'MICRONESIA, FEDERATED STATES OF'
sancIDs[sancIDs$V1==345,'ctyNameA'] = 'SERBIA'
sancIDs[sancIDs$V2==345,'ctyNameB'] = 'SERBIA'
sancIDs[sancIDs$V1==315,'ctyNameA'] = 'CZECH REPUBLIC'
sancIDs[sancIDs$V2==315,'ctyNameB'] = 'CZECH REPUBLIC'
sancIDs[sancIDs$V1==730,'ctyNameA'] = "KOREA, DEMOCRATIC PEOPLE'S REPUBLIC OF"
sancIDs[sancIDs$V2==730,'ctyNameB'] = "KOREA, DEMOCRATIC PEOPLE'S REPUBLIC OF"

sancIDs2 = unique(
    data.frame(cbind(
            rbind(cowcode=t(t(sancIDs[,c(1)])), cowcode=t(t(sancIDs[,c(2)]))),
            rbind(country=t(t(sancIDs[,c(3)])), country=t(t(sancIDs[,c(4)]))) ) ) )
names(sancIDs2) = c('cowcode', 'country')

sancIDs2$cowcode = num(sancIDs2$cowcode)
sancIDs2$country = toupper(char(sancIDs2$country))

sancIDs2$country = toupper(char(sancIDs2$country))
sancIDs[sancIDs$V1==265,'ctyNameA'] = 'Germany Democratic Republic'
sancIDs[sancIDs$V2==265,'ctyNameB'] = 'Germany Democratic Republic'
 
# Add in the data from the panel
sancIDs2$ccode = panel$ccode[match(sancIDs2$country, panel$cname)]
sancIDs2$cname = panel$cname[match(sancIDs2$country, panel$cname)]

sancIDs2[is.na(sancIDs2$ccode),]    # Checks for NAs
sancIDs2[is.na(sancIDs2$cname),] 

# Add back into igo
igo2 = igo
names(igo2)[1] = 'cowcode1'
names(igo2)[3] = 'cowcode2'

igo2$ccode_1 = sancIDs2$ccode[match(igo2$cowcode1, sancIDs2$cowcode)]
igo2$ccode_2 = sancIDs2$ccode[match(igo2$cowcode2, sancIDs2$cowcode)]

 
igo2$cname_1 = sancIDs2$cname[match(igo2$cowcode1, sancIDs2$cowcode)]
igo2$cname_2 = sancIDs2$cname[match(igo2$cowcode2, sancIDs2$cowcode)]

# Finalize IGO dataset
igoFINAL = igo2
igoFINAL = igoFINAL[igoFINAL$year>=1960,c(534:535,5,6:533)]
igoFINAL = data.matrix(igoFINAL)




igoOrgs = read.csv(paste0(pathTnsr, '/igounit_v2.3.csv'))

# create measure of total number of IGO *members* over time
igoOrgs$members = apply(igoOrgs[, -c(1,2,3, which(names(igoOrgs) ==c('dead')):which(names(igoOrgs) ==c('Sources2')))],
                 1, function(x){
    if(all(x == -9)){
        members= NA} 
    else {
    x[which(x %in% c(-1, -9))] = NA
    members = sum(ifelse(x==1, 1, 0), na.rm = TRUE)}# note that even though codebook says will code for associate member/observer status, appears that these categories don't exist in the dataset

return(members)
    })

 
 
# create measure of total number of IGOs over time
library(dplyr)
igoOrgs$count  = apply(igoOrgs[, -c(1,2,3, which(names(igoOrgs) ==c('dead')):which(names(igoOrgs) ==c('Sources2')))],
                 1, function(x){
    count = ifelse(all(x == -9), NA, 1)
    #igo = select(x, c(year, count)) %>% group_by(year) %>% summarise(count = sum(count, na.rm = TRUE))
    return(count)
    })

igoCount = data.frame(igoOrgs %>% group_by(year) %>% summarise(numIGOs = sum(count, na.rm = TRUE)) )

igoOrgsFINAL = merge(igoOrgs, igoCount, by = 'year', all = TRUE)
igoOrgsFINAL =igoOrgsFINAL[-which(igoOrgsFINAL$ioname == "CAEC" ),]
igoOrgsFINAL$ioname = factor(igoOrgsFINAL$ioname)
 
#igoOrgsRaw$avgNumMembers = igoOrgsRaw$members/igoOrgsRaw$numIGOs
# Set all igo codes of 3, -9, and -1 for IGO membership
## to 0 and for igo codes of 1 and 2 set to 1
drop = c(3, -9, -1, 0)
years = c(1960,1965:2005)
 


igoData= lapply(1:length(years), function(ii){
    slice = igoFINAL[which(igoFINAL[,'year']==years[ii]),]
    sList = lapply(4:ncol(slice), function(x) FUN=slice[,c(1:3,x)])
    sList2 = lapply(sList, function(x) FUN= matrix(x[which(!x[,4] %in% drop),], ncol = 4))

    # # calculate weights
    sliceOrg = igoOrgsFINAL[which(igoOrgsFINAL[, 'year'] == years[ii]),c('year', 'ioname', 'members')]
    sListOrg = split(sliceOrg$members, sliceOrg$ioname)
    sListOrgInv = lapply(sListOrg, function(x) 1/x)

    # apply weights to joint membership
    jointMembershipPerYear = lapply(sList2, function(x) {
         x[,4]})
    weightedList =   Map('*', jointMembershipPerYear,  sListOrgInv)
 
    # # # add weighted joint membership back into main data
    sList3= Map(cbind, sList2, weightedList)

    ## add back names to matrices
    sList3 = lapply(1:length(sList3), function(x) {
        mainMat = sList3[[x]]
        nameMat = sList[[x]]
        colnames(mainMat) = c(colnames(nameMat), paste0( colnames(nameMat)[4], '_', 'weighted'))
        return(mainMat)
        } )

    # recompile and aggregate unweighted and weighted joint membership
    sList4 = sList3[which(num(summary(sList3)[,1])>0)]
    sList5 = lapply(sList4, function(x){
        temp = matrix(x, ncol=5)
        temp2 = data.frame( matrix(c(paste0(temp[,1], '_', temp[,2]), temp[,3:5]), ncol = 4))
        return(temp2)})

    sUnList5 = do.call(rbind, sList5)
    sUnList5$X2 =  as.numeric(as.character(sUnList5$X2)); sUnList5$X3 =  as.numeric(as.character(sUnList5$X3)); sUnList5$X4 =  as.numeric(as.character(sUnList5$X4))
    
    yearIGOs = data.frame(sUnList5 %>% group_by(X1) %>% summarise(year = mean(X2), igo = sum(X3), igoWeighted = sum(X4)) )

    return( yearIGOs)})

 
 
# Combine
igoData = do.call('rbind', igoData)

 
# Cleaning
igoDataFINAL = data.frame( igoData, row.names=NULL)
colnames(igoDataFINAL)[1] = c('ccodes')
ccodes = matrix(
    unlist(strsplit(char(igoDataFINAL[,'ccodes']), '_')) 
    ,ncol=2,byrow=T)
colnames(ccodes) = c('ccode1','ccode2')
igoDataFINAL = cbind(ccodes, igoDataFINAL[,c('year','igo', 'igoWeighted')])
igo = data.frame(apply(igoDataFINAL,2,num))

head(igo) 
############################

############################
# Convert unweighted igos to list object
yrs = 1965:2005
igoL = convToList(igo, yrs, 'year', c('ccode1','ccode2'), 'igo',standardize=FALSE)

# Undirected so repeat observations 
igoL = lapply(igoL, function(l){
    l = igoL[[1]]
    revL = l[,c('ccode2','ccode1','igo')] 
    names(revL)[1:2] = paste0('ccode',1:2)
    l = rbind(l, revL)
    return(l)
})
igoL = lapply(igoL, function(x){ x$ij = paste(x$ccode1, x$ccode2, sep='_') ; return(x) })
 
 
igoWL = convToList(igo, yrs, 'year', c('ccode1','ccode2'), 'igoWeighted',standardize=FALSE)
 
# Undirected so repeat observations 
igoWL = lapply(igoWL, function(l){
    l = igoWL[[1]]
    revL = l[,c('ccode2','ccode1','igoWeighted')] 
    names(revL)[1:2] = paste0('ccode',1:2)
    l = rbind(l, revL)
    return(l)
})
igoWL = lapply(igoWL, function(x){ x$ij = paste(x$ccode1, x$ccode2, sep='_') ; return(x) })
 
############################

############################
# Save
save(igo, igoL, igoWL, file=paste0(pathTnsr,'igo.rda'))
 
############################