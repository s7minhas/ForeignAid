if(Sys.info()['user']=='s7m' | Sys.info()['user']=='janus829'){
    source('~/Research/ForeignAid/RCode/setup.R') }
if(Sys.info()['user']=='cindycheng'){
    pathCode = '~/Documents/Papers/ForeignAid/RCode' ; source(paste0(pathCode, '/setup.R'))}

###############################################################
# Clean alliance data [extends from 1816 to 2012]
setwd(paste0(pathData, '/Components/COW_Alliances/version4.1_stata'))
alliance = read.dta('alliance_v4.1_by_directed_yearly.dta')
alliance$ccode1=num(alliance$ccode1)
alliance$ccode2=num(alliance$ccode2)

# clean country names
ctyNameA=toupper(countrycode(alliance$ccode1, "cown", "country.name"))
ctyNameB=toupper(countrycode(alliance$ccode2, "cown", "country.name"))

sancIDs = data.frame( cowcode = intersect(alliance$ccode1, alliance$ccode2),
    country = toupper(
        countrycode(intersect(alliance$ccode1, alliance$ccode2), "cown", "country.name")
        ), 
    stringsAsFactors = F)

#fix time
sancIDs[sancIDs$cowcode==245,'country'] = 'BAVARIA'
sancIDs[sancIDs$cowcode==267,'country'] = 'BADEN'
sancIDs[sancIDs$cowcode==300,'country'] = 'AUSTRIA-HUNGARY'
sancIDs[sancIDs$cowcode==730,'country'] = "KOREA, DEMOCRATIC PEOPLE'S REPUBLIC OF"
sancIDs[sancIDs$cowcode==731,'country'] = "KOREA, DEMOCRATIC PEOPLE'S REPUBLIC OF"
sancIDs[sancIDs$cowcode==678,'country'] = 'YEMEN'
sancIDs[sancIDs$cowcode==680,'country'] = 'S. YEMEN' 
sancIDs[sancIDs$cowcode==817,'country'] = 'S. VIETNAM'
sancIDs[sancIDs$cowcode==260,'country'] = 'GERMANY'
sancIDs[sancIDs$cowcode==345,'country'] = 'SERBIA'
sancIDs[sancIDs$cowcode==315,'country'] = 'CZECH REPUBLIC'

# Add in the data from the panel
sancIDs$ccode = panel$ccode[match(sancIDs$country, panel$cname)]
sancIDs$cname = panel$cname[match(sancIDs$country, panel$cname)]

sancIDs[is.na(sancIDs$ccode),]    # Checks for NAs
sancIDs[is.na(sancIDs$cname),] 

# weight alliances
alliance[, 'defense'] = alliance[, 'defense']*3
alliance[, 'neutrality'] = alliance[, 'neutrality']*2
alliance[, 'nonaggression'] = alliance[, 'nonaggression']*2
alliance$allyWtSum = rowSums(alliance[, c('defense', 'nonaggression', 'neutrality', 'entente')], na.rm = T)
alliance$allyWtMax = apply(alliance[, c('defense', 'nonaggression', 'neutrality', 'entente')], 1, max, na.rm = T)
 
# Add back to alliance
alliance2 = alliance[,c('ccode1', 'ccode2', 'state_name1', 'state_name2','year', 'allyWtMax', 'allyWtSum')]
colnames(alliance2)[1:2] = c('cowcode1', 'cowcode2')
 
alliance2$ccode_1 = sancIDs$ccode[match(alliance2$cowcode1, sancIDs$cowcode)]
alliance2$ccode_2 = sancIDs$ccode[match(alliance2$cowcode2, sancIDs$cowcode)]

alliance2$cname_1 = sancIDs$cname[match(alliance2$cowcode1, sancIDs$cowcode)]
alliance2$cname_2 = sancIDs$cname[match(alliance2$cowcode2, sancIDs$cowcode)]

allianceFINAL = na.omit(alliance2)
allianceFINAL$ally = 1
 
save(allianceFINAL, file='ally.rda')
###############################################################

###############################################################
# Clean IGO data [extends from 1820 to 2005]
igo = read.dta(paste0(pathData, '/Components/COW_IGO/IGO_dyadunit_stata_v2.3.dta'))
 
igo$ccode1=num(igo$ccode1)
igo$ccode2=num(igo$ccode2)

ctyNameA=toupper(countrycode(igo$ccode1, "cown", "country.name"))
ctyNameB=toupper(countrycode(igo$ccode2, "cown", "country.name"))

sancIDs=data.frame(unique(cbind(igo$ccode1, igo$ccode2, ctyNameA, ctyNameB)))

sancIDs$V1= num(sancIDs$V1)
sancIDs$V2 = num(sancIDs$V2)
sancIDs$ctyNameA =as.character(sancIDs$ctyNameA)
sancIDs$ctyNameB =as.character(sancIDs$ctyNameB)

#fix time
sancIDs[sancIDs$V1==260,'ctyNameA'] = 'GERMANY'
sancIDs[sancIDs$V2==260,'ctyNameB'] = 'GERMANY'
sancIDs[sancIDs$V1==731,'ctyNameA'] = "KOREA, DEMOCRATIC PEOPLE'S REPUBLIC OF"
sancIDs[sancIDs$V2==731,'ctyNameB'] = "KOREA, DEMOCRATIC PEOPLE'S REPUBLIC OF"
sancIDs[sancIDs$V1==678,'ctyNameA'] = 'YEMEN'
sancIDs[sancIDs$V2==678,'ctyNameB'] = 'YEMEN'
sancIDs[sancIDs$V1==680,'ctyNameA'] = 'S. YEMEN' 
sancIDs[sancIDs$V2==680,'ctyNameB'] = 'S. YEMEN' 
sancIDs[sancIDs$V1==817,'ctyNameA'] = 'S. VIETNAM'
sancIDs[sancIDs$V2==817,'ctyNameB'] = 'S. VIETNAM'
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
sancIDs2$country = as.character(sancIDs2$country)

# Add in the data from the panel
sancIDs2$ccode = panel$ccode[match(sancIDs2$country, panel$cname)]
sancIDs2$cname = panel$cname[match(sancIDs2$country, panel$cname)]

sancIDs2[is.na(sancIDs2$ccode),]    # Checks for NAs
sancIDs2[is.na(sancIDs2$cname),] 

# Add back into igo
names(igo)[1] = 'cowcode1'
names(igo)[3] = 'cowcode2'

igo$ccode_1 = sancIDs2$ccode[match(igo$cowcode1, sancIDs2$cowcode)]
igo$ccode_2 = sancIDs2$ccode[match(igo$cowcode2, sancIDs2$cowcode)]

igo$cname_1 = sancIDs2$cname[match(igo$cowcode1, sancIDs2$cowcode)]
igo$cname_2 = sancIDs2$cname[match(igo$cowcode2, sancIDs2$cowcode)]

# Finalize IGO dataset
igoFINAL = igo[! (is.na(igo$ccode_1) | is.na(igo$ccode_2)),]
igoFINAL = igoFINAL[igoFINAL$year>=1960,c(534:535,5,6:533)]
igoFINAL = data.matrix(igoFINAL)

# Set all igo codes of 3, -9, and -1 for IGO membership
## to 0 and for igo codes of 1 and 2 set to 1
drop = c(3, -9, -1, 0)
years = c(1960,1965:2005)
igoData = NULL
for(ii in 1:length(years)){
    slice = igoFINAL[which(igoFINAL[,'year']==years[ii]),]
    sList = lapply(4:ncol(slice), function(x) FUN=slice[,c(1:3,x)])
    sList2 = lapply(sList, function(x) FUN=x[which(!x[,4] %in% drop),])
    sList3 = sList2[which(num(summary(sList2)[,1])>0)]

    sList4 = lapply(sList3, function(x){
        temp = matrix(x, ncol=4); paste(temp[,1],temp[,2],sep='_') })
    yearIGOs = t(t(table( unlist(sList4) )))
    yearIGOs = cbind(yearIGOs, year=years[ii])
    igoData = rbind(igoData, yearIGOs)
    print(years[ii])
}

# Cleaning
igoDataFINAL = data.frame(cbind(rownames(igoData), igoData), row.names=NULL)
colnames(igoDataFINAL) = c('ccodes', 'igo', 'year')
ccodes = matrix(
    unlist(strsplit(as.character(igoDataFINAL[,'ccodes']), '_')) 
    ,ncol=2,byrow=T)
colnames(ccodes) = c('ccode_1','ccode_2')
igoDataFINAL = cbind(ccodes, igoDataFINAL[,c('year','igo')])
igoDataFINAL = data.frame(apply(igoDataFINAL,2,num))

save(igoDataFINAL, file='igo.rda')
###############################################################

###############################################################
# Clean UN data
# vote – Vote choice 
# 1 – Yes 
# 2 – Abstain 
# 3 – No 
# 8 – Absent 
# 9 – Not a member 

#### Make new measures from the raw data
 
# load data
unData = read.table(paste0(pathData, '/Components/VoetenData/Raw Data/undata-213.tab'), 
    sep='\t', stringsAsFactors=FALSE, header=TRUE)
unData$uniquename = as.character(unData$uniquename)

# Create year - roll call variable
unData$yrRcid = paste(substring(unData$date, 1, 4), unData$rcid, sep ="_")

# Subset the data to only abstain votes, only yes votes, only no votes
unAbstain = unData[which(unData$vote ==2), c('yrRcid', 'vote', 'ccode', 'uniquename')]
unYes = unData[which(unData$vote ==1), c('yrRcid', 'vote', 'ccode', 'uniquename')]
unNo = unData[which(unData$vote ==3), c('yrRcid', 'vote', 'ccode', 'uniquename')]
unJoint = unData[-which(unData$vote ==8 | unData$vote ==9), c('yrRcid', 'vote', 'ccode', 'uniquename')]

# Create dyad pairs for countries that both agree on a resolution
AbstainDyad = makeDyad(unAbstain, 'yrRcid')
YesDyad = makeDyad(unYes, 'yrRcid')
NoDyad = makeDyad(unNo, 'yrRcid')
JointDyad = makeDyad(unJoint, 'yrRcid')
          
# make unique dyad ID
countries = unique(unData$uniquename)
dyadAll = expand.grid(countries, countries)  
dyadAll$Var1 = as.character(dyadAll$Var1); dyadAll$Var2 = as.character(dyadAll$Var2)
dyadAll= dyadAll[-which(dyadAll[,1] == dyadAll[,2]),]
dyadAll$dname = paste(dyadAll$Var1, dyadAll$Var2, sep = "_")

dyadAll$dyadID = 0
for ( i in 1:dim(dyadAll)[1] ) {
if(dyadAll$dyadID[i] ==0){   
dyadAll$dyadID[which(dyadAll$Var1 %in% c(dyadAll[i,]) & dyadAll$Var2 %in% c(dyadAll[i,]))] = i
}
}

# Add unique dyad ID to Agree dyadic dataset
AbstainDyad$dyadID = dyadAll$dyadID[match(AbstainDyad$dname, dyadAll$dname)]
YesDyad$dyadID = dyadAll$dyadID[match(YesDyad$dname, dyadAll$dname)]
NoDyad$dyadID = dyadAll$dyadID[match(NoDyad$dname, dyadAll$dname)]
JointDyad$dyadID = dyadAll$dyadID[match(JointDyad$dname, dyadAll$dname)]

# aggregate data by year

AbstainDyadYear = aggDyad(AbstainDyad, 'year', 'abstain') 
YesDyadYear = aggDyad(YesDyad, 'year', 'yes') 
NoDyadYear = aggDyad(NoDyad, 'year', 'no')
JointDyadYear = aggDyad(JointDyad, 'year', 'no') 


## Merge datasets
AllDyadYr0 = merge(AbstainDyadYear, YesDyadYear, by = c("dyadID", "year"), all = T)
AllDyadYr1 = merge(NoDyadYear, JointDyadYear, by = c("dyadID", "year"), all = T)
AllDyadYr = merge(AllDyadYr0, AllDyadYr1, by = c("dyadID", "year"), all = T)

# add back country names
AllDyadYr$state_1 = dyadAll$Var1[match(AllDyadYrFINAL$dyadID, dyadAll$dyadID)]
AllDyadYr$state_2 = dyadAll$Var2[match(AllDyadYrFINAL$dyadID, dyadAll$dyadID)]


# Make variable that calculates the proportion of times two countries agree
AllDyadYr = AllDyadYr[, c(1, 2, 4, 5, 8, 6, 7, 3)]
AllDyadYr$jointAgreeNew = rowSums(dplyr::select(AllDyadYr, contains("UN")), na.rm = T )
AllDyadYr$agree3unNew = AllDyadYr$jointAgreeNew/AllDyadYr$all

# Add ccode 
states = union(AllDyadYr$state_1, AllDyadYr$state_2)
temp = data.frame(cbind(states, cname =  toupper(countrycode(states, 'country.name', 'country.name')) ))
temp$cname = as.character(temp$cname)
temp$ccode = panel$ccode[match(temp$cname, panel$cname)]
 
AllDyadYr$cname_1 = temp$cname[match(AllDyadYr$state_1,temp$states)]
AllDyadYr$cname_2 = temp$cname[match(AllDyadYr$state_2,temp$states)]

AllDyadYr$ccode_1 = temp$ccode[match(AllDyadYr$state_1,temp$states)]
AllDyadYr$ccode_2 = temp$ccode[match(AllDyadYr$state_2,temp$states)]

AllDyadYr$cname_1Year = paste(AllDyadYr$cname_1, AllDyadYr$year, sep = "")

unFINALNew = AllDyadYr

setwd(paste0(pathData, '/Components/VoetenData'))
save(unFINALNew, file='unNew.rda')
##############################################################

###############################################################
# Load cleaned data
setwd(paste0(pathData, '/Components/COW_Alliances/version4.1_stata')); load('ally.rda')
setwd(paste0(pathData, '/Components/COW_IGO')); load('igo.rda')
setwd(paste0(pathData, '/Components/VoetenData')); load('unNew.rda')

# Create matrices 
allyMats = DyadBuild(variable='ally', dyadData=allianceFINAL,
    cntry1='ccode_1', cntry2 = 'ccode_2', time='year',
    pd=1970:2010, panel=panel, directed=FALSE)

allyWtMats = DyadBuild(variable='allyWtMax', dyadData=allianceFINAL,
    cntry1='ccode_1', cntry2 = 'ccode_2', time='year',
    pd=1970:2010, panel=panel, directed=TRUE)

igoMats = DyadBuild(variable='igo', dyadData=igoDataFINAL,
    cntry1='ccode_1', cntry2 = 'ccode_2', time='year',
    pd=1970:2005, panel=panel, directed=FALSE)

# unMats.agree2unA = DyadBuild(variable='agree2unA', dyadData=unFINAL,
#     cntry1='ccode_1', cntry2 = 'ccode_2', cntryYear = 'cname_1Year', time='year',
#     pd=1970:2010, panel=panel, directed=FALSE)


unFINALNew$cname_2Year = paste(unFINALNew$cname_2, unFINALNew$year, sep = "")
unMats = DyadBuild(variable='agree3unNew', dyadData=unFINALNew,
    cntry1='ccode_1', cntry2 = 'ccode_2', cntryYear1 = 'cname_1Year', cntryYear2= 'cname_2Year', time='year',
    pd=1970:2010, panel=panel, directed=FALSE)

# Roll mats with gap years over five year window
# warMatsMsum5=mvaStatMat(1970:2010, 5, warMats, avg=FALSE)

setwd(paste0(pathData))
save(
    allyMats, allyWtMats, 
    igoMats, unMats, 
    file='stratInterestMatrics.rda')
###############################################################