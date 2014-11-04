if(Sys.info()['user']=='janus829'){ pathCode='~/Desktop/Research/ForeignAid/RCode' }
if(Sys.info()['user']=='s7m'){ pathCode='~/Research/ForeignAid/RCode' }
if(Sys.info()['user']=='cindycheng'){ pathCode = '~/Documents/Papers/ForeignAid/RCode'}
source(paste0(pathCode, '/setup.R'))

###############################################################
# Clean alliance data [extends from 1816 to 2012]
# setwd(paste0(pathData, '/Components/COW_Alliances/version4.1_stata'))
# alliance = read.dta('alliance_v4.1_by_directed_yearly.dta')
# alliance$ccode1=num(alliance$ccode1)
# alliance$ccode2=num(alliance$ccode2)

# ctyNameA=toupper(countrycode(alliance$ccode1, "cown", "country.name"))
# ctyNameB=toupper(countrycode(alliance$ccode2, "cown", "country.name"))

# sancIDs=data.frame(unique(cbind(alliance$ccode1, alliance$ccode2, ctyNameA, ctyNameB)))

# sancIDs$V1= num(sancIDs$V1)
# sancIDs$V2 = num(sancIDs$V2)
# sancIDs$ctyNameA =as.character(sancIDs$ctyNameA)
# sancIDs$ctyNameB =as.character(sancIDs$ctyNameB)

# sancIDs2 = unique(
#     data.frame(cbind(
#             rbind(cowcode=t(t(sancIDs[,c(1)])), cowcode=t(t(sancIDs[,c(2)]))),
#             rbind(country=t(t(sancIDs[,c(3)])), country=t(t(sancIDs[,c(4)]))) ) ) )
# names(sancIDs2) = c('cowcode', 'country')

# sancIDs2$cowcode = num(sancIDs2$cowcode)
# sancIDs2$country = as.character(sancIDs2$country)

# #fix time
# sancIDs2[sancIDs2$cowcode==245,'country'] = 'BAVARIA'
# sancIDs2[sancIDs2$cowcode==267,'country'] = 'BADEN'
# sancIDs2[sancIDs2$cowcode==300,'country'] = 'AUSTRIA-HUNGARY'
# sancIDs2[sancIDs2$cowcode==730,'country'] = "KOREA, DEMOCRATIC PEOPLE'S REPUBLIC OF"
# sancIDs2[sancIDs2$cowcode==731,'country'] = "KOREA, DEMOCRATIC PEOPLE'S REPUBLIC OF"
# sancIDs2[sancIDs2$cowcode==678,'country'] = 'YEMEN'
# sancIDs2[sancIDs2$cowcode==680,'country'] = 'S. YEMEN' 
# sancIDs2[sancIDs2$cowcode==817,'country'] = 'S. VIETNAM'
# sancIDs2[sancIDs2$cowcode==260,'country'] = 'GERMANY'
# sancIDs2[sancIDs2$cowcode==345,'country'] = 'SERBIA'
# sancIDs2[sancIDs2$cowcode==315,'country'] = 'CZECH REPUBLIC'

# # Add in the data from the panel
# sancIDs2$ccode = panel$ccode[match(sancIDs2$country, panel$cname)]
# sancIDs2$cname = panel$cname[match(sancIDs2$country, panel$cname)]

# sancIDs2[is.na(sancIDs2$ccode),]    # Checks for NAs
# sancIDs2[is.na(sancIDs2$cname),] 

# # Add back to alliance
# alliance2 = alliance[,c('ccode1', 'ccode2', 'state_name1', 'state_name2','year')]
# colnames(alliance2)[1:2] = c('cowcode1', 'cowcode2')

# alliance2$ccode_1 = sancIDs2$ccode[match(alliance2$cowcode1, sancIDs2$cowcode)]
# alliance2$ccode_2 = sancIDs2$ccode[match(alliance2$cowcode2, sancIDs2$cowcode)]

# alliance2$cname_1 = sancIDs2$cname[match(alliance2$cowcode1, sancIDs2$cowcode)]
# alliance2$cname_2 = sancIDs2$cname[match(alliance2$cowcode2, sancIDs2$cowcode)]

# allianceFINAL = na.omit(alliance2)
# allianceFINAL$ally = 1

# save(allianceFINAL, file='ally.rda')
###############################################################

###############################################################
# Clean IGO data [extends from 1820 to 2005]
# setwd(paste0(pathData, '/Components/COW_IGO'))
# # igo = read.dta('IGO_dyadunit_stata_v2.3.dta')
# # save(igo, file='igoData.rda')
# load('igoData.rda')

# igo$ccode1=num(igo$ccode1)
# igo$ccode2=num(igo$ccode2)

# ctyNameA=toupper(countrycode(igo$ccode1, "cown", "country.name"))
# ctyNameB=toupper(countrycode(igo$ccode2, "cown", "country.name"))

# sancIDs=data.frame(unique(cbind(igo$ccode1, igo$ccode2, ctyNameA, ctyNameB)))

# sancIDs$V1= num(sancIDs$V1)
# sancIDs$V2 = num(sancIDs$V2)
# sancIDs$ctyNameA =as.character(sancIDs$ctyNameA)
# sancIDs$ctyNameB =as.character(sancIDs$ctyNameB)

# #fix time
# sancIDs[sancIDs$V1==260,'ctyNameA'] = 'GERMANY'
# sancIDs[sancIDs$V2==260,'ctyNameB'] = 'GERMANY'
# sancIDs[sancIDs$V1==731,'ctyNameA'] = "KOREA, DEMOCRATIC PEOPLE'S REPUBLIC OF"
# sancIDs[sancIDs$V2==731,'ctyNameB'] = "KOREA, DEMOCRATIC PEOPLE'S REPUBLIC OF"
# sancIDs[sancIDs$V1==678,'ctyNameA'] = 'YEMEN'
# sancIDs[sancIDs$V2==678,'ctyNameB'] = 'YEMEN'
# sancIDs[sancIDs$V1==680,'ctyNameA'] = 'S. YEMEN' 
# sancIDs[sancIDs$V2==680,'ctyNameB'] = 'S. YEMEN' 
# sancIDs[sancIDs$V1==817,'ctyNameA'] = 'S. VIETNAM'
# sancIDs[sancIDs$V2==817,'ctyNameB'] = 'S. VIETNAM'
# sancIDs[sancIDs$V1==345,'ctyNameA'] = 'SERBIA'
# sancIDs[sancIDs$V2==345,'ctyNameB'] = 'SERBIA'
# sancIDs[sancIDs$V1==315,'ctyNameA'] = 'CZECH REPUBLIC'
# sancIDs[sancIDs$V2==315,'ctyNameB'] = 'CZECH REPUBLIC'
# sancIDs[sancIDs$V1==730,'ctyNameA'] = "KOREA, DEMOCRATIC PEOPLE'S REPUBLIC OF"
# sancIDs[sancIDs$V2==730,'ctyNameB'] = "KOREA, DEMOCRATIC PEOPLE'S REPUBLIC OF"

# sancIDs2 = unique(
#     data.frame(cbind(
#             rbind(cowcode=t(t(sancIDs[,c(1)])), cowcode=t(t(sancIDs[,c(2)]))),
#             rbind(country=t(t(sancIDs[,c(3)])), country=t(t(sancIDs[,c(4)]))) ) ) )
# names(sancIDs2) = c('cowcode', 'country')

# sancIDs2$cowcode = num(sancIDs2$cowcode)
# sancIDs2$country = as.character(sancIDs2$country)

# # Add in the data from the panel
# sancIDs2$ccode = panel$ccode[match(sancIDs2$country, panel$cname)]
# sancIDs2$cname = panel$cname[match(sancIDs2$country, panel$cname)]

# sancIDs2[is.na(sancIDs2$ccode),]    # Checks for NAs
# sancIDs2[is.na(sancIDs2$cname),] 

# # Add back into igo
# names(igo)[1] = 'cowcode1'
# names(igo)[3] = 'cowcode2'

# igo$ccode_1 = sancIDs2$ccode[match(igo$cowcode1, sancIDs2$cowcode)]
# igo$ccode_2 = sancIDs2$ccode[match(igo$cowcode2, sancIDs2$cowcode)]

# igo$cname_1 = sancIDs2$cname[match(igo$cowcode1, sancIDs2$cowcode)]
# igo$cname_2 = sancIDs2$cname[match(igo$cowcode2, sancIDs2$cowcode)]

# # Finalize IGO dataset
# igoFINAL = igo[! (is.na(igo$ccode_1) | is.na(igo$ccode_2)),]
# igoFINAL = igoFINAL[igoFINAL$year>=1960,c(534:535,5,6:533)]
# igoFINAL = data.matrix(igoFINAL)

# # Set all igo codes of 3, -9, and -1 for IGO membership
# ## to 0 and for igo codes of 1 and 2 set to 1
# drop = c(3, -9, -1, 0)
# years = c(1960,1965:2005)
# igoData = NULL
# for(ii in 1:length(years)){
#     slice = igoFINAL[which(igoFINAL[,'year']==years[ii]),]
#     sList = lapply(4:ncol(slice), function(x) FUN=slice[,c(1:3,x)])
#     sList2 = lapply(sList, function(x) FUN=x[which(!x[,4] %in% drop),])
#     sList3 = sList2[which(num(summary(sList2)[,1])>0)]
#     sList4 = lapply(sList3, function(x){
#         temp = matrix(x, ncol=4); paste(temp[,1],temp[,2],sep='_') })
#     yearIGOs = t(t(table( unlist(sList4) )))
#     yearIGOs = cbind(yearIGOs, year=years[ii])
#     igoData = rbind(igoData, yearIGOs)
#     print(years[ii])
# }

# # Cleaning
# igoDataFINAL = data.frame(cbind(rownames(igoData), igoData), row.names=NULL)
# colnames(igoDataFINAL) = c('ccodes', 'igo', 'year')
# ccodes = matrix(
#     unlist(strsplit(as.character(igoDataFINAL[,'ccodes']), '_')) 
#     ,ncol=2,byrow=T)
# colnames(ccodes) = c('ccode_1','ccode_2')
# igoDataFINAL = cbind(ccodes, igoDataFINAL[,c('year','igo')])
# igoDataFINAL = data.frame(apply(igoDataFINAL,2,num))

# save(igoDataFINAL, file='igo.rda')
###############################################################

###############################################################
# Clean PRIO War Data [first rec'd war in 1946 last in 2012]
# setwd(paste0(pathData, '/Components/PRIO_ArmedConflict'))
# war = read.csv('ucdp.prio.armed.conflict.v4.2013.csv')

# war2 = war[war$Type==2,]
# war2 = unique(war2[,c('ID','SideA', 'SideA2nd', 'SideB',  'SideB2nd', 'YEAR')])
# war2 = war2[1:(nrow(war2)-1),]

# war2$SideA_All = ifelse(trim(war2$SideA2nd)!='',
#     as.character(paste(war2$SideA, war2$SideA2nd, sep=',')),
#     as.character(war2$SideA))

# war2$SideB_All = ifelse(trim(war2$SideB2nd)!='',
#     as.character(paste(war2$SideB, war2$SideB2nd, sep=',')),
#     as.character(war2$SideB))

# war2 = data.frame(war2, row.names=NULL)

# # Arranging to panel format and breaking rows with
# # multiple countries listed into separate rows
# war3 = NULL
# for(ii in 1:nrow(war2)){
#     wSlice = war2[ii, c('SideA_All','SideB_All','YEAR')]
#     Acnts = trim(unlist(strsplit(wSlice$SideA_All,',')))
#     Bcnts = trim(unlist(strsplit(wSlice$SideB_All,',')))
#     lAcnts = length(Acnts)
#     lBcnts = length(Bcnts)
#     if(lAcnts>1 | lBcnts>1){
#         wSlice2 = NULL
#         for(jj in 1:lBcnts){
#             wSlice1_5 = cbind(t(t(Acnts)), t(t(Bcnts))[jj], wSlice[,'YEAR'])
#             colnames(wSlice1_5) = c('SideA_All','SideB_All','YEAR')
#             wSlice2 = rbind(wSlice2, wSlice1_5)
#         }
#         war3 = rbind(war3, wSlice2)
#     } else{
#         wSlice2 = wSlice
#         war3 = rbind(war3, wSlice2)
#     }
# }

# # Adding in cname and ccode
# war3$YEAR = num(war3$YEAR)
# colnames(war3) = c('state_name1','state_name2','year')

# war3 = war3[war3$state_name1!='Hyderabad',]
# war3 = war3[war3$state_name2!='Hyderabad',]

# war3$state_name1[war3$state_name1=='United Arab Emirate'] = 'United Arab Emirates'
# war3$state_name2[war3$state_name2=='United Arab Emirate'] = 'United Arab Emirates'

# states = unique(append(war3$state_name1, war3$state_name2))
# temp = data.frame(cbind(
#     states, cname=toupper(countrycode(states, 'country.name', 'country.name'))))
# temp$cname = as.character(temp$cname)
# temp$cname[temp$cname=='Czechoslovakia'] = 'CZECH REPUBLIC'
# temp$ccode = panel$ccode[match(temp$cname,panel$cname)]

# war3$cname_1 = temp$cname[match(war3$state_name1,temp$states)]
# war3$cname_2 = temp$cname[match(war3$state_name2,temp$states)]

# war3$ccode_1 = temp$ccode[match(war3$state_name1,temp$states)]
# war3$ccode_2 = temp$ccode[match(war3$state_name2,temp$states)]

# war3 = war3[!is.na(war3$ccode_1),]
# war3 = war3[!is.na(war3$ccode_2),]

# warFINAL = war3
# warFINAL$war = 1
# save(warFINAL, file='war.rda')
###############################################################

###############################################################
# Clean UN data

# vote – Vote choice 
# 1 – Yes 
# 2 – Abstain 
# 3 – No 
# 8 – Absent 
# 9 – Not a member 
 
# setwd(paste0(pathData, '/Components/VoetenData/Affinity scores, cow country codes'))
# unData = read.table("session_affinity_scores_un_67_02132013-cow.tab", header=T, stringsAsFactors=F)

 
# # Create variable : i and j agree (no abstensions) / total instances where i and j vote (including abstensions)

# unData$agree2un = as.numeric(unData$agree2un) 
# unData$jointvotes2 = as.numeric(unData$jointvotes2)

# unData$agree2unA<-unData$agree2un*unData$jointvotes2/unData$jointvotes3
 
# # Clean up countrynames
# unData$cname_1 = toupper(countrycode(unData$statea, "cown", "country.name"))
# unData$cname_2 = toupper(countrycode(unData$stateb, "cown", "country.name")) 

# unData$state_name1 = countrycode(unData$statea, "cown", "country.name")
# unData$state_name2 = countrycode(unData$stateb, "cown", "country.name")

# names(unData)[ which(names(unData) %in% c('statea', 'stateb'))]  <- c("ccode_1", "ccode_2")

# unDataFINAL = unData[, c('state_name1', 'state_name2', 'cname_1', 'cname_2', 'ccode_1', 'ccode_2', 'year', 'agree2un', 'agree2unA', 'agree3un')]


# unDataFINAL$agree2unA[which(is.na(unDataFINAL$agree2unA))]<-0
# unDataFINAL$agree2un[is.na(unDataFINAL$agree2un)] = 0
# unDataFINAL$cname_1Year <- paste(unDataFINAL$cname_1, unDataFINAL$year, sep="")
# unDataFINAL$cname_2Year <- paste(unDataFINAL$cname_2, unDataFINAL$year, sep="")


# unDataFINAL<-unDataFINAL[-c( 
# which(unDataFINAL$cname_1Year %in% setdiff(unDataFINAL$cname_1Year, panel$cnameYear)),
# which(unDataFINAL$cname_2Year %in% setdiff(unDataFINAL$cname_2Year, panel$cnameYear))),  ]
 

# setwd(paste0(pathData, '/Components/VoetenData'))
# save(unDataFINAL, file='un.rda')
###############################################################

###############################################################
# # Clean MIDs data

# # load data
# setwd(paste0(pathData, '/Components/MIDs'))
# mid<-read.csv("MIDDyadic_v3.10.csv", stringsAsFactors=F)

# # clean up names
# names(mid)<-tolower(names(mid))
# names(mid)[which(names(mid) %in% c('ccodea', 'ccodeb'))]  = c("ccode_1", "ccode_2")
 
# mid$cname_1 = toupper(countrycode(mid$ccode_1, "cown", "country.name")) 
# mid$cname_2 = toupper(countrycode(mid$ccode_2, "cown", "country.name")) 

# mid$state_name1 = countrycode(mid$ccode_1, "cown", "country.name") 
# mid$state_name2 = countrycode(mid$ccode_2, "cown", "country.name")

# ## Expand the dataset to account for conflicts over all years

# mid1 = panelyear(mid, mid$styear, mid$endyear)
# mid1$mid = 1


# # select variables/years you want
# midFINAL = mid1[, c('state_name1', 'state_name2', 'cname_1', 'cname_2', 'ccode_1', 'ccode_2', 'year', 'mid', 'sideadya', 'sideadyb')]

# save(midFINAL, file='mid.rda')
# ###############################################################

# ###############################################################
# Clean directed alliance data

# setwd(paste0(pathData, '/Components/LeedsData'))
# ally<-read.csv("alliance_v4.1_by_directed.csv", stringsAsFactors=F)

 
# # clean up names
# names(ally)[which(names(ally) %in% c('ccode1', 'ccode2'))]  = c("ccode_1", "ccode_2")
 
# ally$cname_1 = toupper(countrycode(ally$ccode_1, "cown", "country.name")) 
# ally$cname_2 = toupper(countrycode(ally$ccode_2, "cown", "country.name")) 

 
# ## Expand the dataset to account for alliances over all years 
# ally$dyad_end_year[which(is.na(ally$dyad_end_year))] = 2012
# ally1 = panelyear(ally, ally$dyad_st_year, ally$dyad_end_year)
# ally1$ally = 1

# allyDirFINAL = ally1[, c('state_name1', 'state_name2', 'cname_1', 'cname_2', 'ccode_1', 'ccode_2', 'year', 'ally')] 


# allyDirFINAL$cname_1Year <- paste(allyDirFINAL$cname_1, allyDirFINAL$year, sep = "")
# allyDirFINAL$cname_2Year <- paste(allyDirFINAL$cname_2, allyDirFINAL$year, sep = "")

# allyDirFINAL<-allyDirFINAL[-c( 
 # which(allyDirFINAL$cname_1Year %in% setdiff(allyDirFINAL$cname_1Year, panel$cnameYear)),
 # which(allyDirFINAL$cname_2Year %in% setdiff(allyDirFINAL$cname_2Year, panel$cnameYear))),  ]


# save(allyDirFINAL, file ='allydir.rda') 
###############################################################

###############################################################
# Load cleaned data
setwd(paste0(pathData, '/Components/COW_Alliances/version4.1_stata')); load('ally.rda')
setwd(paste0(pathData, '/Components/COW_IGO')); load('igo.rda')
setwd(paste0(pathData, '/Components/PRIO_ArmedConflict')); load('war.rda')
setwd(paste0(pathData, '/Components/VoetenData')); load('un.rda')
setwd(paste0(pathData, '/Components/MIDs')); load('mid.rda')
setwd(paste0(pathData, '/Components/LeedsData')); load('allydir.rda')


# Create matrices 
allyMats = DyadBuild(variable='ally', dyadData=allianceFINAL,
    cntry1='ccode_1', cntry2 = 'ccode_2', time='year',
    pd=1970:2010, panel=panel, directed=FALSE)

igoMats = DyadBuild(variable='igo', dyadData=igoDataFINAL,
    cntry1='ccode_1', cntry2 = 'ccode_2', time='year',
    pd=1970:2005, panel=panel, directed=FALSE)

warMats = DyadBuild(variable='war', dyadData=warFINAL,
    cntry1='ccode_1', cntry2 = 'ccode_2', time='year',
    pd=1970:2010, panel=panel, directed=FALSE)

unMats.agree2unA = DyadBuild(variable='agree2unA', dyadData=unDataFINAL,
    cntry1='ccode_1', cntry2 = 'ccode_2', cntryYear = 'cname_1Year', time='year',
    pd=1970:2010, panel=panel, directed=FALSE)


# Roll mats with gap years over five year window
warMatsMsum5=mvaStatMat(1970:2010, 5, warMats, avg=FALSE)

setwd(pathData)
save(allyMats, igoMats, warMatsMsum5, 
    unMats.agree2un, unMats.agree2unA, unMats.agree3un,
    file='stratInterestMatrics.rda')
###############################################################