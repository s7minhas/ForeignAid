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


# # clean country names
# ctyNameA=toupper(countrycode(alliance$ccode1, "cown", "country.name"))
# ctyNameB=toupper(countrycode(alliance$ccode2, "cown", "country.name"))

# sancIDs = data.frame( cowcode = intersect(alliance$ccode1, alliance$ccode2), 
#                        country = toupper(countrycode(intersect(alliance$ccode1, alliance$ccode2), "cown", "country.name")), stringsAsFactors = F)

# #fix time
# sancIDs[sancIDs$cowcode==245,'country'] = 'BAVARIA'
# sancIDs[sancIDs$cowcode==267,'country'] = 'BADEN'
# sancIDs[sancIDs$cowcode==300,'country'] = 'AUSTRIA-HUNGARY'
# sancIDs[sancIDs$cowcode==730,'country'] = "KOREA, DEMOCRATIC PEOPLE'S REPUBLIC OF"
# sancIDs[sancIDs$cowcode==731,'country'] = "KOREA, DEMOCRATIC PEOPLE'S REPUBLIC OF"
# sancIDs[sancIDs$cowcode==678,'country'] = 'YEMEN'
# sancIDs[sancIDs$cowcode==680,'country'] = 'S. YEMEN' 
# sancIDs[sancIDs$cowcode==817,'country'] = 'S. VIETNAM'
# sancIDs[sancIDs$cowcode==260,'country'] = 'GERMANY'
# sancIDs[sancIDs$cowcode==345,'country'] = 'SERBIA'
# sancIDs[sancIDs$cowcode==315,'country'] = 'CZECH REPUBLIC'

# # Add in the data from the panel
# sancIDs$ccode = panel$ccode[match(sancIDs2$country, panel$cname)]
# sancIDs$cname = panel$cname[match(sancIDs2$country, panel$cname)]

# sancIDs[is.na(sancIDs$ccode),]    # Checks for NAs
# sancIDs[is.na(sancIDs$cname),] 

# # weight alliances
 
# alliance[, 'defense'] = alliance[, 'defense']*3
# alliance[, 'neutrality'] = alliance[, 'neutrality']*2
# alliance[, 'nonaggression'] = alliance[, 'nonaggression']*2
# alliance$allyWtSum = rowSums(alliance[, c('defense', 'nonaggression', 'neutrality', 'entente')], na.rm = T)
# alliance$allyWtMax = apply(alliance[, c('defense', 'nonaggression', 'neutrality', 'entente')], 1, max, na.rm = T)
 
# # Add back to alliance
# alliance2 = alliance[,c('ccode1', 'ccode2', 'state_name1', 'state_name2','year', 'allyWtMax', 'allyWtSum')]
# colnames(alliance2)[1:2] = c('cowcode1', 'cowcode2')
 
# alliance2$ccode_1 = sancIDs$ccode[match(alliance2$cowcode1, sancIDs2$cowcode)]
# alliance2$ccode_2 = sancIDs$ccode[match(alliance2$cowcode2, sancIDs2$cowcode)]

# alliance2$cname_1 = sancIDs$cname[match(alliance2$cowcode1, sancIDs2$cowcode)]
# alliance2$cname_2 = sancIDs$cname[match(alliance2$cowcode2, sancIDs2$cowcode)]

# allianceFINAL = na.omit(alliance2)
# allianceFINAL$ally = 1

 
# save(allianceFINAL, file='ally.rda')
###############################################################

###############################################################
# # Clean IGO data [extends from 1820 to 2005]
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
#     }
#     sList3 = sList2[which(num(summary(sList2)[,1])>0)]

#     sList4 = lapply(sList3, function(x){
#         temp = matrix(x, ncol=4); paste(temp[,1],temp[,2],sep='_') })
#     yearIGOs = t(t(table( unlist(sList4) )))
#     yearIGOs = cbind(yearIGOs, year=years[ii])
#     igoData = rbind(igoData, yearIGOs)
#     print(years[ii])
# }
#
# 
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

# #### Make new measures from the raw data
 
# # load data
# setwd(paste0(pathData, '/Components/VoetenData/Raw Data'))
# load('unData.rda')
# unData$uniquename = as.character(unData$uniquename)

# # Create year - roll call variable
# unData$yrRcid = paste(substring(unData$date, 1, 4), unData$rcid, sep ="_")

# # Subset the data to only abstain votes, only yes votes, only no votes
# unAbstain = unData[which(unData$vote ==2), c('yrRcid', 'vote', 'ccode', 'uniquename')]
# unYes = unData[which(unData$vote ==1), c('yrRcid', 'vote', 'ccode', 'uniquename')]
# unNo = unData[which(unData$vote ==3), c('yrRcid', 'vote', 'ccode', 'uniquename')]
# unJoint = unData[-which(unData$vote ==8 | unData$vote ==9), c('yrRcid', 'vote', 'ccode', 'uniquename')]

# # Create dyad pairs for countries that both agree on a resolution
# AbstainDyad = makeDyad(unAbstain, 'yrRcid')
# YesDyad = makeDyad(unYes, 'yrRcid')
# NoDyad = makeDyad(unNo, 'yrRcid')
# JointDyad = makeDyad(unJoint, 'yrRcid')
          
# # make unique dyad ID
# countries = unique(unData$uniquename)
# dyadAll = expand.grid(countries, countries)  
# dyadAll$Var1 = as.character(dyadAll$Var1); dyadAll$Var2 = as.character(dyadAll$Var2)
# dyadAll= dyadAll[-which(dyadAll[,1] == dyadAll[,2]),]
# dyadAll$dname = paste(dyadAll$Var1, dyadAll$Var2, sep = "_")

# dyadAll$dyadID = 0
# for ( i in 1:dim(dyadAll)[1] ) {
# if(dyadAll$dyadID[i] ==0){   
# dyadAll$dyadID[which(dyadAll$Var1 %in% c(dyadAll[i,]) & dyadAll$Var2 %in% c(dyadAll[i,]))] = i
# }
# }

# # Add unique dyad ID to Agree dyadic dataset
# AbstainDyad$dyadID = dyadAll$dyadID[match(AbstainDyad$dname, dyadAll$dname)]
# YesDyad$dyadID = dyadAll$dyadID[match(YesDyad$dname, dyadAll$dname)]
# NoDyad$dyadID = dyadAll$dyadID[match(NoDyad$dname, dyadAll$dname)]
# JointDyad$dyadID = dyadAll$dyadID[match(JointDyad$dname, dyadAll$dname)]

# # aggregate data by year

# AbstainDyadYear = aggDyad(AbstainDyad, 'year', 'abstain') 
# YesDyadYear = aggDyad(YesDyad, 'year', 'yes') 
# NoDyadYear = aggDyad(NoDyad, 'year', 'no')
# JointDyadYear = aggDyad(JointDyad, 'year', 'no') 


# ## Merge datasets
# AllDyadYr0 = merge(AbstainDyadYear, YesDyadYear, by = c("dyadID", "year"), all = T)
# AllDyadYr1 = merge(NoDyadYear, JointDyadYear, by = c("dyadID", "year"), all = T)
# AllDyadYr = merge(AllDyadYr0, AllDyadYr1, by = c("dyadID", "year"), all = T)

# # add back country names
# AllDyadYr$state_1 = dyadAll$Var1[match(AllDyadYrFINAL$dyadID, dyadAll$dyadID)]
# AllDyadYr$state_2 = dyadAll$Var2[match(AllDyadYrFINAL$dyadID, dyadAll$dyadID)]


# # Make variable that calculates the proportion of times two countries agree
# AllDyadYr = AllDyadYr[, c(1, 2, 4, 5, 8, 6, 7, 3)]
# AllDyadYr$jointAgreeNew = rowSums(dplyr::select(AllDyadYr, contains("UN")), na.rm = T )
# AllDyadYr$agree3unNew = AllDyadYr$jointAgreeNew/AllDyadYr$all

# # Add ccode 
# states = union(AllDyadYr$state_1, AllDyadYr$state_2)
# temp = data.frame(cbind(states, cname =  toupper(countrycode(states, 'country.name', 'country.name')) ))
# temp$cname = as.character(temp$cname)
# temp$ccode = panel$ccode[match(temp$cname, panel$cname)]
 
# AllDyadYr$cname_1 = temp$cname[match(AllDyadYr$state_1,temp$states)]
# AllDyadYr$cname_2 = temp$cname[match(AllDyadYr$state_2,temp$states)]

# AllDyadYr$ccode_1 = temp$ccode[match(AllDyadYr$state_1,temp$states)]
# AllDyadYr$ccode_2 = temp$ccode[match(AllDyadYr$state_2,temp$states)]

# AllDyadYr$cname_1Year = paste(AllDyadYr$cname_1, AllDyadYr$year, sep = "")

# unFINALNew = AllDyadYr

# setwd(paste0(pathData, '/Components/VoetenData'))
# save(unFINALNew, file='unNew.rda')



# # Note that making the whole dataset into a dyadic edgelist takes a lot of computing power - ugly hack below to make it run faster
# # n = length(unique(unJoint$yrRcid))
# # unJoint1 = unJoint[which(unJoint$yrRcid %in% unique(unJoint$yrRcid)[1:c(n/2-.5)]),]
# # unJoint1.5 = unJoint[which(unJoint$yrRcid %in% unique(unJoint$yrRcid)[c(n/2+.5):c(3/4*n - .25)]),]
# # unJoint2 = unJoint[which(unJoint$yrRcid %in% unique(unJoint$yrRcid)[c(3/4*n + .75):n]),]
# # 
# # JointDyad1 = mcparallel(makeDyad(unJoint1, 'yrRcid'))
# # JointDyad1.5 = mcparallel(makeDyad(unJoint1.5, 'yrRcid'))
# # JointDyad2 = mcparallel(makeDyad(unJoint2, 'yrRcid'))
 
# # res =  mccollect(JointDyad1, wait = F)
# # res1.5 =  mccollect(JointDyad1.5, wait = F)
# # res2 = mccollect(JointDyad2, wait = F)
  
# # JD = res[[1]]
# # JD1.5 = res1.5[[1]]
# # JD2 = res2[[1]]
  
# # JD2.0 = JD2[1:c(dim(JD2)[1]/2),]
# # JD2.1 = JD2[c(dim(JD2)[1]/2 + 1): dim(JD2)[1],]
 
# # JD$dyadID = dyadAll$dyadID[match(JD$dname, dyadAll$dname)]
# # JD1.5$dyadID = dyadAll$dyadID[match(JD1.5$dname, dyadAll$dname)]
# # JD2.0$dyadID = dyadAll$dyadID[match(JD2.0$dname, dyadAll$dname)]
# # JD2.1$dyadID = dyadAll$dyadID[match(JD2.1$dname, dyadAll$dname)]
 
# # JointDyadYear1 = aggDyad(JD, 'year', 'all')
# # JointDyadYear1.5 = aggDyad(JD1.5, 'year', 'all')
# # JointDyadYear2.0 = aggDyad(JD2.0, 'year', 'all')
# # JointDyadYear2.1 = aggDyad(JD2.1, 'year', 'all')
 
# # JointDyad = rbind(JointDyadYear1, JointDyadYear1.5, JointDyadYear2.0, JointDyadYear2.1)
# # JointDyadYear = aggregate(all ~ dyadID + year, data = JointDyad, sum) 
 
# # JointDyadYear$cname_1 = dyadAll$Var1[match(JointDyadYear$dyadID, dyadAll$dyadID)]
# # JointDyadYear$cname_2 = dyadAll$Var2[match(JointDyadYear$dyadID, dyadAll$dyadID)]
 




# # #### Using measures from the original dataset
# # setwd(paste0(pathData, '/Components/VoetenData/Affinity scores, cow country codes'))
# # unData = read.table("session_affinity_scores_un_67_02132013-cow.tab", header=T, stringsAsFactors=F)
  
# # # Create variable : i and j agree (no abstensions) / total instances where i and j vote (including abstensions)
# # unData$agree2unA<-unData$agree2un*unData$jointvotes2/unData$jointvotes3
 
# # # Clean up countrynames
# # unData$cname_1 = toupper(countrycode(unData$statea, "cown", "country.name"))
# # unData$cname_2 = toupper(countrycode(unData$stateb, "cown", "country.name")) 
 
# # unData$state_name1 = countrycode(unData$statea, "cown", "country.name")
# # unData$state_name2 = countrycode(unData$stateb, "cown", "country.name")
# # names(unData)[ which(names(unData) %in% c('statea', 'stateb'))]  <- c("ccode_1", "ccode_2")
 
# # unDataFINAL = unData[, c('state_name1', 'state_name2', 'cname_1', 'cname_2', 'ccode_1', 'ccode_2', 'year', 'agree2un', 'agree2unA', 'agree3un')]
# # unDataFINAL$agree2unA[which(is.na(unDataFINAL$agree2unA))]<-0
# # unDataFINAL$agree2un[is.na(unDataFINAL$agree2un)] = 0
# # unDataFINAL$cname_1Year <- paste(unDataFINAL$cname_1, unDataFINAL$year, sep="")
# # unDataFINAL$cname_2Year <- paste(unDataFINAL$cname_2, unDataFINAL$year, sep="")
 
# # unDataFINAL<-unDataFINAL[-c( 
# # which(unDataFINAL$cname_1Year %in% setdiff(unDataFINAL$cname_1Year, panel$cnameYear)),
# # which(unDataFINAL$cname_2Year %in% setdiff(unDataFINAL$cname_2Year, panel$cnameYear))),  ]
  
# # setwd(paste0(pathData, '/Components/VoetenData'))
# # save(unDataFINAL, file='un.rda')

 
###############################################################

###############################################################
# # Clean MIDs data

# # load data
# setwd(paste0(pathData, '/Components/MIDs'))
# mid<-read.csv("MIDB_4.01.csv", stringsAsFactors=F)

# # clean up names
# names(mid) = tolower(names(mid))
# dispnums = unique(mid$dispnum3)

# midDyad = do.call(rbind, lapply(dispnums, function(x){
#                  slice = mid[mid$dispnum3 == x,]
#                  sideA = slice[which(slice$sidea ==1),]
#                  sideB = slice[which(slice$sidea ==0),]
#                  rawDyad = data.frame(cbind(expand.grid(sideA$ccode, sideB$ccode), unique(slice$dispnum3)))
#                  names(rawDyad) = c("ccode_1", "ccode_2", "dispnum3")
#                  rawDyadF = merge( rawDyad, slice[, c('dispnum3', 'ccode', 'hostlev')],  by.x = c('dispnum3', 'ccode_2'), by.y = c('dispnum3', 'ccode'), all.x = T)
#                  return(rawDyadF)
#                  }) )

 


# # note there are multiple endyears per dispute so can't just add directly in the lapply function above, must merge as in below
# midDyadYr = merge(midDyad, mid[, c('dispnum3', 'styear', 'endyear')], by = c("dispnum3"), all = T)
# midDyadYr = unique(midDyadYr)
# # create unique dyadIDs
# countries = unique(mid$ccode)
# dyadAll = expand.grid(countries, countries)  
# dyadAll$Var1 = as.character(dyadAll$Var1); dyadAll$Var2 = as.character(dyadAll$Var2)
# dyadAll= dyadAll[-which(dyadAll[,1] == dyadAll[,2]),]
# dyadAll$dname = paste(dyadAll$Var1, dyadAll$Var2, sep = "_")

# dyadAll$dyadID = 0
# for ( i in 1:dim(dyadAll)[1] ) {
# if(dyadAll$dyadID[i] ==0){   
# dyadAll$dyadID[which(dyadAll$Var1 %in% c(dyadAll[i,]) & dyadAll$Var2 %in% c(dyadAll[i,]))] = i
# }
# }


# # # Add unique dyad ID to mid dyadic dataset
# midDyadYr$dname = paste(midDyadYr$ccode_1, midDyadYr$ccode_2, sep ="_")
# midDyadYr$dyadID = dyadAll$dyadID[match(midDyadYr$dname, dyadAll$dname)]


 
# ## Expand the dataset to account for conflicts over all years
# midDyadAllYr  = panelyear(midDyadYr, midDyadYr$styear, midDyadYr$endyear)
# midDyadAllYr$mid = 1


 

# # Aggregate mids per dyad-year
# midDyadAggYr = select(midDyadAllYr, -(c(styear, endyear))) %>% group_by (dyadID, year) %>% summarize(hostlev = mean(hostlev), mid = sum(mid))

 
# # # Add back ccode and country names 
# midDyadAggYr$cow_1 = num(dyadAll$Var1[match(midDyadAggYr$dyadID, dyadAll$dyadID)])
# midDyadAggYr$cow_2 = num(dyadAll$Var2[match(midDyadAggYr$dyadID, dyadAll$dyadID)])


# midDyadAggYr$ccode_1 = panel$ccode[match(midDyadAggYr$cow_1, panel$COWCODE)]
# midDyadAggYr$ccode_2 = panel$ccode[match(midDyadAggYr$cow_2, panel$COWCODE)]

# midDyadAggYr$cname_1 = panel$cname[match(midDyadAggYr$ccode_1, panel$ccode)]
# midDyadAggYr$cname_2 = panel$cname[match(midDyadAggYr$ccode_2, panel$ccode)]


# midDyadAggYr = midDyadAggYr[-which(is.na(midDyadAggYr$ccode_1)|is.na(midDyadAggYr$ccode_2)),]

# # select variables/years you want
# midFINAL = data.frame(midDyadAggYr)

# summary(midFINAL)
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
# Clean joint military exercise data
# library(dplyr)
# library(stringr)

# setwd(paste0(pathData, '/Components/Orazio'))
# jme<-read.csv("jme_v1.1.csv", stringsAsFactors=F)

# jme2 = dplyr::select(jme, c(s.year, e.year, statea:statep))
# jme2$ID = 1:dim(jme2)[1]
 
# # make into dyadic data
# jmeDyad = melt(jme2, id.vars = c('s.year', 'e.year', 'ID'))
# jmeDyad =  plyr::rename(jmeDyad, replace= c("value" = "country"))
# jmeDyad = jmeDyad[-which(jmeDyad$country==""), - which(names(jmeDyad)=="variable")]
# jmeDyad$country = str_trim(toupper(jmeDyad$country))
 
# uniqueID = unique(jmeDyad[,'ID'])
# rawDyad = NULL 
# for (ii in 1:length(uniqueID)){
	# slice = jmeDyad[which(jmeDyad[, 'ID'] == uniqueID[ii]),]
	# if( dim(slice)[1] ==1 ){
	# sList2 = c(slice[,'ID'], slice[,4], NA)} 
	# else if ( dim(slice)[1] > 1 ){
	# sList2 = cbind(unique(slice[, 'ID']),t(combn(slice[,4], 2))) }
	# rawDyad = rbind(rawDyad, sList2)
# }

# rawDyad2 = data.frame(rawDyad, stringsAsFactors= F)
# names(rawDyad2) = c("ID", "stabb_1", "stabb_2")

 
# # add years
# jmeDyadYr = merge(rawDyad2, jme2[, c('s.year', 'e.year', 'ID')], by = "ID", all = T ) 
 
# # remove data for which we don't have end years 
# jmeDyadYrClean = jmeDyadYr[-which(jmeDyadYr$e.year =="-9"|jmeDyadYr$e.year =="xxxx"),]
# jmeDyadYrClean$e.year = num(jmeDyadYrClean$e.year)
# # jmeDyadYrClean = jmeDyadYrClean[-which(jmeDyadYrClean$e.year < jmeDyadYrClean$s.year),] - fixed in v1.1 version of the dataset

# # get full number of years
# table(jmeDyadYrClean$e.year - jmeDyadYrClean$s.year)
# jmeDyadAllYrs = panelyear(jmeDyadYrClean, jmeDyadYrClean$s.year, jmeDyadYrClean$e.year)
 
# # add cow codes and countrynames
 
# stabb = union(jmeDyadAllYrs$stabb_1, jmeDyadAllYrs$stabb_2)
# cow = countrycode (stabb, 'iso3c', 'cown')
# countryKey = data.frame(stabb, cow)
# countryKey$ccode = panel$ccode[match(countryKey$cow, panel$COWCODE )]
# countryKey$cname = panel$cname[match(countryKey$cow, panel$COWCODE )]

# jmeDyadAllYrs$ccode_1 = countryKey$ccode[match(jmeDyadAllYrs$stabb_1, countryKey$stabb)]
# jmeDyadAllYrs$ccode_2 = countryKey$ccode[match(jmeDyadAllYrs$stabb_2, countryKey$stabb)]
# jmeDyadAllYrs$cname_1 = countryKey$cname[match(jmeDyadAllYrs$stabb_1, countryKey$stabb)]
# jmeDyadAllYrs$cname_2 = countryKey$cname[match(jmeDyadAllYrs$stabb_2, countryKey$stabb)]

# jmeDyadAllYrs$cname_1[grep("SRB", jmeDyadAllYrs$stabb_1)] = "SERBIA"
# jmeDyadAllYrs$cname_2[grep("SRB", jmeDyadAllYrs$stabb_2)] = "SERBIA"
# jmeDyadAllYrs$ccode_1[grep("SRB", jmeDyadAllYrs$stabb_1)] = 345
# jmeDyadAllYrs$ccode_2[grep("SRB", jmeDyadAllYrs$stabb_2)] = 345

# jmeDyadAllYrs$cname_1[grep("CSK", jmeDyadAllYrs$stabb_1)] = "CZECHOSLOVAKIA"
# jmeDyadAllYrs$cname_2[grep("CSK", jmeDyadAllYrs$stabb_2)] = "CZECHOSLOVAKIA"
# jmeDyadAllYrs$ccode_1[grep("CSK", jmeDyadAllYrs$stabb_1)] = 315
# jmeDyadAllYrs$ccode_2[grep("CSK", jmeDyadAllYrs$stabb_2)] = 315

# jmeDyadAllYrs$cname_1[grep("DDR", jmeDyadAllYrs$stabb_1)] = "GERMAN DEMOCRATIC REPUBLIC"
# jmeDyadAllYrs$cname_2[grep("DDR", jmeDyadAllYrs$stabb_2)] ="GERMAN DEMOCRATIC REPUBLIC"
# jmeDyadAllYrs$ccode_1[grep("DDR", jmeDyadAllYrs$stabb_1)] = 265
# jmeDyadAllYrs$ccode_2[grep("DDR", jmeDyadAllYrs$stabb_2)] =265


# jmeDyadAggYrs = jmeDyadAllYrs[-which(is.na(jmeDyadAllYrs$ccode_1)|is.na(jmeDyadAllYrs$ccode_2)), ]
# jmeDyadAggYrs$jme = 1

 
# # Aggregate by Dyad Year
# jmeFINAL = jmeDyadAggYrs %>% group_by (cname_1, cname_2, ccode_1, ccode_2, year ) %>% dplyr::summarize( jme = sum(jme))
# jmeFINAL  = data.frame(jmeFINAL )
 
# setwd(paste0(pathData, '/Components/Orazio'))
# save(jmeFINAL, file = 'jme_v1_1.rda')

# # missing states names
# jmeDyadAllYrs[which(jmeDyadAllYrs$ccode_1 %in% union(unique(jmeDyadAllYrs[which(is.na(jmeDyadAllYrs$ccode_1)),'stabb_1'] ), unique(jmeDyadAllYrs[which(is.na(jmeDyadAllYrs$ccode_2)), 'stabb_2'] )) | jmeDyadAllYrs$ccode_2 %in% union(unique(jmeDyadAllYrs[which(is.na(jmeDyadAllYrs$ccode_1)),'stabb_1'] ), unique(jmeDyadAllYrs[which(is.na(jmeDyadAllYrs$ccode_2)), 'stabb_2'] ))),] 
# # get the 8 NATO Members.... or throw it out
# #DDR is East Germany, CSK is Czechoslovakia, and SRB is Serbia. 

# states = c("Denmark", 'Bulgaria', 'Japan', 'Germany', 'Algeria', 'United Arab Emirates', 'Germany', 'Czechoslovakia', 'Italy')
# exist = countrycode(states, 'country.name', 'iso3c')

 

###############################################################

###############################################################
# Arms Transfers


# folder = paste0(pathData, "/components/SIPRI/sipri")
# filenames <- list.files(folder, pattern="*.csv", full.names=TRUE)
# ldf <- lapply(filenames, read.csv, skip = 10, header = T)
 

# # get country names 
# countries = lapply(filenames, read.csv, header = F)
# country = lapply(countries, function(x){
# 	header = as.character(x[1,1])
# 	n = nchar(header)
# 	country = substring(header, 24, n - 11)
# 	return(country)
# 	})
 
# names(ldf) = unlist(country)

# # merge data
# arms = do.call(rbind,lapply(names(lapply(ldf, names)), function(x){
#  	data = ldf[[x]]
#  	data =  dplyr::select(data, -c(X, Total))
#  	data = data[-which(data$X.1==""|data$X.1=="Total"),]
# 	data = melt(data, id.vars = "X.1")
# 	names(data) = c("country_2", "year", "arms")
# 	data$year = as.numeric(gsub("X", "", data$year))
# 	data$country_1 = x
# 	return(data)
# }))

# armsDyad = arms[-which(is.na(arms$arms)),]
# armsDyad$arms[which(armsDyad$arms==0)]<-0.5
 
 
# # standardize country names and country codes

# uniqueCountry = union(unique(arms$country_1), unique(arms$country_2))
# country = countrycode(uniqueCountry, "country.name", "country.name")
# countryKey = data.frame(country = country, countrySIPRI =  uniqueCountry, stringsAsFactors = F)
# countryKey[which(countryKey$countrySIPRI == "UAE"),'country'] = 'United Arab Emirates'
# countryKey[grep("UK", countryKey$countrySIPRI), 'country']= 'United Kingdom'
# countryKey$ccode = panel$ccode[match(toupper(countryKey$country), panel$cname )]
# countryKey$cname = panel$cname[match(toupper(countryKey$country), panel$cname )]
# countryKey = countryKey[-which(countryKey$countrySIPRI=="African Union**"),]
 

# armsDyad$cname_1 = countryKey$cname[match(armsDyad$country_1, countryKey$countrySIPRI)]
# armsDyad$cname_2 = countryKey$cname[match(armsDyad$country_2, countryKey$countrySIPRI)]
# armsDyad$ccode_1 = countryKey$ccode[match(armsDyad$country_1, countryKey$countrySIPRI)]
# armsDyad$ccode_2 = countryKey$ccode[match(armsDyad$country_2, countryKey$countrySIPRI)]

# armsDyad = armsDyad[-which(is.na(armsDyad$cname_1)|is.na(armsDyad$cname_2)),]

 
# # Aggregate by dyad-year (some countries have multiple entries)
# armsAggDyad = armsDyad %>% group_by (ccode_1, ccode_2, cname_1, cname_2, year) %>% dplyr::summarize( arms = sum(arms))
 
 
 
# # Weight by total number of exports for that exporter during a particular year
# exportYr = armsDyad %>% group_by (cname_2, year) %>% dplyr::summarize( armsTot = sum(arms))
# armsFINAL = merge(armsAggDyad, exportYr, by = c("cname_2", "year"), all = T)
# armsFINAL$armsWt = armsFINAL$arms / armsFINAL$armsTot
 
# # save 
# setwd(paste0(pathData, "/components/SIPRI"))
# save(armsFINAL, file ='arms.rda')

###############################################################

###############################################################
# Load cleaned data
setwd(paste0(pathData, '/Components/COW_Alliances/version4.1_stata')); load('ally.rda')
setwd(paste0(pathData, '/Components/COW_IGO')); load('igo.rda')
setwd(paste0(pathData, '/Components/PRIO_ArmedConflict')); load('war.rda')
setwd(paste0(pathData, '/Components/VoetenData')); load('unNew.rda')
setwd(paste0(pathData, '/Components/MIDs')); load('mid.rda')
setwd(paste0(pathData, "/components/SIPRI")); load('arms.rda')
setwd(paste0(pathData, '/Components/Orazio')); save(jmeFINAL, file = 'jme_v1_1.rda')
# setwd(paste0(pathData, '/Components/LeedsData')); load('allydir.rda')



# Create matrices 
# allyMats = DyadBuild(variable='ally', dyadData=allianceFINAL,
#     cntry1='ccode_1', cntry2 = 'ccode_2', time='year',
#     pd=1970:2010, panel=panel, directed=FALSE)

allyWtMats = DyadBuild(variable='allyWtMax', dyadData=allianceFINAL,
    cntry1='ccode_1', cntry2 = 'ccode_2', time='year',
    pd=1970:2010, panel=panel, directed=TRUE)

igoMats = DyadBuild(variable='igo', dyadData=igoDataFINAL,
    cntry1='ccode_1', cntry2 = 'ccode_2', time='year',
    pd=1970:2005, panel=panel, directed=FALSE)

warMats = DyadBuild(variable='war', dyadData=warFINAL,
    cntry1='ccode_1', cntry2 = 'ccode_2', time='year',
    pd=1970:2010, panel=panel, directed=FALSE)

midMats = DyadBuild(variable='mid', dyadData=midFINAL,
    cntry1='ccode_1', cntry2 = 'ccode_2', time='year',
    pd=1970:2010, panel=panel, directed=FALSE)

hostlevMats = DyadBuild(variable='hostlev', dyadData=midFINAL,
    cntry1='ccode_1', cntry2 = 'ccode_2', time='year',
    pd=1970:2010, panel=panel, directed=T)

# unMats.agree2unA = DyadBuild(variable='agree2unA', dyadData=unFINAL,
#     cntry1='ccode_1', cntry2 = 'ccode_2', cntryYear = 'cname_1Year', time='year',
#     pd=1970:2010, panel=panel, directed=FALSE)


unFINALNew$cname_2Year = paste(unFINALNew$cname_2, unFINALNew$year, sep = "")
unMats = DyadBuild(variable='agree3unNew', dyadData=unFINALNew,
    cntry1='ccode_1', cntry2 = 'ccode_2', cntryYear1 = 'cname_1Year', cntryYear2= 'cname_2Year', time='year',
    pd=1970:2010, panel=panel, directed=FALSE)

armsWtMats = DyadBuild(variable='armsWt', dyadData=armsFINAL,
    cntry1='ccode_1', cntry2 = 'ccode_2',  time='year',
    pd=1970:2010, panel=panel, directed=TRUE)

armsMats = DyadBuild(variable='arms', dyadData=armsFINAL,
    cntry1='ccode_1', cntry2 = 'ccode_2',  time='year',
    pd=1970:2010, panel=panel, directed=TRUE)

jmeMats = DyadBuild(variable='jme', dyadData=jmeFINAL,
    cntry1='ccode_1', cntry2 = 'ccode_2',  time='year',
    pd=1974:2010, panel=panel, directed=FALSE)


# Roll mats with gap years over five year window
warMatsMsum5=mvaStatMat(1970:2010, 5, warMats, avg=FALSE)


 
setwd(paste0(pathData))
save(allyWtMats, igoMats, warMatsMsum5, midMats, hostlevMats, unMats,armsMats, armsWtMats, jmeMats,
    file='stratInterestMatrics.rda')


 

###############################################################



