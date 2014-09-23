if(Sys.info()['user']=='janus829'){ 
    pathCode='~/Desktop/Research/ForeignAid/RCode' }
if(Sys.info()['user']=='s7m'){ 
    pathCode='~/Research/ForeignAid/RCode' }
source(paste0(pathCode, '/setup.R'))

###############################################################
# Clean PRIO War Data [first rec'd war in 1946 last in 2012]
setwd(paste0(pathData, '/Components/PRIO_ArmedConflict'))
war = read.csv('ucdp.prio.armed.conflict.v4.2013.csv')

war2 = war[war$Type==2,]
war2 = unique(war2[,c('ID','SideA', 'SideA2nd', 'SideB',  'SideB2nd', 'YEAR')])
war2 = war2[1:(nrow(war2)-1),]

war2$SideA_All = ifelse(trim(war2$SideA2nd)!='',
    as.character(paste(war2$SideA, war2$SideA2nd, sep=',')),
    as.character(war2$SideA))

war2$SideB_All = ifelse(trim(war2$SideB2nd)!='',
    as.character(paste(war2$SideB, war2$SideB2nd, sep=',')),
    as.character(war2$SideB))

war2 = data.frame(war2, row.names=NULL)

# Arranging to panel format and breaking rows with
# multiple countries listed into separate rows
war3 = NULL
for(ii in 1:nrow(war2)){
    wSlice = war2[ii, c('SideA_All','SideB_All','YEAR')]
    Acnts = trim(unlist(strsplit(wSlice$SideA_All,',')))
    Bcnts = trim(unlist(strsplit(wSlice$SideB_All,',')))
    lAcnts = length(Acnts)
    lBcnts = length(Bcnts)
    if(lAcnts>1 | lBcnts>1){
        wSlice2 = NULL
        for(jj in 1:lBcnts){
            wSlice1_5 = cbind(t(t(Acnts)), t(t(Bcnts))[jj], wSlice[,'YEAR'])
            colnames(wSlice1_5) = c('SideA_All','SideB_All','YEAR')
            wSlice2 = rbind(wSlice2, wSlice1_5)
        }
        war3 = rbind(war3, wSlice2)
    } else{
        wSlice2 = wSlice
        war3 = rbind(war3, wSlice2)
    }
}

# Adding in cname and ccode
war3$YEAR = as.numeric(as.character(war3$YEAR))
colnames(war3) = c('state_name1','state_name2','year')

war3 = war3[war3$state_name1!='Hyderabad',]
war3 = war3[war3$state_name2!='Hyderabad',]

war3$state_name1[war3$state_name1=='United Arab Emirate'] = 'United Arab Emirates'
war3$state_name2[war3$state_name2=='United Arab Emirate'] = 'United Arab Emirates'

states = unique(append(war3$state_name1, war3$state_name2))
temp = data.frame(cbind(
    states, cname=toupper(countrycode(states, 'country.name', 'country.name'))))
temp$cname = as.character(temp$cname)
temp$cname[temp$cname=='Czechoslovakia'] = 'CZECH REPUBLIC'
temp$ccode = panel$ccode[match(temp$cname,panel$cname)]

war3$cname_1 = temp$cname[match(war3$state_name1,temp$states)]
war3$cname_2 = temp$cname[match(war3$state_name2,temp$states)]

war3$ccode_1 = temp$ccode[match(war3$state_name1,temp$states)]
war3$ccode_2 = temp$ccode[match(war3$state_name2,temp$states)]

war3 = war3[!is.na(war3$ccode_1),]
war3 = war3[!is.na(war3$ccode_2),]

warFINAL = war3
warFINAL$war = 1

warMats <- DyadBuild(variable='war', dyadData=warFINAL,
    cntry1='ccode_1', 'ccode_2', time='year',
    pd=1970:2010, panel=panel, directed=FALSE)

setwd(pathData)
save(warMats, file='stratInterestMatrics.rda')
###############################################################

###############################################################
# Clean UN data
setwd(paste0(pathData, '/Components/VoetenData/Raw data'))
# unData=read.table('undata-213.tab', sep='\t', header=TRUE)
# save(unData, file='unData.rda')
load('unData.rda')

# vote – Vote choice 
# 1 – Yes 
# 2 – Abstain 
# 3 – No 
# 8 – Absent 
# 9 – Not a member 

# Add year variable to unData
unData$date=as.Date(unData$date)
unData$year=as.numeric(format(unData$date, "%Y"))

# Clean up countrynames
unData$cname=

# Convert to dyadic-year dataset, where value of interest is 
# the sum of times in a year that i-j voted yes on 
# same bill

# Apply dyadbuild function to get matrices necessary for GBME
###############################################################