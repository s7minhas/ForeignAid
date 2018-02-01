if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/ForeignAid/RCode/setup.R') }

###############################################################
# Download file from ICOW site
allyURL = 'http://www.correlatesofwar.org/data-sets/formal-alliances/alliances-data-dta-zip/at_download/file'
allyName = paste0(pathTnsr, 'ally.zip')
if(!file.exists(allyName)) { download.file(allyURL, allyName) }

ally = unzip(allyName, 
	'version4.1_dta/alliance_v4.1_by_directed_yearly.dta') %>% read.dta()
unlink(paste0(getwd(), 'version4.1_dta'), recursive=TRUE, force=TRUE)

# Include only post 1946 data
ally = ally[ally$year>=1946,]

# Subset to relevant vars
ally = ally[,c(
	'ccode1','ccode2','state_name1','state_name2','year',
	'defense', 'neutrality', 'nonaggression', 'entente'
	)]
###############################################################

###############################################################
# Match ally names to panel
cntries = c(ally$state_name1, ally$state_name2) %>% char() %>% unique() %>% data.frame(cntry=.)
cntries$cname = cname(cntries$cntry)

# Fix few cnames issue so it matches with panel
cntries$cname[cntries$cntry=="Yemen People's Republic"] = 'S. YEMEN'
cntries$cname[cntries$cntry=="Yugoslavia"] = 'SERBIA'
cntries$cname[cntries$cntry=="Czechoslovakia"] = 'CZECH REPUBLIC'

# Add ccode
cntries$ccode = panel$ccode[match(cntries$cname,panel$cname)]

# Merge updated cname and ccode to un
ally$cname1 = cntries$cname[match(ally$state_name1, cntries$cntry)]
ally$cname2 = cntries$cname[match(ally$state_name2, cntries$cntry)]
ally$ccode1 = cntries$ccode[match(ally$state_name1, cntries$cntry)]
ally$ccode2 = cntries$ccode[match(ally$state_name2, cntries$cntry)]

# Create separate dfs for defense and any
ally$totCnt = apply(ally[,c('defense', 'neutrality', 'nonaggression', 'entente')], 1, sum)
ally$any = apply(ally[,c('defense', 'neutrality', 'nonaggression', 'entente')], 1, sum) %>% ifelse(., 1, 0)
ally$defEnt = apply(ally[,c('defense', 'entente')], 1, sum) %>% ifelse(., 1, 0)
ally$defEntSum = apply(ally[,c('defense', 'entente')], 1, sum)
ally$did = paste(ally$ccode1,ally$ccode2,ally$year, sep='_')

# Aggregate to count variable for any alliance, need to split DF for this
loadPkg('doBy')
totAlly = ally[ally$totCnt>=1,c('ccode1','ccode2','cname1','cname2','year','totCnt')]
totAlly = summaryBy(totCnt ~ ccode1 + ccode2 + year, data=totAlly, keep.names=TRUE, FUN=sum)

anyAlly = ally[ally$any==1,c('ccode1','ccode2','cname1','cname2','year','any')]
anyAlly = summaryBy(any ~ ccode1 + ccode2 + year, data=anyAlly, keep.names=TRUE, FUN=sum)

defEntAlly = ally[ally$defEnt==1,c('ccode1','ccode2','cname1','cname2','year','defEnt')]
defEntAlly = summaryBy(defEnt ~ ccode1 + ccode2 + year, data=defEntAlly, keep.names=TRUE, FUN=sum)

defEntSumAlly = ally[ally$defEntSum==1,c('ccode1','ccode2','cname1','cname2','year','defEntSum')]
defEntSumAlly = summaryBy(defEntSum ~ ccode1 + ccode2 + year, data=defEntSumAlly, keep.names=TRUE, FUN=sum)

defAlly = ally[ally$defense==1,c('ccode1','ccode2','cname1','cname2','year','defense')]
defAlly = summaryBy(defense ~ ccode1 + ccode2 + year, data=defAlly, keep.names=TRUE, FUN=sum)
###############################################################

###############################################################
# Convert to list
yrs = ally$year %>% unique() %>% sort()
totAllyL = convToList(totAlly, yrs, 'year', c('ccode1','ccode2'), 'totCnt', standardize=FALSE, addDyadLabel=TRUE)
anyAllyL = convToList(anyAlly, yrs, 'year', c('ccode1','ccode2'), 'any', standardize=FALSE, addDyadLabel=TRUE)
defEntAllyL = convToList(defEntAlly, yrs, 'year', c('ccode1','ccode2'), 'defEnt', standardize=FALSE, addDyadLabel=TRUE)
defEntSumAllyL = convToList(defEntSumAlly, yrs, 'year', c('ccode1','ccode2'), 'defEntSum', standardize=FALSE, addDyadLabel=TRUE)
defAllyL = convToList(defAlly, yrs, 'year', c('ccode1','ccode2'), 'defense', standardize=FALSE, addDyadLabel=TRUE)
###############################################################

###############################################################
# Save
save(
	totAlly, totAllyL, 
	anyAlly, anyAllyL, 
	defEntAlly, defEntAllyL, 
	defEntSumAlly, defEntSumAllyL, 	
	defAlly, defAllyL, 	
	file=paste0(pathTnsr, 'ally.rda'))
###############################################################