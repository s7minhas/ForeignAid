if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
    source('~/Research/ForeignAid/RCode/setup.R') }

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
sancIDs2$country = char(sancIDs2$country)

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

# Set all igo codes of 3, -9, and -1 for IGO membership
## to 0 and for igo codes of 1 and 2 set to 1
drop = c(3, -9, -1, 0)
years = c(1960,1965:2005)

igoData = lapply(1:length(years), function(ii){
    slice = igoFINAL[which(igoFINAL[,'year']==years[ii]),]
    sList = lapply(4:ncol(slice), function(x) FUN=slice[,c(1:3,x)])
    sList2 = lapply(sList, function(x) FUN=x[which(!x[,4] %in% drop),])
    sList3 = sList2[which(num(summary(sList2)[,1])>0)]
    sList4 = lapply(sList3, function(x){
        temp = matrix(x, ncol=4); paste(temp[,1],temp[,2],sep='_') })
    yearIGOs = t(t(table( unlist(sList4) )))
    yearIGOs = cbind(yearIGOs, year=years[ii])
    return(yearIGOs)
})

# Combine
igoData = do.call('rbind', igoData)

# Cleaning
igoDataFINAL = data.frame(cbind(rownames(igoData), igoData), row.names=NULL)
colnames(igoDataFINAL) = c('ccodes', 'igo', 'year')
ccodes = matrix(
    unlist(strsplit(char(igoDataFINAL[,'ccodes']), '_')) 
    ,ncol=2,byrow=T)
colnames(ccodes) = c('ccode1','ccode2')
igoDataFINAL = cbind(ccodes, igoDataFINAL[,c('year','igo')])
igo = data.frame(apply(igoDataFINAL,2,num))
############################

############################
# Convert to list object
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
############################

############################
# Save
save(igo, igoL, file=paste0(pathTnsr,'igo.rda'))
############################