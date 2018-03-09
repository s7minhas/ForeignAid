if(Sys.info()["user"]=="janus829" | Sys.info()["user"]=="s7m"){
	source('~/Research/ForeignAid/RCode/setup.R') }
if(Sys.info()["user"]=="cindycheng"){
    source('~/Documents/Papers/ForeignAid/RCode/setup.R') }


 

#----------------------------------------------------------------- 
# load monadic data (note that as of March 2018, only available dyadic data is limited to 1992 to 2001)
midb = read.csv(paste0(pathTnsr, 'MID-level/MIDB_4.01.csv'), stringsAsFactors = FALSE)
midb$dispNum3_ccode = paste0(midb$DispNum3, midb$ccode)
midb = midb[which(midb$StYear > 1970),]

#----------------------------------------------------------------- 

dyads = midb[, c('DispNum3', 'ccode',  'SideA' )] 
 
dyadSplit = split(dyads, dyads$DispNum3)
dyadDf = lapply(dyadSplit, function(x){
  sideA = x$ccode[which(x$SideA == 1)]
  sideB = x$ccode[which(x$SideA == 0)]
  dyad = data.frame(cbind(expand.grid(sideA, sideB)),
                    expand.grid(sideB, sideA))
  names(dyad) = c('ccodeA', 'ccodeB')
  dyad$DispNum3 = unique(x$DispNum3)
  dyad = dyad[, c('DispNum3', 'ccodeA', 'ccodeB')]
  dyad = dyad[order(dyad$ccodeA),]
  dyad$dispNum3_ccodeA = paste0(dyad$DispNum3, dyad$ccodeA)
  dyad$dispNum3_ccodeB = paste0(dyad$DispNum3, dyad$ccodeB)
  return(dyad)
  }) %>% do.call(rbind,.) 

rownames(dyadDf) <- NULL
 
# merge other data onto dyadDf
dyadA = merge(dyadDf[, -which(names(dyadDf) %in% c('dispNum3_ccodeB' ))], 
              midb[, -which(names(midb) %in% c('DispNum3', 'DispNum4', 'ccode'))],
              by.x = 'dispNum3_ccodeA', by.y = 'dispNum3_ccode', all.x = TRUE)
 

dyadB = merge(dyadDf[, -which(names(dyadDf) %in% c('dispNum3_ccodeA' ))], 
              midb[, -which(names(midb) %in% c('DispNum3', 'DispNum4', 'ccode'))],
              by.x = 'dispNum3_ccodeB', by.y = 'dispNum3_ccode', all.x = TRUE)
 
# expand year range of mids so that each year gets its own row
library(tidyr)
dyadA$year <- mapply(seq, dyadA$StYear, dyadA$EndYear, SIMPLIFY = FALSE)
dyadA = dyadA %>% unnest(year) 
dyadB$year <- mapply(seq, dyadB$StYear, dyadB$EndYear, SIMPLIFY = FALSE)
dyadB = dyadB %>% unnest(year) 

# change dyadB so that ccodeB is now ccodeA
names(dyadB)[which(names(dyadB) %in% c('ccodeA', 'ccodeB'))] = c('ccodeB', 'ccodeA')
dyadB = dyadB[, c(names(dyadB)[1], names(dyadA)[-1])]

# combine data
mids = rbind(dyadA[,-1], dyadB[,-1])
mids[c(1:3, 5:21)] = sapply(mids[c(1:3, 5:21)], as.numeric)
 
 
#-----------------------------------------------------------------
# Match mid names to panel
ccodes = c(mids$ccodeA, mids$ccodeB) %>% unique() %>% data.frame(cowcode=.)
ccodes$cname = panel$cname[match(ccodes$cowcode, panel$COWCODE)]
 
# Add ccode
ccodes$ccode = panel$ccode[match(ccodes$cname,panel$cname)]

 
# check
ccodes[which(is.na(ccodes$ccode)),]

# Merge updated cname and ccode to un
mids$cname1 = ccodes$cname[match(mids$ccodeA, ccodes$cowcode)]
mids$cname2 = ccodes$cname[match(mids$ccodeB, ccodes$cowcode)]
mids$ccode1 = ccodes$ccode[match(mids$ccodeA, ccodes$cowcode)]
mids$ccode2 = ccodes$ccode[match(mids$ccodeB, ccodes$cowcode)]

#-----------------------------------------------------------------
# Make list of mids data
library(dplyr)
mids$midCount = 1
mids$Fatality[which(mids$Fatality == -9)] = NA

 
midsAgg <- data.frame(mids %>% group_by(ccode1, ccode2, year) %>% 
                    summarise(HostLev = mean(HostLev, na.rm = TRUE),
                              midCount = sum(midCount),
                              fatalMids = mean(Fatality, na.rm = TRUE)
                              ))
 
# rescale the data so that high values mean less conflict/more strategic interest 
midsAgg$HostLevRev = c(midsAgg$HostLev - c(max(midsAgg$HostLev, na.rm = TRUE)+1))*-1
midsAgg$midCountRev = c(midsAgg$midCount - c(max(midsAgg$midCount, na.rm = TRUE)+1))*-1
midsAgg$fatalMidsRev = c(midsAgg$fatalMids - c(max(midsAgg$fatalMids, na.rm = TRUE)+1))*-1
 

yrs =mids$year %>% unique() %>% sort()
hostLevL = convToList(midsAgg, yrs, 'year', c('ccode1','ccode2'), 'HostLevRev', standardize=FALSE, addDyadLabel=TRUE)
midCountL = convToList(midsAgg, yrs, 'year', c('ccode1','ccode2'), 'midCountRev', standardize=FALSE, addDyadLabel=TRUE)
fatalMidsL = convToList(midsAgg, yrs, 'year', c('ccode1','ccode2'), 'fatalMidsRev', standardize=FALSE, addDyadLabel=TRUE)
 
#-----------------------------------------------------------------

save(midsAgg,
  hostLevL,
  midCountL,
  fatalMidsL,
  file=paste0(pathTnsr, 'mids.rda'))



