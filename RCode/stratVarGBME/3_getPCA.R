
## Tweaks to PCA:

# 1) no option to return eigenvalues in the wrapper function GenerateDilsNetwork; changed code to do this which allows us to see how much variance each component explains
# 2) scale and center is turned to FALSE ; usually shouldn't data be centered before you do a PCA analysis?
# 3) does not sample with replacement, best practice with bootstrapping would seem to indicate that the sample size of each bootstrapped sample should be the same as the size of the original sample, change code to fix this http://www.stata.com/support/faqs/statistics/bootstrapped-samples-guidelines/
# 4) eigenvectors are sometimes inverted relative to each other across samples (i.e. bootstrap 1 will return eigenvector (.5, .4, -.1) while bootstrap 2 will return(-.51, -.42, .1) ;
# 5) to address this problem, made all the eigenvectors consistent with each other by constraining the first element of each eigenvector to be positive
# previously the code had addressed this problem by returning the absolute value for all elements of the eigenvector, which seems baffling
 

# Outstanding issues
# 1) subsample = F returns an error
# 2) unclear what weights value returns


rm(list = ls())


if (Sys.info()['user']=="cindycheng"){
  pathCode="~/Documents/Papers/ForeignAid/RCode";
  pathResults = '~/Dropbox/ForeignAid/Results'}
 
if (Sys.info()['user'] == 'cindy'){
  pathCode="/home/cindy";
  pathResults = "/home/cindy"
}


# load packages
# library(dplyr)
source(paste0(pathCode, "/Analysis/dilsTweak.R"))

# load data
setwd(paste0(pathResults, "/gbmeLatDist"))
load('allyWtDist.rda')
allyDist = data.frame(res)
load('igoDist.rda')
igoDist = data.frame(res)
load('unNewDist.rda')
unDist = data.frame(res)
load('midDist.rda')
midDist = data.frame(res)
load('hostlevDist.rda')
hostlevDist = data.frame(res)
load('hostlevsumDist.rda')
hostlevsumDist = data.frame(res)
load('armsDist.rda')
armsDist = data.frame(res)
load('warMsumDistStd.rda')
warDist = data.frame(res)
load('armsSumDist.rda')
armsSumDist = data.frame(res)
load('jmeDist.rda')
jmeDist = data.frame(res) 


###### PCA - UN, IGO and Ally########
# merge data
D1 = merge(allyDist, igoDist, by = c("ccode1", "ccode2", "year"), all = T)
D = merge(D1, unDist, by = c("ccode1", "ccode2", "year"), all = T)
 

# PCA on full Data
PCA_AllYrs = NULL
PCA_coefficients = NULL
PCA_eigenvalues.sd = NULL
PCA_bootstrap.sds  = NULL

for (yr in c(1970:2005)){
  PCA = getPCA(D, yr = yr, n.sub = 1000)  
  PCA_AllYrs = rbind(PCA_AllYrs, data.frame(PCA$dils.edgelist)) 
    PCA_coefficients = rbind(PCA_coefficients, c(yr, PCA$coefficients))
    PCA_eigenvalues.sd = rbind(PCA_eigenvalues.sd, c(yr, PCA$sdev ))
    PCA_bootstrap.sds = rbind(PCA_bootstrap.sds, c(yr, PCA$bootstrap.sds))  
}

PCA_FullData = list(PCA_AllYrs= PCA_AllYrs, PCA_coefficients = PCA_coefficients, PCA_eigenvalues.sd= PCA_eigenvalues.sd, PCA_bootstrap.sds = PCA_bootstrap.sds  )

save(PCA_FullData, file = "PCA_FullData.rda")

 
###### PCA - JME, HostLev and ArmsTransfers, War ? ########
# hostlev, arms
# hostlevsum, armsSum

#hostlev, armsSum
#hostlevsum, arms

# mid, arms
# mid armsSum


# # merge data
# D1 = merge(midDist, warDist, by = c("ccode1", "ccode2", "year"), all = T)
# D2 = armsSumDist
# D2 = merge(armsDist, jmeDist, by = c("ccode1", "ccode2", "year"), all = T)
# D = merge(D1, D2, by = c("ccode1", "ccode2", "year"), all = T)
# head(D2)

# #  
# D$midDistRescale = -D$midDist + max(D$midDist, na.rm = T)
# D$hostlevDistRescale = -D$hostlevDist + max(D$hostlevDist, na.rm = T)
# D$warDistRescale = -D$warMsum5Dist + max(D$warMsum5Dist, na.rm = T)
# DF = D[, -which(names(D) %in% c("midDist", "warMsum5Dist"))]


# D$hostlevsumDistRescale = -D$hostlevsumDist + max(D$hostlevsumDist, na.rm = T)
# D$warDistRescale = -D$warMsum5Dist + max(D$warMsum5Dist, na.rm = T)
# DF = D[, -which(names(D) %in% c("hostlevsumDist", "warMsum5Dist"))]
# summary(DF)
# cor(DF[, -c(1:3)])

# # PCA on full Data
# PCA_AllYrs = NULL
# PCA_coefficients = NULL
# PCA_eigenvalues.sd = NULL
# PCA_bootstrap.sds  = NULL

# for (yr in c(1990:2010)){
#   PCA = getPCA(DF, yr = yr, n.sub = 1000)  
#   PCA_AllYrs = rbind(PCA_AllYrs, data.frame(PCA$dils.edgelist)) 
#     PCA_coefficients = rbind(PCA_coefficients, c(yr, PCA$coefficients))
#     PCA_eigenvalues.sd = rbind(PCA_eigenvalues.sd, c(yr, PCA$sdev ))
#     PCA_bootstrap.sds = rbind(PCA_bootstrap.sds, c(yr, PCA$bootstrap.sds))  
# }

 

# PCA_FullData = list(PCA_AllYrs= PCA_AllYrs, PCA_coefficients = PCA_coefficients, PCA_eigenvalues.sd= PCA_eigenvalues.sd, PCA_bootstrap.sds = PCA_bootstrap.sds  )

# save(PCA_FullData, file = "PCA_FullData_midWarArmsSum.rda")



### Evaluate eigenvalues
setwd(pathResults)
load('PCA/PCA_FullData_allyIGOUN.rda')
colMeans(PCA_FullData$PCA_eigenvalues.sd[, -1]/c(rowSums(PCA_FullData$PCA_eigenvalues.sd[, -1])))

?colMeans


 
