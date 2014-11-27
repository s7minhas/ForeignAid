
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
load('allyDist.rda')
allyDist = data.frame(res)
load('igoDist.rda')
igoDist = data.frame(res)
load('unDist.rda')
unDist = data.frame(res)
load('warMsum5Dist.rda')
warDist = data.frame(res)

# merge data
D1 = merge(allyDist, igoDist, by = c("ccode1", "ccode2", "year"), all = T)
D2 = merge(unDist, warDist, by = c("ccode1", "ccode2", "year"), all = T)
D = merge(D1, D2, by = c("ccode1", "ccode2", "year"), all = T)

##### Clean up data ####

# Rescale war matrix
D$warRescale =  - D$warMsum5Dist + max(D$warMsum5Dist)

# Full Data frame
DF = D[, - which(names(D) %in% c("warMsum5Dist"))]  # warMsum5Dist is removed because GenerateDilsNetwork returns all columns that are not used in the PCA
 
# Without War variables at all
DF1 = D[, - which(names(D) %in% c("warMsum5Dist", "warRescale"))]
DF1 = DF1[-which(is.na(DF1$unDist) & DF1$year>2005 ),] # 345 = Yugoslavia; 713 = Taiwan ; 990 = Samoa ; 731 ; 731 = North Korea 341 Montenegro

###### PCA ########
# PCA on full Data
PCA_AllYrs = NULL
PCA_coefficients = NULL
PCA_eigenvalues.sd = NULL
PCA_bootstrap.sds  = NULL

for (yr in c(1970:2010)){
  PCA = getPCA(DF, yr = yr, n.sub = 5000)  
  PCA_AllYrs = rbind(PCA_AllYrs, data.frame(PCA$dils.edgelist)) 
  if (yr <=2005){
    PCA_coefficients = rbind(PCA_coefficients, c(yr, PCA$coefficients))
    PCA_eigenvalues.sd = rbind(PCA_eigenvalues.sd, c(yr, PCA$sdev ))
    PCA_bootstrap.sds = rbind(PCA_bootstrap.sds, c(yr, PCA$bootstrap.sds))
  } else if(yr >2005){
    PCA_coefficients = rbind(PCA_coefficients, c(yr, PCA$coefficients, NA))
    PCA_eigenvalues.sd = rbind(PCA_eigenvalues.sd, c(yr, PCA$sdev , NA))
    PCA_bootstrap.sds = rbind(PCA_bootstrap.sds, c(yr, PCA$bootstrap.sds, NA))
  }
  
}

PCA_FullData = list(PCA_AllYrs= PCA_AllYrs, PCA_coefficients = PCA_coefficients, PCA_eigenvalues.sd= PCA_eigenvalues.sd, PCA_bootstrap.sds = PCA_bootstrap.sds  )

save(PCA_FullData, file = "PCA_FullData.rda")

 
###### PCA without war data
PCA_AllYrs_NW = NULL
PCA_coefficients_NW = NULL
PCA_eigenvalues.sd_NW = NULL
PCA_bootstrap.sds_NW  = NULL

for (yr in c(1970:2005)){
  PCA = getPCA(DF1, yr = yr, n.sub = 5000)  
  PCA_AllYrs_NW = rbind(PCA_AllYrs_NW, data.frame(PCA$dils.edgelist)) 
  if (yr <=2005){
  PCA_coefficients_NW = rbind(PCA_coefficients_NW, c(yr, PCA$coefficients))
  PCA_eigenvalues.sd_NW = rbind(PCA_eigenvalues.sd_NW, c(yr, PCA$sdev ))
  PCA_bootstrap.sds_NW = rbind(PCA_bootstrap.sds_NW, c(yr, PCA$bootstrap.sds))
  } else if (yr > 2005){
    PCA_coefficients_NW = rbind(PCA_coefficients_NW, c(yr, PCA$coefficients, NA))
    PCA_eigenvalues.sd_NW = rbind(PCA_eigenvalues.sd_NW, c(yr, PCA$sdev, NA ))
    PCA_bootstrap.sds_NW = rbind(PCA_bootstrap.sds_NW, c(yr, PCA$bootstrap.sds, NA))
  }
}

 
PCA_NW = list(PCA_AllYrs_NW= PCA_AllYrs_NW, PCA_coefficients_NW = PCA_coefficients_NW, PCA_eigenvalues.sd_NW= PCA_eigenvalues.sd_NW, PCA_bootstrap.sds_NW = PCA_bootstrap.sds_NW  )
save(PCA_NW, file = "PCA_NW.rda")
 



 
