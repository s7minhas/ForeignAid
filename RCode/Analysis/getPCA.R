

# subsample = F returns an error
# no option to return.sds in the wrapper function GenerateDilsNetwork
# scale and center is turned to FALSE ; usually shouldn't data be centered before you do a PCA analysis?
# unclear what weights value returns
# does not sample with replacement, change code to fix this


# eigenvectors are sometimes inverted; changing the code so that they are absolute values seems like the wrong way
# to address this problem; should instead make all eigenvectors consistent with each other....
# it seems to me that you shouldn't take the absolute value per se, but ensure that
# all the components are going in the same direction.....

# the way it does the PCA is through bootstrapping so, 
# note that it would be nice to know how much the first component explains; adjust function to return this


# http://www.stata.com/support/faqs/statistics/bootstrapped-samples-guidelines/
# The above statement contains the key to choosing the right number of replications. Here is the recipe:

# Choose a large but tolerable number of replications. Obtain the bootstrap estimates.
# Change the random-number seed. Obtain the bootstrap estimates again, using the same number of replications.
# Do the results change meaningfully? If so, the first number you chose was too small. Try a larger number. If results are similar enough, you probably have a large enough number. To be sure, you should probably perform step 2 a few more times, but I seldom do.

rm(list = ls())


if (Sys.info()['user']=="cindycheng"){
  pathCode="~/Documents/Papers/ForeignAid/RCode";
  pathResults = '~/Dropbox/ForeignAid/Results'}
 
# load packages
library(dplyr)
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



PCA_AllYrs = NULL
PCA_coefficients = NULL
PCA_eigenvalues.sd = NULL
PCA_bootstrap.sds  = NULL

for (yr in c(1970:2010)){
	PCA = getPCA(yr = yr)
	PCA_AllYrs = rbind(PCA_AllYrs, data.frame(PCA$dils.edgelist)) 
	PCA_coefficients = rbind(PCA_coefficients, c(yr, PCA$coefficients))
	PCA_eigenvalues.sd = rbind(PCA_eigenvalues.sd, c(yr, PCA$sdev ))
	PCA_bootstrap.sds = rbind(PCA_bootstrap.sds, c(yr, PCA$bootstrap.sds))

}

PCATotal = list(PCA_AllYrs, PCA_coefficients, PCA_eigenvalues.sd, PCA_bootstrap.sds)

save(PCATotal, file = "PCA.rda")

 
 