# Analysis of Spatially Weighted Effects on FDI Inflows
# Author: Shahryar Minhas
###########################################################################
# Loading workspace
source('/Users/janus829/Research/WardProjects/SpatEffects/setup.R')
setwd(pathData)
load("combinedData.rda")
cntries <- length(Y); params<-ncol(X[[1]])
###########################################################################

###########################################################################
# Using MLE on each indiv country to set priors
sigmaOLS<-betaOLS<-NULL
for(ii in 1:cntries) {
  fit<-lm(Y[[ii]]~X[[ii]][,2:ncol(X[[ii]])] )
  betaOLS<-rbind(betaOLS,c(fit$coef)) 
  sigmaOLS<-c(sigmaOLS, summary(fit)$sigma^2) } 
###########################################################################

###########################################################################
# Hierarchical regression model
# Prior parameters
mu0 <- apply(betaOLS, 2, function(x) FUN=mean(x))
nu0 <- 1
sigma20 <- mean(sigmaOLS)
eta0 <- params + 2
S0 <- cov(betaOLS)
lambda0 <- cov(betaOLS)

# Starting values
theta <- mu0
invSigma<-solve(S0)
sigma2 <- mean(sigmaOLS, na.rm=T)

# Storage for results
sims <- 10000
cntryBetas <- matrix(NA, nrow=cntries, ncol=params)
thetaPost <- matrix(NA, nrow=sims/10, ncol=params)
SIGMAPost <- list()
sigma2Post <- NULL

for(iter in 1:sims){
  # Run regressions for each country 
  for(cntry in 1:cntries){
    cntryVar <- solve( invSigma + t(X[[cntry]])%*%X[[cntry]]/sigma2 )
    cntryMean <- cntryVar%*%( invSigma%*%theta + t(X[[cntry]])%*%Y[[cntry]]/sigma2 )
    cntryBetas[cntry,] <- rmvnorm(1, cntryMean, cntryVar) } 

  # Using results from country level regressions to update full sample means
  lambda <-  solve( solve(lambda0) + cntries*invSigma )
  mu <- lambda%*%( solve(lambda0)%*%mu0 + cntries*invSigma%*%apply(cntryBetas, 2, mean) )
  theta <- t(rmvnorm(1, mu, lambda))

  # Using results from country level regressions to update full sample sigma
  ctheta <- matrix(theta, nrow=cntries, ncol=params, byrow=TRUE)
  invSigma<-rwish( 1, eta0+cntries, solve( S0+t(cntryBetas-ctheta)%*%(cntryBetas-ctheta) ) ) 
  
  # Updating sum of squared residuals to calculcate full condit of error term
  SSR<-0
  for(j in 1:cntries) { SSR <- SSR + sum( (Y[[cntry]]-X[[cntry]]%*%cntryBetas[j,] )^2 ) }
  sigma2 <- 1/rgamma(1,(nu0+sum(N))/2, (nu0*sigma20+SSR)/2 )
  
  # Storing results from every 10th iteration to reduce autocorrelation
  if(iter%%10==0){
    thetaPost[iter/10,] <- t(theta)
    SIGMAPost[[iter/10]] <- solve(invSigma)
    sigma2Post[iter/10] <- sigma2
  if(iter%%(sims/10)==0){cat(100*iter/sims,"%  ", sep="")} }
}
###########################################################################

varNames <- append( 'intercept', dimnames(X[[1]])[[2]][2:ncol(X[[1]])])
colnames(thetaPost) <- varNames

###########################################################################
# Burning first 100
thetaPost.burn <- thetaPost[101:nrow(thetaPost),]
SIGMAPost.burn <- SIGMAPost[101:length(SIGMAPost)]

# Assessing convergence
par(mfrow=c(2,5))
for(ii in 1:ncol(thetaPost.burn)){
  plot(101:1000,thetaPost.burn[,ii], type='l')}
par(mfrow=c(1,1))  
###########################################################################

###########################################################################
# Sampling from posterior predictive
betaPostPred.burn <- matrix(NA, nrow=nrow(thetaPost.burn), ncol=params)
colnames(betaPostPred.burn) <- varNames
for(ii in 1:nrow(thetaPost.burn)){ 
  betaPostPred.burn[ii,] <- rmvnorm(1,thetaPost.burn[ii,], SIGMAPost.burn[[ii]]) }
###########################################################################

###########################################################################
# Dirty look at results
cbind(mean=apply(thetaPost.burn, 2, mean), t(apply(thetaPost.burn, 2, 
  function(x) FUN=quantile(x,c(0.025,0.975)))))
cbind(mean=apply(betaPostPred.burn, 2, mean), t(apply(betaPostPred.burn, 2, 
  function(x) FUN=quantile(x,c(0.025,0.975)))))

par(mfrow=c(2,5))
for(ii in 1:ncol(thetaPost.burn)){
  plot(density(thetaPost.burn[,ii])) }
par(mfrow=c(1,1))    

par(mfrow=c(2,5))
for(ii in 1:ncol(betaPostPred.burn)){
  plot(density(betaPostPred.burn[,ii])) }
  par(mfrow=c(1,1))
###########################################################################

###########################################################################
# MLE estimates
model <- ln_fdi_inflows ~ ln_gdp_capita + ln_population + int_conf + polconiii +
              ln_fdi_inflows_mindist + ln_gdp_mindist + ln_pop_mindist + int_conf_mindist +
              interaction + (1|cowcode)
MLEra <- lmer(model, data)
summary(MLEra)
###########################################################################

###########################################################################
# Saving some results for further analysis
setwd(pathData)
save(betaPostPred.burn, thetaPost.burn, SIGMAPost, sigma2Post, MLEra, file='results1.rda')
###########################################################################