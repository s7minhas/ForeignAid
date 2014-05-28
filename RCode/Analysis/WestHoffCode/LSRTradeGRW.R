### Code to accompany LSN paper
### AHW & PDH
### March 18, 2010
### International Trade Example - with randomly implemented random walk

# R Development Core Team (2010). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. ISBN
# 3-900051-07-0, URL http://www.R-project.org.

# Some functions were taken form the MCMCpack
# Andrew D. Martin, Kevin M. Quinn and Jong Hee Park (2010). MCMCpack: Markov chain Monte Carlo (MCMC) Package. R package version 1.0-7.
# http://CRAN.R-project.org/package=MCMCpack
  
###############################################
rm(list=ls(all=TRUE))
func.time <- proc.time()


###############################################
## Libraries & source code
library(MASS)
library(mvtnorm)

###############################################
## Load in data
data <- read.table("Trade.txt")


###############################################
## Get data attributes
A <- length(unique(data$i))
T <- length(unique(data$t))

###############################################
## Set up design matrix
X.c <- cbind(rep(1,A*(A-1)*T), data[,c(7:12)], (data$pty.exp)*(data$pty.imp))
names(X.c) <- c("int", names(data[,c(7:12)]),"pty.int")
row.names(X.c) <- 1:nrow(X.c)
X.c <- as.matrix(X.c)
K <- ncol(X.c)


## Create a time varying design matrix
X.c.t <- matrix(0, nrow=dim(X.c)[[1]], ncol=dim(X.c)[[2]]*T)

c.l <- seq(1, dim(X.c.t)[[2]], dim(X.c)[[2]])
c.h <- seq(dim(X.c)[[2]], dim(X.c.t)[[2]], dim(X.c)[[2]])

for(t in 1:T){
	r.n <- as.numeric(row.names(X.c[data$t==t,]))
	X.c.t[r.n, c.l[t]:c.h[t]] <- as.matrix(X.c[r.n,])
}

colnames(X.c.t) <- c(rep(names(X.c), T))


#############################################
## Set up socio-cubes
dn.Y <- list(3)
dn.Y[[1]] <- unique(data$exp)
dn.Y[[2]] <- unique(data$exp)
dn.Y[[3]] <- 1:T

dn.X <- vector("list",4)
dn.X[[1]] <- unique(data$exp)
dn.X[[2]] <- unique(data$exp)
dn.X[[3]] <- colnames(X.c.t)
dn.X[[4]] <- 1:T

Y <- array(NA, c(A,A,T), dimnames=dn.Y)
X <- array(0, c(A,A,K*T,T),dimnames=dn.X)

c <- 1
for(i in 1:(A-1)){
	for(j in (i+1):A){
		for(t in 1:T){
				print(c)
				Y[i,j,t] <- data$ltrade[c]
				X[i,j,,t] <- as.numeric(X.c.t[c,])
				c <- c+1
				Y[j,i,t] <- data$ltrade[c]
				X[j,i,,t] <- as.numeric(X.c.t[c,])
				c <- c+1
			}}}


Eta <- array(NA, c(A,A,T), dimnames=dn.Y)

###############################################
## Some functions

info <- function(){
	Out <- matrix(0, 1, 13)
	Out <- data.frame(Out)
	names(Out) <- c("Date", "Tot.time", "Actors", "Time", "Burn.in", "Scans", "Thin", "Phi.GG.Stat", "Phi.SR.Stat",
					"Phi.GG.Acc", "Gamma.GG.Acc", "Phi.SR.Acc", "Gamma.SR.Acc")
					
	Out$Date <- date()
	Out$Tot.time <- func.time2[1]-func.time[1]
	Out$Actors <- A
	Out$Time <- T
	Out$Burn.in <- burn
	Out$Scans <- mcmc
	Out$Thin <- thin
	Out$Phi.GG.Stat <- Phi.GG.stat/(it)
	Out$Phi.SR.Stat <- Phi.SR.stat/(it)
	Out$Phi.GG.Acc <- Phi.GG.acc/(it)
	Out$Gamma.GG.Acc <- Gamma.GG.acc/(it)
	Out$Phi.SR.Acc <- Phi.SR.acc/(it)
	Out$Gamma.SR.Acc <- Gamma.SR.acc/(it)
	return(Out)
}

###
my.log.dmvnorm <- function(Y, mu, det.Sigma, inv.Sigma){
	k <- length(Y) 
	return ( (-k/2.0)*log(2*pi) - 0.5 * log(det.Sigma) 
			 -0.5 * t(Y-mu) %*% inv.Sigma %*%(Y-mu) )
	}


###
stat.check <- function(C){
	
	eig <- abs(eigen(C)$values)
	check <- length(eig[eig>=1])
	
	if(check==0){out<-1}
	if(check!=0){out <-0}
	return(out)
## 1 = stationary
}



### From MCMCpack
rwish <- function (v, S) 
{
    if (!is.matrix(S)) 
        S <- matrix(S)
			if (nrow(S) != ncol(S)) {
				stop(message = "S not square in rwish().\n")
			}
			if (v < nrow(S)) {
				stop(message = "v is less than the dimension of S in rwish().\n")
			}
			p <- nrow(S)
			CC <- chol(S)
			Z <- matrix(0, p, p)
			diag(Z) <- sqrt(rchisq(p, v:(v - p + 1)))
			if (p > 1) {
				pseq <- 1:(p - 1)
				Z[rep(p * pseq, pseq) + unlist(lapply(pseq, seq))] <- rnorm(p*(p - 1)/2)
			}
			return(crossprod(Z %*% CC))
}


### From MCMCpack
riwish <- function (v, S) 
{
    return(solve(rwish(v, solve(S))))
}


### From MCMCpack
diwish <- function (W, v, S) 
{
    if (!is.matrix(S)) 
        S <- matrix(S)
			if (nrow(S) != ncol(S)) {
				stop("W not square in diwish().\n")
			}
			if (!is.matrix(W)) 
				S <- matrix(W)
					if (nrow(W) != ncol(W)) {
						stop("W not square in diwish().\n")
					}
					if (nrow(S) != ncol(W)) {
						stop("W and X of different dimensionality in diwish().\n")
					}
					if (v < nrow(S)) {
						stop("v is less than the dimension of S in  diwish().\n")
					}
					k <- nrow(S)
					gammapart <- 1
					for (i in 1:k) {
						gammapart <- gammapart * gamma((v + 1 - i)/2)
					}
					denom <- gammapart * 2^(v * k/2) * pi^(k * (k - 1)/4)
					detS <- det(S)
					detW <- det(W)
					hold <- S %*% solve(W)
					tracehold <- sum(hold[row(hold) == col(hold)])
					num <- detS^(v/2) * detW^(-(v + k + 1)/2) * exp(-1/2 * tracehold)
					return(num/denom)
}



###
X.tm1.GG <- function(yt){
	out <- as.matrix( rbind(yt, c(yt[2], yt[1]))) 	
	return(out)
}

###
X.tm1.SR <- function(yt){
	out <- matrix( c(yt, rep(0,4), yt), ncol=4, byrow=T) 	
	return(out)
}


## Log likelihood
Log.Likelihood <- function(Y.GG, Sigma.GG){
	
	LL <- 0
	Det.Sigma.GG <- det(Sigma.GG)
	Inv.Sigma.GG <- solve(Sigma.GG)
	mu.GG <- rep(0, 2*T)
	
	
	for(i in 1:(A-1)){
		for(j in(i+1):A){
					Y.GG.i <- matrix(rbind(Y.GG[i,j,],Y.GG[j,i,]), ncol=1)
					LL <- LL + my.log.dmvnorm(Y.GG.i, mu.GG, Det.Sigma.GG, Inv.Sigma.GG)
			}}
	
	return(LL)
}


## Log sampling distribution of the random effects 
Log.Sam.Dist.SR <- function(S, R, Sigma.SR){
	
	Det.Sigma.SR <- det(Sigma.SR)
	Inv.Sigma.SR <- solve(Sigma.SR)
	mu.SR <- rep(0, 2*T)
	
	LL <- 0
	
	for(i in 1:A){
		
		S.i <- unique(S[i,,][-i,])
		R.i <- unique(R[,i,][-i,])
		Y.SR.i <- matrix( rbind(S.i,R.i), ncol=1, byrow=T)
		
		LL <- LL + my.log.dmvnorm(Y.SR.i, mu.SR, Det.Sigma.SR, Inv.Sigma.SR)
		
		}
	
	return(LL)
}



## Compute Sigma(0) for an AR(1) model from Phi & Gamma
Sigma.0 <- function(Phi, Gamma){
	vec.Phi <- matrix(Phi, ncol=1)
	vec.Gamma <- matrix(Gamma, ncol=1)
	
	kron.Phi <- kronecker(Phi, Phi)
	Iden <- diag(1, nrow=dim(kron.Phi)[[1]], ncol=dim(kron.Phi)[[2]])
	
	vec.Sigma.0 <- solve(Iden - kron.Phi)%*%vec.Gamma
	return(matrix(vec.Sigma.0,ncol=2))
}


## Compute Sigma(1), Sigma(2), ..., Sigma(T-1) from Sigma(0) and Phi and T
Sigma.par <- function(Sigma.0, Phi, T){
	
	out <- NULL
	out <- c(out, t(Sigma.0))
	
	Sigma.d <- Sigma.0
	for(t in 1:(T-1)){
		Sigma <- Sigma.d%*%t(Phi)
		out <- c(out, t(Sigma))
		Sigma.d <- Sigma
	}
	
	return(out)
}


## Function to compute Sigma
build.Sigma <- function(Phi, Gamma){

Sigma.0.out <- Sigma.0(Phi, Gamma)
Sigma.par.out <- Sigma.par(Sigma.0.out, Phi, T)
	
	
	Sigma <- matrix(0, nrow=2*T, ncol=2*T)
	vcv.c <- 1
	
	for(t in 1:T){
		for(i in 1:4){
			Sigma <- Sigma + Sigma.par.out[vcv.c]*Sigma.list[,,t,i]	
			vcv.c <- vcv.c + 1	
		}}	
	
	return(Sigma)
}


##
trace <- function(X){
out <- sum(diag(X))	
return(out)
}


##
log.kern.iwish <- function(W, v, S){
	k <- dim(S)[[1]]
	out	<- - ((v+k+1)/2)*log(det(W)) - (1/2)*trace(S%*%solve(W))
	return(out)
}

##
log.dinvgamma <- function (theta, alpha, beta){
    log.density <- alpha * log(beta) - lgamma(alpha) - (alpha + 1) * log(theta) - (beta/theta)
    return(log.density)
}

##
log.kern.igamma <- function(theta, alpha, beta){
	out	<- - (alpha+1)*log(theta) - beta/theta
	return(out)
}


###############################################################
## Sigma update List
print("Creating an array of matrices to build Sigma")


Sigma.list <- array(0, c(2*T, 2*T, T, 4))

z1 <- matrix( c(1,0,0,0), nrow=2, ncol=2, byrow=T)
z2 <- matrix( c(0,1,0,0), nrow=2, ncol=2, byrow=T)
z3 <- matrix( c(0,0,1,0), nrow=2, ncol=2, byrow=T)
z4 <- matrix( c(0,0,0,1), nrow=2, ncol=2, byrow=T)


diff <- seq(0, 2*T, by=2)

for(t in 1:T){
	for(u in 1:(2*T)){
		for(v in 1:(2*T)){
			if( u%%2==1){
				if( v%%2==1){
					
					if(v-u==diff[t]){
						Sigma.list[,,t,1][u:(u+1), v:(v+1)] <- z1
						Sigma.list[,,t,2][u:(u+1), v:(v+1)] <- z2
						Sigma.list[,,t,3][u:(u+1), v:(v+1)] <- z3
						Sigma.list[,,t,4][u:(u+1), v:(v+1)] <- z4
					}
					
				
					if(v-u==-diff[t] & v-u!=0){
						Sigma.list[,,t,1][u:(u+1), v:(v+1)] <- t(z1)
						Sigma.list[,,t,2][u:(u+1), v:(v+1)] <- t(z2)
						Sigma.list[,,t,3][u:(u+1), v:(v+1)] <- t(z3)
						Sigma.list[,,t,4][u:(u+1), v:(v+1)] <- t(z4)
					}
					
				}}						
		}}
}




##########################################################################
##########################################################################
## MCMC parameters	
mcmc <- 45000
burn <- 10000
thin <- 20
iter <- mcmc + burn
store <- mcmc/thin


## Storage
LL.store <- rep(0,length=store)
SR.store <- matrix(0, nrow=store, ncol=2*T*A)
Phi.GG.store <- matrix(0, nrow=store, ncol=2)
Gamma.GG.store <- matrix(0, nrow=store, ncol=4)
Phi.SR.store <- matrix(0, nrow=store, ncol=4)
Gamma.SR.store <- matrix(0, nrow=store, ncol=4)
Beta.store <- matrix(0, nrow=store, ncol=K*T)



######################################
## Starting values

S <- array(NA, c(A,A,T))
R <- array(NA, c(A,A,T))

for(i in 1:A){
	for(j in 1:A){
		if(i!=j){
			for(t in 1:T){
				S[i,,t][-i] <- rnorm(1)
				R[,j,t][-j] <- rnorm(1)
			}}}}


Gamma.GG <- matrix(c(1,0,0,1),2,2)
Det.Gamma.GG <- det(Gamma.GG)
Inv.Gamma.GG <- solve(Gamma.GG)

gamma.sq.a <- 2*(Gamma.GG[1,1] + Gamma.GG[1,1]*(Gamma.GG[1,2]/Gamma.GG[1,1]))
gamma.sq.b <- 2*(Gamma.GG[1,1] - Gamma.GG[1,1]*(Gamma.GG[1,2]/Gamma.GG[1,1]))

Gamma.SR <- matrix(c(1,0,0,1) , nrow=2, ncol=2)
Det.Gamma.SR <- det(Gamma.SR)
Inv.Gamma.SR <- solve(Gamma.SR)
gamma.sq.s <- Gamma.SR[1,1]
gamma.sq.r <- Gamma.SR[2,2]
lambda.sr <- Gamma.SR[1,2]/sqrt(Gamma.SR[1,1]*Gamma.SR[2,2])

Phi.GG <- matrix(c(0,0), nrow=1, ncol=2)
Phi.SR <- matrix(c(0,0,0,0), nrow=1, ncol=4)

Sigma.GG <- build.Sigma(matrix(c(Phi.GG, Phi.GG[2], Phi.GG[1]),2,2), Gamma.GG)
Inv.Sigma.GG <- solve(Sigma.GG)

Sigma.SR <- build.Sigma(matrix(Phi.SR,2,2, byrow=T), Gamma.SR)
Inv.Sigma.SR <- solve(Sigma.SR)

####################################
## Stationary 
Phi.GG.stat <- 0
Phi.SR.stat <- 0

## Acceptance
Phi.GG.acc <- 0
Phi.SR.acc <- 0
Gamma.GG.acc <- 0
Gamma.SR.acc <- 0

## Windows
eps.Phi.GG <- 1
eps.Phi.SR <- 0.1
eps.gamma.s <- 0.1
eps.gamma.r <- 0.1
eps.lambda.sr <- 0.1
eps.gamma.sq.ab <- 0.1

###################################
## Priors

## Prior for Beta.T
M.Beta.t <- rep(0, K)
V.Beta.t <- 100

M.Beta.T <- rep(M.Beta.t, T)
V.Beta.T <- diag(V.Beta.t, K*T)
Inv.V.Beta.T <- solve(V.Beta.T)

## Priors for Phi.SR
M.Phi.SR <- rep(0,4)
V.Phi.SR <- diag(100,4)

## Priors for Sigma.SR
v.sr <- 4
S.SR <- matrix(c(1,0,0,1), 2,2)

## Priors for Phi.GG
M.Phi.GG <- rep(0,2)
V.Phi.GG <- diag(100,2)

## Priors for Sigma.GG
alpha <- 1
delta <- 1

###############################################################################
## MCMC 
store.c <- 1

for(it in 1:iter){
print("***************************************")
print("***************************************")

print("Iter")
print(it)
	
ptm.mcmc <- proc.time()


####################################
## Update Beta
	
if(it==1 | runif(1) < 0.50){		
print("Update Beta")
	
Z <- Y-S-R 
	
A.beta <- matrix(0, K*T, K*T)	
B.beta <- matrix(0, K*T, 1)
	
for(i in 1:(A-1)){
	for(j in (i+1):A){
			Z.p <- matrix(rbind(Z[i,j,],Z[j,i,]), ncol=1, byrow=T)
			X.p <- matrix(rbind(X[i,j,,],X[j,i,,]), ncol=K*T, byrow=T)
			A.beta <- A.beta + t(X.p)%*%Inv.Sigma.GG%*%X.p
			B.beta <- B.beta + t(X.p)%*%Inv.Sigma.GG%*%Z.p
		}}

V.use <- solve(A.beta + Inv.V.Beta.T)
M.use <- V.use%*%(B.beta + Inv.V.Beta.T%*%M.Beta.T)
	
Beta <- t(rmvnorm(1, M.use, V.use))	

for(i in 1:(A-1)){
		for(j in (i+1):A){
				Eta[i,j,] <- t(X[i,j,,])%*%Beta
				Eta[j,i,] <- t(X[j,i,,])%*%Beta
		}}
}
	
####################################
## Update SR
if(it==1 | runif(1) < 0.50){	
	print("Update SR")
	V.use <- solve((A-1)*Inv.Sigma.GG + Inv.Sigma.SR)
	
	for(i in 1:A){
		
		Z <- matrix(0, nrow=2*T, ncol=1)
		
		for(j in 1:A){
			if(j!=i){

			Z.ij <- (Y-Eta-R)[i,j,]
			Z.ji <- (Y-Eta-S)[j,i,]
			
			Z.ij.ji <- matrix(rbind(Z.ij, Z.ji), ncol=1)	
				
			Z <- Z + Z.ij.ji
			}}
		
		M.use <- V.use%*%(Inv.Sigma.GG%*%Z)
		SR.i <- mvrnorm(1, M.use, V.use)
		S.i <- matrix(SR.i, ncol=2, byrow=T)[,1]
		R.i <- matrix(SR.i, ncol=2, byrow=T)[,2]
		
		S[i,,][-i,] <- matrix(rep(S.i,(A-1)), nrow=(A-1), byrow=T)
		R[,i,][-i,] <- matrix(rep(R.i,(A-1)), nrow=(A-1), byrow=T)
	}
}
	
####################################
## Update Sigma.GG
print("Update Sigma.GG")
sam <- rbinom(1,1, 0.5)	
	
Y.GG <- Y-Eta-S-R
LL.GG <- Log.Likelihood(Y.GG, Sigma.GG)
##

c <- 1	
Y.GG.list <- array(NA, c(T, 2, (A*(A-1)/2)))
for(i in 1:(A-1)){
	for(j in (i+1):A){
			Y.GG.list[,,c] <- matrix(rbind(Y.GG[i,j,],Y.GG[j,i,]), ncol=2, byrow=T)
			c <- c+1
		}}

	
## Update Phi.GG
print("Update Phi.GG")	
	
A.Phi.GG <- matrix(0, nrow=2, ncol=2)
B.Phi.GG <- matrix(0, nrow=2, ncol=1)
	
	for(i in 1:(A*(A-1)/2)){
		for(t in 2:T){
			
			X.tm1 <- X.tm1.GG(c(Y.GG.list[,,i][t-1,]))
			Y.t <- Y.GG.list[,,i][t,]
			
			A.Phi.GG <- A.Phi.GG + t(X.tm1)%*%Inv.Gamma.GG%*%X.tm1
			B.Phi.GG <- B.Phi.GG + t(X.tm1)%*%Inv.Gamma.GG%*%Y.t
			
		}}
	
V.use <- solve(A.Phi.GG + solve(V.Phi.GG))
V.Win.Phi.GG <- V.use

###	
if(it==1 | sam==0){		
M.use <- V.use%*%(B.Phi.GG + solve(V.Phi.GG)%*%M.Phi.GG)
Phi.GG.p <- rmvnorm(1, M.use, V.use)
	
if(stat.check(matrix(c(Phi.GG.p, Phi.GG.p[2], Phi.GG.p[1]), ncol=2, byrow=T))==1){
	
	Phi.GG.stat <- Phi.GG.stat + 1
	Sigma.GG.p <- build.Sigma(matrix(c(Phi.GG.p, Phi.GG.p[2], Phi.GG.p[1]), ncol=2, byrow=T), Gamma.GG)
	LL.GG.p <- Log.Likelihood(Y.GG, Sigma.GG.p)
	
	lr <- LL.GG.p + dmvnorm(Phi.GG.p, M.Phi.GG, V.Phi.GG, log=T) + dmvnorm(Phi.GG, M.use, V.use, log=T) -
		  LL.GG - dmvnorm(Phi.GG, M.Phi.GG, V.Phi.GG, log=T) - dmvnorm(Phi.GG.p, M.use, V.use, log=T)
		
		
	if(log(runif(1)) < lr){
		Phi.GG <- Phi.GG.p	
		Sigma.GG <- Sigma.GG.p
		Inv.Sigma.GG <- solve(Sigma.GG)
		LL.GG <- LL.GG.p
		Phi.GG.acc <- Phi.GG.acc + 1
	}}
	
}
	

######	
if(it!=1 & sam==1){
		
		Phi.GG.p <- rmvnorm(1, Phi.GG,  eps.Phi.GG*V.Win.Phi.GG)
		
		if(stat.check(matrix(c(Phi.GG.p, Phi.GG.p[2], Phi.GG.p[1]), ncol=2, byrow=T))==1){
			
			Phi.GG.stat <- Phi.GG.stat + 1
			Sigma.GG.p <- build.Sigma(matrix(c(Phi.GG.p, Phi.GG.p[2], Phi.GG.p[1]), ncol=2, byrow=T), Gamma.GG)
			LL.GG.p <- Log.Likelihood(Y.GG, Sigma.GG.p)
			
			lr <- LL.GG.p + dmvnorm(Phi.GG.p, M.Phi.GG, V.Phi.GG, log=T) -
				  LL.GG - dmvnorm(Phi.GG, M.Phi.GG, V.Phi.GG, log=T) 
			
			
			if(log(runif(1)) < lr){
				Phi.GG <- Phi.GG.p	
				Sigma.GG <- Sigma.GG.p
				Inv.Sigma.GG <- solve(Sigma.GG)
				LL.GG <- LL.GG.p
				Phi.GG.acc <- Phi.GG.acc + 1
			}}
		
	}
	
	
	
	
## Update Gamma.GG
print("Update Gamma.GG")
sam <- rbinom(1,1, 0.5)	
	
#####	
if(it==1 | sam==0){	
Phi.GG.use <- matrix(c(Phi.GG, Phi.GG[2], Phi.GG[1]), nrow=2, ncol=2)		
Y.GG.A <- rep(0, (A*(A-1)/2)*(T-1))
Y.GG.B <- rep(0, (A*(A-1)/2)*(T-1))
	
c <- 1
for(i in 1:(A*(A-1)/2)){
	for(t in 2:T){
		out <- matrix(Y.GG.list[,,i][t,], nrow=2, ncol=1) - matrix(Phi.GG.use%*%Y.GG.list[,,i][t-1,], nrow=2, ncol=1)
		Y.GG.A[c] <- out[1]+out[2]
		Y.GG.B[c] <- out[1]-out[2]
		c <- c+1
	}}
	
n <- length(Y.GG.A)
v.a <- sum(Y.GG.A^2)/2
v.b <- sum(Y.GG.B^2)/2
	
alpha.star <- (n/2) + alpha
beta.a.star <- v.a + delta
beta.b.star <- v.b + delta

alpha.beta.a <- c(alpha.star, beta.a.star)
alpha.beta.b <- c(alpha.star, beta.b.star)

gamma.sq.a.p <- 1/rgamma(1, shape=alpha.beta.a[1], rate=alpha.beta.a[2])  
gamma.sq.b.p <- 1/rgamma(1, shape=alpha.beta.b[1], rate=alpha.beta.b[2])
	
gamma.sq.gg.p <- (gamma.sq.a.p + gamma.sq.b.p)/4
lambda.gg.p <- (gamma.sq.a.p - gamma.sq.b.p)/(gamma.sq.a.p + gamma.sq.b.p)
	
	
Gamma.GG.p <- matrix( c(gamma.sq.gg.p, lambda.gg.p*gamma.sq.gg.p, lambda.gg.p*gamma.sq.gg.p, gamma.sq.gg.p), nrow=2, ncol=2) 	
Sigma.GG.p <- build.Sigma(matrix(c(Phi.GG, Phi.GG[2], Phi.GG[1]),2,2), Gamma.GG.p)

LL.GG.p <- Log.Likelihood(Y.GG, Sigma.GG.p)	
	
lr <- LL.GG.p + 
	log.dinvgamma(gamma.sq.a.p, alpha, delta) + log.dinvgamma(gamma.sq.b.p, alpha, delta) +
	log.dinvgamma(gamma.sq.a, alpha.beta.a[1], alpha.beta.a[2]) + log.dinvgamma(gamma.sq.b, alpha.beta.b[1], alpha.beta.b[2]) -
	(LL.GG + 
	log.dinvgamma(gamma.sq.a, alpha, delta) + log.dinvgamma(gamma.sq.b, alpha, delta) +
	log.dinvgamma(gamma.sq.a.p, alpha.beta.a[1], alpha.beta.a[2]) + log.dinvgamma(gamma.sq.b.p, alpha.beta.b[1], alpha.beta.b[2]))
	
if(log(runif(1)) < lr){
	gamma.sq.a <- gamma.sq.a.p
	gamma.sq.b <- gamma.sq.b.p
	Gamma.GG <- Gamma.GG.p	
	Inv.Gamma.GG <- solve(Gamma.GG)
	Sigma.GG <- Sigma.GG.p
	Inv.Sigma.GG <- solve(Sigma.GG)
	LL.GG <- LL.GG.p
	Gamma.GG.acc <- Gamma.GG.acc + 1
}

}
	
#####	
if(it!=1 & sam==1){
		
		gamma.sq.a.p <- abs(gamma.sq.a + runif(1, -eps.gamma.sq.ab, eps.gamma.sq.ab))
		gamma.sq.b.p <- abs(gamma.sq.b + runif(1, -eps.gamma.sq.ab, eps.gamma.sq.ab))
		
		gamma.sq.g.p <- (gamma.sq.a.p+gamma.sq.b.p)/4
		lambda.gamma.p <- (gamma.sq.a.p-gamma.sq.b.p)/(gamma.sq.a.p+gamma.sq.b.p)
		
		Gamma.GG.p <- matrix( c(gamma.sq.g.p, lambda.gg.p*gamma.sq.g.p, lambda.gg.p*gamma.sq.g.p, gamma.sq.g.p), nrow=2, ncol=2) 	
		Sigma.GG.p <- build.Sigma(matrix(c(Phi.GG, Phi.GG[2], Phi.GG[1]),2,2), Gamma.GG.p)
		
		LL.GG.p <- Log.Likelihood(Y.GG, Sigma.GG.p)	
		
		lr <- LL.GG.p + log.kern.igamma(gamma.sq.a.p, alpha, delta) + log.kern.igamma(gamma.sq.b.p, alpha, delta) -
		      LL.GG - log.kern.igamma(gamma.sq.a, alpha, delta) - log.kern.igamma(gamma.sq.b, alpha, delta) 
		
		
		if(log(runif(1)) < lr){
			gamma.sq.a <- gamma.sq.a.p
			gamma.sq.b <- gamma.sq.b.p
			Gamma.GG <- Gamma.GG.p	
			Inv.Gamma.GG <- solve(Gamma.GG)
			Sigma.GG <- Sigma.GG.p
			Inv.Sigma.GG <- solve(Sigma.GG)
			LL.GG <- LL.GG.p
			Gamma.GG.acc <- Gamma.GG.acc + 1
		}
	}
	
	
	
########################################################
## Update Sigma.SR
print("Update Sigma.SR")
LL.SR <-  Log.Sam.Dist.SR(S, R, Sigma.SR)
sam <- rbinom(1,1, 0.5)

# Update Phi.SR
print("Update Phi.SR")

A.Phi.SR <- matrix(0, nrow=4, ncol=4)
B.Phi.SR <- matrix(0, nrow=4, ncol=1)

for(i in 1:A){
	
	S.i <- unique(S[i,,][-i,])
	R.i <- unique(R[,i,][-i,])
	Y.SR.i <- matrix( rbind(S.i,R.i), ncol=2, byrow=T)
	
	for(t in 2:T){	
		
		X.tm1 <- X.tm1.SR(Y.SR.i[t-1,])
		Y.t <- Y.SR.i[t,]
		A.Phi.SR <- A.Phi.SR + t(X.tm1)%*%Inv.Gamma.SR%*%X.tm1
		B.Phi.SR <- B.Phi.SR + t(X.tm1)%*%Inv.Gamma.SR%*%Y.t
	}}


V.use <- solve(A.Phi.SR + solve(V.Phi.SR))
V.Win.Phi.SR <- V.use

	
###	
if(it==1 | sam==0){	
M.use <- V.use%*%(B.Phi.SR + solve(V.Phi.SR)%*%M.Phi.SR)
Phi.SR.p <- rmvnorm(1, M.use, V.use)

if(stat.check(matrix(Phi.SR.p, ncol=2, byrow=T))==1){
		Phi.SR.stat <- Phi.SR.stat + 1
		
		Sigma.SR.p <- build.Sigma(matrix(Phi.SR.p, ncol=2, byrow=T), Gamma.SR)
		LL.SR.p <- Log.Sam.Dist.SR(S, R, Sigma.SR.p)
		
	lr <-  LL.SR.p + dmvnorm(Phi.SR.p, M.Phi.SR, V.Phi.SR, log=T) + dmvnorm(Phi.SR, M.use, V.use, log=T) -
		   LL.SR - dmvnorm(Phi.SR, M.Phi.SR, V.Phi.SR, log=T) - dmvnorm(Phi.SR.p, M.use, V.use, log=T)
	
	if(log(runif(1)) < lr){
		Phi.SR <- Phi.SR.p	
		Sigma.SR <- Sigma.SR.p
		Inv.Sigma.SR <- solve(Sigma.SR)
		LL.SR <- LL.SR.p
		Phi.SR.acc <- Phi.SR.acc + 1
	}}

}
	

#####	
if(it!=1 & sam==1){
		Phi.SR.p <- rmvnorm(1, Phi.SR, eps.Phi.SR*V.Win.Phi.SR)
		
		if(stat.check(matrix(Phi.SR.p, ncol=2, byrow=T))==1){
			Phi.SR.stat <- Phi.SR.stat + 1
			
			Sigma.SR.p <- build.Sigma(matrix(Phi.SR.p, ncol=2, byrow=T), Gamma.SR)
			LL.SR.p <- Log.Sam.Dist.SR(S, R, Sigma.SR.p)
			
			lr <-  LL.SR.p + dmvnorm(Phi.SR.p, M.Phi.SR, V.Phi.SR, log=T) -
			       LL.SR - dmvnorm(Phi.SR, M.Phi.SR, V.Phi.SR, log=T)
			
			if(log(runif(1)) < lr){
				Phi.SR <- Phi.SR.p	
				Sigma.SR <- Sigma.SR.p
				Inv.Sigma.SR <- solve(Sigma.SR)
				LL.SR <- LL.SR.p
				Phi.SR.acc <- Phi.SR.acc + 1
			}}
		
	}
	
	

## Update Gamma.SR
print("Update Gamma.SR")
sam <- rbinom(1,1, 0.5)	
	
if(it==1 | sam==0){	
Z.sum.sq <- matrix(0, nrow=2, ncol=2)

for(i in 1:A){
	
	S.i <- unique(S[i,,][-i,])
	R.i <- unique(R[,i,][-i,])
	Y.SR.i <- matrix( rbind(S.i,R.i), ncol=2, byrow=T)
	
	for(t in 2:T){	
		
		X.tm1 <- X.tm1.SR(Y.SR.i[t-1,])
		Y.t <- Y.SR.i[t,]
		
		Z.sum.sq <- Z.sum.sq + (Y.t - X.tm1%*%t(Phi.SR))%*%t((Y.t- X.tm1%*%t(Phi.SR)))
	}}


Gamma.SR.p <- riwish( A*(T-1) + v.sr, Z.sum.sq + S.SR)
Sigma.SR.p <- build.Sigma(matrix(Phi.SR, ncol=2, byrow=T), Gamma.SR.p)
	
LL.SR.p <- 	Log.Sam.Dist.SR(S, R, Sigma.SR.p)
	
lr <- LL.SR.p + log.kern.iwish(Gamma.SR.p, v.sr, S.SR) + log.kern.iwish(Gamma.SR, A*(T-1)+v.sr, Z.sum.sq + S.SR) -
	  LL.SR - log.kern.iwish(Gamma.SR, v.sr, S.SR) - log.kern.iwish(Gamma.SR.p, A*(T-1)+v.sr, Z.sum.sq + S.SR)

	
if(log(runif(1)) < lr){
	Gamma.SR <- Gamma.SR.p	
	Inv.Gamma.SR <- solve(Gamma.SR)
	Sigma.SR <- Sigma.SR.p
	Inv.Sigma.SR <- solve(Sigma.SR)
	LL.SR <- LL.SR.p
	Gamma.SR.acc <- Gamma.SR.acc + 1
}

}
	
####	
if(it!=1 & sam==1){	
		gamma.sq.s.p <- abs(runif(1, -eps.gamma.s, eps.gamma.s) + gamma.sq.s)
		gamma.sq.r.p <- abs(runif(1, -eps.gamma.r, eps.gamma.r) + gamma.sq.r)
		lambda.sr.p <- runif(1, -eps.lambda.sr, eps.lambda.sr) + lambda.sr
		if(lambda.sr.p>1){lambda.sr.p <- 1-(lambda.sr.p-1)}
		if(lambda.sr.p< -1){lambda.sr.p <- -1 + (-1-lambda.sr.p)}
		gamma.sr.p <- lambda.sr.p*sqrt(gamma.sq.s.p*gamma.sq.r.p) 	
		
		Gamma.SR.p <- matrix(c(gamma.sq.s.p, gamma.sr.p, gamma.sr.p, gamma.sq.r.p),2,2,byrow=T)
		Sigma.SR.p <- build.Sigma(matrix(Phi.SR, ncol=2, byrow=T), Gamma.SR.p)
		
		LL.SR.p <- 	Log.Sam.Dist.SR(S, R, Sigma.SR.p)
		
		lr <- LL.SR.p + log.kern.iwish(Gamma.SR.p, v.sr, S.SR) -
			  LL.SR - log.kern.iwish(Gamma.SR, v.sr, S.SR)
		
		if(log(runif(1)) < lr){
			Gamma.SR <- Gamma.SR.p	
			gamma.sq.s <- Gamma.SR[1,1]
			gamma.sq.r <- Gamma.SR[2,2]
			gamma.sr.rho <- Gamma.SR[1,2]/sqrt(gamma.sq.s*gamma.sq.r)	
			Inv.Gamma.SR <- solve(Gamma.SR)
			Sigma.SR <- Sigma.SR.p
			Inv.Sigma.SR <- solve(Sigma.SR)
			LL.SR <- LL.SR.p
			Gamma.SR.acc <- Gamma.SR.acc + 1
		}
	}
	
	


############################################################

## Some print statements
cat("\n","mcmc iteration :", proc.time() - ptm.mcmc)

cat("\n", "Phi.GG Stat:", (Phi.GG.stat)/(it))	
cat("\n", "Phi.SR Stat:", (Phi.SR.stat)/(it))
cat("\n", "Phi.GG Acc:", (Phi.GG.acc)/(it))	
cat("\n", "Gamma.GG Acc:", (Gamma.GG.acc)/(it))	
cat("\n", "Phi.SR Acc:", (Phi.SR.acc)/(it))	
cat("\n", "Gamma.SR Acc:", (Gamma.SR.acc)/(it), "\n")	

	print(Phi.GG)
	print(Phi.SR)
	print(c(t(Gamma.GG)))
	print(c(t(Gamma.SR)))
#	print(Beta)

############################################################

if(it>burn && it%%thin==0){
LL.store[store.c] <- LL.GG
Phi.GG.store[store.c,] <- Phi.GG
Gamma.GG.store[store.c,] <- c(t(Gamma.GG)) 
Phi.SR.store[store.c,] <- Phi.SR
Gamma.SR.store[store.c,] <- c(t(Gamma.SR)) 
Beta.store[store.c,] <- t(Beta) 

SR.out <- NULL
for(i in 1:A){
		
	S.i <- unique(S[i,,][-i,])
	R.i <- unique(R[,i,][-i,])
	Y.SR.i <- matrix( rbind(S.i,R.i), ncol=1, byrow=T)	
	SR.out <- c(SR.out, Y.SR.i)
	
	}	
	
SR.store[store.c,] <- SR.out
	
store.c <- store.c+1


func.time2 <- proc.time()
mcmc.info <- info()
write.table(mcmc.info, "Info.txt", row.names=F)
write.table(LL.store,"LL.txt", row.names=F, col.names=F)
write.table(Phi.GG.store, "PhiGG.txt", row.names=F, col.names=F)
write.table(Gamma.GG.store, "GammaGG.txt", row.names=F, col.names=F)
write.table(Phi.SR.store, "PhiSR.txt", row.names=F, col.names=F)
write.table(Gamma.SR.store, "GammaSR.txt", row.names=F, col.names=F)
write.table(SR.store, "SR.txt", row.names=F, col.names=F)
write.table(Beta.store, "Beta.txt", row.names=F, col.names=F)
	
}}


func.time2 <- proc.time()
mcmc.info <- info()
write.table(mcmc.info, "Info.txt", row.names=F)
write.table(LL.store,"LL.txt", row.names=F, col.names=F)
write.table(Phi.GG.store, "PhiGG.txt", row.names=F, col.names=F)
write.table(Gamma.GG.store, "GammaGG.txt", row.names=F, col.names=F)
write.table(Phi.SR.store, "PhiSR.txt", row.names=F, col.names=F)
write.table(Gamma.SR.store, "GammaSR.txt", row.names=F, col.names=F)
write.table(SR.store, "SR.txt", row.names=F, col.names=F)
write.table(Beta.store, "Beta.txt", row.names=F, col.names=F)

