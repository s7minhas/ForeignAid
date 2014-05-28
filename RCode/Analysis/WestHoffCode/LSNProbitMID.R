### Code to accompany LSN paper
### AHW & PDH
### March 21, 2010
### MIDs Example

# R Development Core Team (2010). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. ISBN
# 3-900051-07-0, URL http://www.R-project.org.

# Some functions were taken form the MCMCpack
# Andrew D. Martin, Kevin M. Quinn and Jong Hee Park (2010). MCMCpack: Markov chain Monte Carlo (MCMC) Package. R package version 1.0-7.
# http://CRAN.R-project.org/package=MCMCpack
  

##
rm(list=ls(all=TRUE))
func.time <- proc.time()


###############################################
## Libraries & Source Code
library(MASS)
library(mvtnorm)

###############################################
## Load in data
data <- read.table("MID.txt")


###############################################
## Get data attributes
A <- length(unique(data$i))
T <- length(unique(data$t))


###############################################
## Set up design matrix
X.c <- cbind(rep(1,A*(A-1)*T), data[,c(7:11)])
X.c <- as.data.frame(X.c)
names(X.c) <- c("int", names(data[,c(7:11)]))
row.names(X.c) <- 1:dim(X.c)[[1]]
X.c <- as.matrix(X.c)

K <- ncol(X.c)

################################################
## Create a Time Varying Design Matrix
X.c.t <- matrix(0, nrow=dim(X.c)[[1]], ncol=dim(X.c)[[2]]*T)

c.l <- seq(1,dim(X.c.t)[[2]],dim(X.c)[[2]])
c.h <- seq(dim(X.c)[[2]],dim(X.c.t)[[2]],dim(X.c)[[2]])

for(t in 1:T){
	r.n <- as.numeric(row.names(X.c[data$t==t,]))
	X.c.t[r.n, c.l[t]:c.h[t]] <- as.matrix(X.c[r.n,])
}


names(X.c.t) <- c(rep(names(X.c), T))


#############################################
## Set up socio-cubes
## W is the observed response
## Below in Starting values, the Ys are the latent resposes 

dn.W <- list(3)
dn.W[[1]] <- unique(data$nat.i)
dn.W[[2]] <- unique(data$nat.i)
dn.W[[3]] <- 1:T

dn.X <- vector("list",4)
dn.X[[1]] <- unique(data$nat.i)
dn.X[[2]] <- unique(data$nat.i)
dn.X[[3]] <- colnames(X.c.t)
dn.X[[4]] <- 1:T

W <- array(NA, c(A,A,T), dimnames=dn.W)
X <- array(0, c(A,A,K*T,T),dimnames=dn.X)

c <- 1
for(i in 1:(A-1)){
	for(j in (i+1):A){
		for(t in 1:T){
			print(c)
			W[i,j,t] <- data$mid[c]
			X[i,j,,t] <- as.numeric(X.c.t[c,])
			c <- c+1
			W[j,i,t] <- data$mid[c]
			X[j,i,,t] <- as.numeric(X.c.t[c,])
			c <- c+1
		}}}


Eta <- array(NA, c(A,A,T), dimnames=dn.W)
############################################################################################
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

##
my.log.dmvnorm <- function(Y, mu, det.Sigma, inv.Sigma){
	k <- length(Y) 
	return ( (-k/2.0)*log(2*pi) - 0.5 * log(det.Sigma) 
			 -0.5 * t(Y-mu) %*% inv.Sigma %*%(Y-mu) )
	}


##
stat.check <- function(C){
	
	eig <- abs(eigen(C)$values)
	check <- length(eig[eig>=1])
	
	if(check==0){out<-1}
	if(check!=0){out <-0}
	return(out)
## 1 = stationary
}


## From MCMCpack
rwish <- function (v, S){
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


## From MCMCpack
riwish <- function (v, S) {
    return(solve(rwish(v, solve(S))))
}


## From MCMCpack
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



##
X.tm1.GG <- function(yt){
	out <- as.matrix( rbind(yt, c(yt[2], yt[1]))) 	
	return(out)
}

##
X.tm1.SR <- function(yt){
	out <- matrix( c(yt, rep(0,4), yt), ncol=4, byrow=T) 	
	return(out)
}


## likelihood
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
## Function to Compute Sigma
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
log.dinvgamma <- function (x, alpha, beta){
    log.density <- alpha * log(beta) - lgamma(alpha) - (alpha + 1) * log(x) - (beta/x)
    return(log.density)
}



##
rtnorm <- function(mean,sd,lower,upper){
	mu <- mean
	sigma <- sd
	a <- lower
	b <- upper
	
	FA <- pnorm(((a-mu)/sigma))
	FB <- pnorm(((b-mu)/sigma))
	out <- mu+sigma*qnorm(runif(1)*(FB-FA)+FA)

	return(out)
}


###############################################################
## Sigma update list
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
mcmc <- 2562500
burn <- 62500
thin <- 1000
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


## Starting Values
S <- array(NA, c(A,A,T))
R <- array(NA, c(A,A,T))

for(i in 1:A){
	for(j in 1:A){
		if(i!=j){
			for(t in 1:T){
				S[i,,t][-i] <- rnorm(1)
				R[,j,t][-j] <- rnorm(1)
			}}}}


## Y is the underlying latent variable
Y <- array(NA, c(A,A,T), dimnames=dn.W)

for(i in 1:(A-1)){
	for(j in (i+1):A){
		for(t in 1:T){
			
			if(W[i,j,t]==0){Y[i,j,t] <- rtnorm(mean=0, sd=1, lower=-Inf, upper=0)}
			if(W[i,j,t]==1){Y[i,j,t] <- rtnorm(mean=0, sd=1, lower=0, upper=Inf)}
			
			if(W[j,i,t]==0){Y[j,i,t] <- rtnorm(mean=0, sd=1, lower=-Inf, upper=0)}
			if(W[j,i,t]==1){Y[j,i,t] <- rtnorm(mean=0, sd=1, lower=0, upper=Inf)}
		
		}}}




Sigma.SR <- diag(1, nrow=2*T, ncol=2*T)
Inv.Sigma.SR <- solve(Sigma.SR)

Sigma.GG <- diag(1, nrow=2*T, ncol=2*T)
Inv.Sigma.GG <- solve(Sigma.GG)
rho.gg <- 0

Gamma.GG <- diag(1, nrow=2, ncol=2)
gamma.sq.g <- Sigma.GG[1,1]
lambda.gg <- Sigma.GG[1,2]/Sigma.GG[1,1]

gamma.sq.a <- 1
gamma.sq.b <- 1

Det.Gamma.GG <- det(Gamma.GG)
Inv.Gamma.GG <- solve(Gamma.GG)

Gamma.SR <- diag(1, nrow=2, ncol=2)
Inv.Gamma.SR <- solve(Gamma.SR)
sigma.sq.s <- 1
sigma.sq.r <- 1
lambda.sr <- 0

Phi.GG <- matrix(0, nrow=1, ncol=2)
Phi.SR <- matrix(0, nrow=1, ncol=4)

Phi.GG.m <- matrix(0,2,2)
Phi.SR.m <- matrix(0,2,2)

Beta <- rep(0, K*T)

## Stationary
Phi.GG.stat <- 0
Phi.SR.stat <- 0

## Acceptance
Phi.GG.acc <- 0
Phi.SR.acc <- 0
Gamma.GG.acc <- 0
Gamma.SR.acc <- 0


## Prior for Beta.T
M.Beta.t <- rep(0, K)
V.Beta.t <- 100

M.Beta.T <- rep(M.Beta.t, T)
V.Beta.T <- diag(V.Beta.t, K*T)
Inv.V.Beta.T <- solve(V.Beta.T)

## Priors for Phi.SR
M.Phi.SR <- rep(0,4)
V.Phi.SR <- diag(100,4)

## Priors for Phi.GG
M.Phi.GG <- rep(0,2)
V.Phi.GG <- diag(100,2)

## Priors for Sigma.SR
v.sr <- 4
S.SR <- matrix(c(1,0,0,1), 2,2)

## Priors for gamma.cor
v.gamma <- 100

## Windows
eps.rho.gg <- 0.1

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

	V <- solve(A.beta + Inv.V.Beta.T)
	M <- V%*%(B.beta + Inv.V.Beta.T%*%M.Beta.T)
	

Beta <- t(rmvnorm(1, M, V))
	
	for(i in 1:(A-1)){
		for(j in (i+1):A){
			Eta[i,j,] <- t(X[i,j,,])%*%Beta
			Eta[j,i,] <- t(X[j,i,,])%*%Beta
		}}


####################################
## Update SR
	print("Update SR")
	
	V <- solve((A-1)*Inv.Sigma.GG + Inv.Sigma.SR)
	
	for(i in 1:A){
		
		Z <- matrix(0, nrow=2*T, ncol=1)
		
		for(j in 1:A){
			if(j!=i){

			Z.ij <- (Y-Eta-R)[i,j,]
			Z.ji <- (Y-Eta-S)[j,i,]
			
			Z.ij.ji <- matrix(rbind(Z.ij, Z.ji), ncol=1)	
				
			Z <- Z + Z.ij.ji
			}}
		
		M <- V%*%(Inv.Sigma.GG%*%Z)
		SR.i <- mvrnorm(1, M, V)
		S.i <- matrix(SR.i, ncol=2, byrow=T)[,1]
		R.i <- matrix(SR.i, ncol=2, byrow=T)[,2]
		
		S[i,,][-i,] <- matrix(rep(S.i,(A-1)), nrow=(A-1), byrow=T)
		R[,i,][-i,] <- matrix(rep(R.i,(A-1)), nrow=(A-1), byrow=T)
	}
	
	
####################################
## Update Sigma.GG
print("Update Sigma.GG")
	
Y.GG <- Y-Eta-S-R
LL.GG <- Log.Likelihood(Y.GG, Sigma.GG)

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
	
	
	V <- solve(A.Phi.GG + solve(V.Phi.GG))
	M <- V%*%(B.Phi.GG + solve(V.Phi.GG)%*%M.Phi.GG)
	Phi.GG.p <- rmvnorm(1, M, V)
	
	if(stat.check(matrix(c(Phi.GG.p, Phi.GG.p[2], Phi.GG.p[1]), ncol=2, byrow=T))==1){
		Phi.GG.stat <- Phi.GG.stat + 1
		
		gamma.sq.g.p <- 1 - Phi.GG.p[1]^2 - Phi.GG.p[2]^2 - 2*rho.gg*Phi.GG.p[1]*Phi.GG.p[2]
		gamma.gg.p <- rho.gg - 2*Phi.GG.p[1]*Phi.GG.p[2] - rho.gg*Phi.GG.p[1]^2 - rho.gg*Phi.GG.p[2]^2
		Gamma.GG.p <- matrix(c(gamma.sq.g.p, gamma.gg.p, gamma.gg.p, gamma.sq.g.p),2,2, byrow=T)
		
		if(gamma.sq.g.p > 0 & gamma.gg.p >= -1 & gamma.gg.p <= 1){
			
			Sigma.GG.p <- build.Sigma(matrix(c(Phi.GG.p, Phi.GG.p[2], Phi.GG.p[1]), ncol=2, byrow=T), Gamma.GG.p)
			LL.GG.p <- Log.Likelihood(Y.GG, Sigma.GG.p)
			
			lr <- LL.GG.p + dmvnorm(Phi.GG.p, M.Phi.GG, V.Phi.GG, log=T)  + dmvnorm(Phi.GG, M, V, log=T) - 
			      LL.GG - dmvnorm(Phi.GG, M.Phi.GG, V.Phi.GG, log=T) - dmvnorm(Phi.GG.p, M, V, log=T)
			
			
			if(log(runif(1)) < lr){
				Phi.GG <- Phi.GG.p	
				Phi.GG.m <- matrix(c(Phi.GG[1], Phi.GG[2], Phi.GG[2], Phi.GG[1]), 2,2, byrow=T)
				Gamma.GG <- Gamma.GG.p
				Inv.Gamma.GG <- solve(Gamma.GG)
				Sigma.GG <- Sigma.GG.p
				Inv.Sigma.GG <- solve(Sigma.GG)
				LL.GG <- LL.GG.p
				Phi.GG.acc <- Phi.GG.acc + 1
				
			}	
		}}
	
	
##### Update Gamma
print("Update rho.gg")	
	
	rho.gg.p <- rnorm(1, rho.gg, eps.rho.gg)
	
	if(rho.gg.p < -1){rho.gg.p <- -2 - rho.gg.p}
	if(rho.gg.p > 1){rho.gg.p <- 2 - rho.gg.p}
	
	gamma.sq.g.p <- 1 - Phi.GG[1]^2 - Phi.GG[2]^2 - 2*rho.gg.p*Phi.GG[1]*Phi.GG[2]
	gamma.gg.p <- rho.gg.p - 2*Phi.GG[1]*Phi.GG[2] - rho.gg.p*Phi.GG[1]^2 - rho.gg.p*Phi.GG[2]^2
	Gamma.GG.p <- matrix(c(gamma.sq.g.p, gamma.gg.p, gamma.gg.p, gamma.sq.g.p),2,2, byrow=T)
	
	
	if(gamma.sq.g.p > 0 & gamma.gg.p >= -1 & gamma.gg.p <= 1){
		
		Sigma.GG.p <- build.Sigma(matrix(c(Phi.GG, Phi.GG[2], Phi.GG[1]), ncol=2, byrow=T), Gamma.GG.p)
		LL.GG.p <- Log.Likelihood(Y.GG, Sigma.GG.p)
		
		lr <- LL.GG.p + dnorm(rho.gg.p, 0, sqrt(v.gamma), log=T) - LL.GG  - dnorm(rho.gg, 0, sqrt(v.gamma), log=T)
		
		
		if(log(runif(1)) < lr){
			rho.gg <- rho.gg.p
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


V <- solve(A.Phi.SR + solve(V.Phi.SR))
M <- V%*%(B.Phi.SR + solve(V.Phi.SR)%*%M.Phi.SR)
Phi.SR.p <- rmvnorm(1, M, V)


	if(stat.check(matrix(Phi.SR.p, ncol=2, byrow=T))==1){
		Phi.SR.stat <- Phi.SR.stat + 1
		
		Sigma.SR.p <- build.Sigma(matrix(Phi.SR.p, ncol=2, byrow=T), Gamma.SR)
		LL.SR.p <- Log.Sam.Dist.SR(S, R, Sigma.SR.p)
		
		lr <-  LL.SR.p + dmvnorm(Phi.SR.p, M.Phi.SR, V.Phi.SR, log=T)  + dmvnorm(Phi.SR, M, V, log=T) -
			   LL.SR - dmvnorm(Phi.SR, M.Phi.SR, V.Phi.SR, log=T) - dmvnorm(Phi.SR.p, M, V, log=T)
		
		if(log(runif(1)) < lr){
			Phi.SR <- Phi.SR.p	
			Phi.SR.m <- matrix(c(Phi.SR[1], Phi.SR[2], Phi.SR[3], Phi.SR[4]), 2,2, byrow=T)
			Sigma.SR <- Sigma.SR.p
			Inv.Sigma.SR <- solve(Sigma.SR)
			LL.SR <- LL.SR.p
			Phi.SR.acc <- Phi.SR.acc + 1
		}
	}

## 
print("Update Gamma.SR")

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


Gamma.SR.p <- riwish( A*(T-1)+v.sr, Z.sum.sq + S.SR)
Sigma.SR.p <- build.Sigma(matrix(Phi.SR, ncol=2, byrow=T), Gamma.SR.p)
	
LL.SR <- 	Log.Sam.Dist.SR(S, R, Sigma.SR.p)

lr <-   LL.SR.p + log.kern.iwish(Gamma.SR.p, v.sr, S.SR) + log.kern.iwish(Gamma.SR,  A*(T-1)+v.sr, Z.sum.sq + S.SR) -
		LL.SR - log.kern.iwish(Gamma.SR, v.sr, S.SR) -  log.kern.iwish(Gamma.SR.p,  A*(T-1)+v.sr, Z.sum.sq + S.SR)

if(log(runif(1)) < lr){
	Gamma.SR <- Gamma.SR.p	
	Inv.Gamma.SR <- solve(Gamma.SR)
	Sigma.SR <- Sigma.SR.p
	Inv.Sigma.SR <- solve(Sigma.SR)
	LL.SR <- LL.SR.p
	Gamma.SR.acc <- Gamma.SR.acc + 1
}



######## Update the latent response Y
########
print("Update Latent Y")

## for t==1

Sigma.0.GG <- Sigma.0(Phi.GG.m, Gamma.GG)
V <- solve(t(Phi.GG.m)%*%Inv.Gamma.GG%*%Phi.GG.m + solve(Sigma.0.GG) )

for(i in 1:(A-1)){
	for(j in (i+1):A){
		
		mu <- Eta + S + R
		mu.ij.1 <- c(mu[i,j,1], mu[j,i,1])
		
		Z <- Y - Eta - S - R
		C.ij.2 <- c(Z[i,j,2], Z[j,i,2]) + Phi.GG.m%*%mu.ij.1
		
		M <- V%*%(t(Phi.GG.m)%*%Inv.Gamma.GG%*%C.ij.2 + solve(Sigma.0.GG)%*%mu.ij.1)
		
## ij | ji
		
		M.ij <- M[1,1] + (V[1,2]/V[2,2])*(Y[j,i,1]-M[2,1])
		V.ij <- V[1,1] - (V[1,2]*V[2,1])/V[2,2]
		
		if(W[i,j,1]==0){Y[i,j,1] <- rtnorm(mean=M.ij, sd=sqrt(V.ij), lower=-Inf, upper=0)}
		if(W[i,j,1]==1){Y[i,j,1] <- rtnorm(mean=M.ij, sd=sqrt(V.ij), lower=0, upper=Inf)}
		
## ji | ij
		
		M.ji <- M[2,1] + (V[2,1]/V[1,1])*(Y[i,j,1]-M[1,1])
		V.ji <- V[2,2] - (V[2,1]*V[1,2])/V[1,1]
		
		if(W[j,i,1]==0){Y[j,i,1] <- rtnorm(mean=M.ji, sd=sqrt(V.ji), lower=-Inf, upper=0)}
		if(W[j,i,1]==1){Y[j,i,1] <- rtnorm(mean=M.ji, sd=sqrt(V.ji), lower=0, upper=Inf)}
	
		


	}}


## 1 < t < T

V <- solve(t(Phi.GG.m)%*%Inv.Gamma.GG%*%Phi.GG.m + Inv.Gamma.GG)

for(i in 1:(A-1)){
	for(j in (i+1):A){
		for(t in 2:(T-1)){
			
			mu <- Eta + S + R
			mu.ij.t <- c(mu[i,j,t], mu[j,i,t])
			
			Z <- Y - Eta - S - R
			C.ij.tp1 <- c(Z[i,j,t+1], Z[j,i,t+1]) + Phi.GG.m%*%mu.ij.t
			D.ij.tm1 <- mu.ij.t +  Phi.GG.m%*%c(Z[i,j,t-1], Z[j,i,t-1])
			
			M <- V%*%(t(Phi.GG.m)%*%Inv.Gamma.GG%*%C.ij.tp1 + Inv.Gamma.GG%*%D.ij.tm1)
			
## ij | ji
			
			M.ij <- M[1,1] + (V[1,2]/V[2,2])*(Y[j,i,t]-M[2,1])
			V.ij <- V[1,1] - (V[1,2]*V[2,1])/V[2,2]
			
			if(W[i,j,t]==0){Y[i,j,t] <- rtnorm(mean=M.ij, sd=sqrt(V.ij), lower=-Inf, upper=0)}
			if(W[i,j,t]==1){Y[i,j,t] <- rtnorm(mean=M.ij, sd=sqrt(V.ij), lower=0, upper=Inf)}
			
## ji | ij
			
			M.ji <- M[2,1] + (V[2,1]/V[1,1])*(Y[i,j,t]-M[1,1])
			V.ji <- V[2,2] - (V[2,1]*V[1,2])/V[1,1]
			
			if(W[j,i,t]==0){Y[j,i,t] <- rtnorm(mean=M.ji, sd=sqrt(V.ji), lower=-Inf, upper=0)}
			if(W[j,i,t]==1){Y[j,i,t] <- rtnorm(mean=M.ji, sd=sqrt(V.ji), lower=0, upper=Inf)}
			

		}}}



## t = T


V <- Gamma.GG

for(i in 1:(A-1)){
	for(j in (i+1):A){
		
		
		mu <- Eta + S + R
		mu.ij.T <- c(mu[i,j,T], mu[j,i,T])
		
		Z <- Y - Eta - S - R
		D.ij.tm1 <- mu.ij.T +  Phi.GG.m%*%c(Z[i,j,T-1], Z[j,i,T-1])
		
		M <- V%*%(Inv.Gamma.GG%*%D.ij.tm1)
		
## ij | ji
		
		M.ij <- M[1,1] + (V[1,2]/V[2,2])*(Y[j,i,T]-M[2,1])
		V.ij <- V[1,1] - (V[1,2]*V[2,1])/V[2,2]
		
		if(W[i,j,T]==0){Y[i,j,T] <- rtnorm(mean=M.ij, sd=sqrt(V.ij), lower=-Inf, upper=0)}
		if(W[i,j,T]==1){Y[i,j,T] <- rtnorm(mean=M.ij, sd=sqrt(V.ij), lower=0, upper=Inf)}
		
## ji | ij
		
		M.ji <- M[2,1] + (V[2,1]/V[1,1])*(Y[i,j,T]-M[1,1])
		V.ji <- V[2,2] - (V[2,1]*V[1,2])/V[1,1]
		
		if(W[j,i,T]==0){Y[j,i,T] <- rtnorm(mean=M.ji, sd=sqrt(V.ji), lower=-Inf, upper=0)}
		if(W[j,i,T]==1){Y[j,i,T] <- rtnorm(mean=M.ji, sd=sqrt(V.ji), lower=0, upper=Inf)}
		
		

		
	}}



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
	print(Beta[1:K])



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
write.table(LL.store, "LL.txt", row.names=F, col.names=F)
write.table(Phi.GG.store, "PhiGG.txt", row.names=F, col.names=F)
write.table(Gamma.GG.store, "GammaGG.txt", row.names=F, col.names=F)
write.table(Phi.SR.store, "PhiSR.txt", row.names=F, col.names=F)
write.table(Gamma.SR.store, "GammaSR.txt", row.names=F, col.names=F)

write.table(SR.store, "SR.txt",  row.names=F, col.names=F)
write.table(Beta.store, "Beta.txt", row.names=F, col.names=F)


}}


func.time2 <- proc.time()
mcmc.info <- info()
write.table(mcmc.info, "Info.txt", row.names=F)
write.table(LL.store, "LL.txt", row.names=F, col.names=F)
write.table(Phi.GG.store, "PhiGG.txt", row.names=F, col.names=F)
write.table(Gamma.GG.store, "GammaGG.txt", row.names=F, col.names=F)
write.table(Phi.SR.store, "PhiSR.txt", row.names=F, col.names=F)
write.table(Gamma.SR.store, "GammaSR.txt", row.names=F, col.names=F)

write.table(SR.store, "SR.txt",  row.names=F, col.names=F)
write.table(Beta.store, "Beta.txt", row.names=F, col.names=F)


