###Preconditions of the main function "gbme"

##objects with no defaults
#Y : a square matrix (n*n)

##objects with defaults
#Xd : an n*n*rd array representing dyad-specific predictors
#Xs : an n*rs matrix of sender-specific predictors
#Xr : an n*rr matrix of receiver-specific predictors

# fam : "gaussian" "binomial", or "poisson"
# if fam="binomial" then an (n*n) matrix N of trials is needed
#
# pi.Sab = (s2a0,sab0,s2b0,v0) : parameters of inverse wishart dist
# pi.s2u = vector of length 2: parameters of an inv-gamma distribution
# pi.s2v = vector of length 2: parameters of an inv-gamma distribution
# pi.s2z = k*2 matrix : parameters of k inv-gamma distributions
#
# pim.bd  : vector of length rd, the prior mean for beta.d
# piS.bd  : a rd*rd pos def matrix, the prior covariance for beta.d
#
# pim.b0sr : vector of length 1+rs+rr, prior mean for (beta0,beta.s,beta.r)
# piS.b0sr  : a (1+rs+rr)*(1+rs+rr) pos def matrix, 
#             the prior variance for (beta0,beta.s,beta.r)


# starting values
# beta.d   #a vector of length rd
# beta.u   #a vector of length 1+rs+rr
# s        #a vector of length n
# r        #a vector of length n 
# z        #an n*k matrix

###the main  function

library(magic)
library(msm)
library(lme4)
library(mnormt)


gbme.ef <-function(
  Y=matrix(nrow(Y), ncol(Y)),  
  Xd=array(dim=c(nrow(Y),nrow(Y),0)),
  Xs=matrix(nrow=nrow(Y),ncol=0),
  Xr=matrix(nrow=nrow(Y),ncol=0), 
  
  #model specification
  fam="gaussian",         
  N=matrix(1, ncol(Y), ncol(Y)),
  k=0,
  priors=NULL, # the list of priors
  startv=NULL, # the list of starting values
  #random seed, 
  seed=0,
  burn=0, #the size of burn-in
  pred=FALSE, #sample from predicted distribution; NO by default
  
  #details of output
  NS=100,         #number of scans of mcmc to run
  odens=1,         #output density
  owrite=T,          #write output to a file? 
  ofilename="out",   #name of output file 
  oround=3,          #rounding of output
  zwrite=(k>0),      #write z to  file 
  zfilename="Z",     #outfile for z 
  sdigz=3,           #rounding for z
  awrite=F,
  bwrite=F,
  afilename="A",
  bfilename="B",
  write.all=FALSE,
  out.name=NULL
  )
{
  
  ###set seed
  set.seed(seed)                                                                 
  
  ###dimensions of everything
  n<<-ncol(Y)
  rd<-dim(Xd)[3]
  rs<-dim(Xs)[2]
  rr<-dim(Xr)[2]   
  diag(Y)<-rep(0,n)
  ###
  
  ## create a folder for the output
  
  ##
  if (write.all==TRUE){ 
    
    awrite=TRUE
    bwrite=TRUE
    
    if (fam=="gaussian"){folder = paste("output", "y", sep=".")}
    if (fam=="binomial"){folder = paste("output", "m", sep=".")}
    
    initwd = getwd()
    wpath  = paste(getwd(),"/",folder, sep="")  
    dir.create(wpath)
    setwd(wpath)
    f.dir = function(x){  dir.create(paste(wpath,x, sep="/"))}
    f.path=function(x) {paste(wpath,x,paste(x, out.name, sep="."), sep="/")}
    
    sapply(c("out", "a", "b", "u", "v", "yhat", "uTv"), f.dir)
    
    out.path  = f.path("out")
    a.path  = f.path("a")
    b.path  = f.path("b")
    u.path  = f.path("u")
    v.path  = f.path("v")
    yhat.path = f.path("yhat")
    uTv.path = f.path("uTv")
    
  }
  
  ###column names for output file
  
  cnames<-c("k","scan","ll",paste("bd",seq(1,rd,length=rd),sep="")[0:rd],"b0",
            paste("bs",seq(1,rs,length=rs),sep="")[0:rs],
            paste("br",seq(1,rr,length=rr),sep="")[0:rr],
            "s2a","sab","s2b","se2","rho", paste("s2e",seq(1,k,length=k),sep="")[0:k],
            paste("s2f",seq(1,k,length=k),sep="")[0:k] )                         
  
  ###design matrix for unit-specific predictors
  
  X.u <- cbind(rep(.5,2*n), adiag(Xs,Xr))
  
  ###construct an upper triangular matrix (useful for later computations)
  tmp<-matrix(1,n,n)
  tmp[lower.tri(tmp,diag=T)]<-NA 
  UT<<-tmp
  ###
  
  if (is.null(startv)){
    X = apply(Xd, 3, c)
    s=rep(1:n,n)
    r=rep(1:n, each=n)
    cat("Using MLE to calculate starting values", '\n')
    options(warn=-1)
    if (fam=="gaussian") mle  <- glmer(c(Y) ~ X + (1|s) + (1|r))
    if (fam=="binomial") mle  <- glmer(c(Y) ~ X + (1|s) + (1|r), family=binomial(link=probit))
    startv$beta.d <- fixef(mle)[-1]
    startv$beta.u <- c(fixef(mle)[1], rep(0, ncol(X.u)-1))
    startv$s <- ranef(mle)$s[,1]
    startv$r <- ranef(mle)$r[,1]
    startv$z <- matrix(0, n, max(k,1))
  }
  
  
  if (is.null(priors)){
    pi.Sab<- c(1,0,1,4) # inv-wishart
    pi.s2u<- c(1, 1) # inv-gamma: appriori mean approx 1  
    pi.s2v<- c(1, 1) # inv-gamma: appriori mean approx 1
    pi.s2z<- matrix(c(1,1), max(k,1),2, byrow=TRUE) # inv-gamma for z's
    pim.bd<- rep(0, rd) 
    piS.bd<- 100*diag(rd)
    pim.b0sr<- rep(0, 1 + rs + rr) 
    piS.b0sr<- 100*diag(1 + rs + rr) 
  }
  
  beta.d<-startv$beta.d ; beta.u<-startv$beta.u 
  s<-startv$s ; r<-startv$r ; z<-startv$z;
  e <- z; f <- z;
  su <- sv <- pi.s2u[2]
  rho <-0;
  se <-1;
  
  
  ###Matrices for ANOVA  on Xd, s, r
  tmp<-TuTv(n)   #unit level effects design matrix for u=yij+yji, v=yij-yji
  Tu<-tmp$Tu
  Tv<-tmp$Tv
  
  tmp<-XuXv(Xd)   #regression design matrix for u=yij+yji, v=yij-yji
  Xu<-tmp$Xu  
  Xv<-tmp$Xv
  
  XTu<<-cbind(Xu,Tu)
  XTv<<-cbind(Xv,Tv)
  
  tXTuXTu<<-t(XTu)%*%XTu
  tXTvXTv<<-t(XTv)%*%XTv
  ###
  
  ###redefining hyperparams
  Sab0<-matrix( c(pi.Sab[1],pi.Sab[2],pi.Sab[2],pi.Sab[3]),nrow=2,ncol=2) 
  v0<-pi.Sab[4]   
  ###
  
  ###initializing error matrix
  E<-matrix(0,nrow=n,ncol=n)
  ###
  
  theta<- Y.hat <- Y 
  
  yhat <- uTv <- array(NA, dim=c((NS-burn)/odens, n, n)) 
  nst = 1
  
  ##### The Markov chain Monte Carlo algorithm
  cat("MCMC sampling", '\n')
  cat("Progress: ")
  
  for(ns in 1:NS){  
    
    main.time<-proc.time()
    
    ###impute any missing values 
    if(any(is.na(Y))) {  
      mu<-theta.betaX.d.srE.ef(beta.d,Xd,s,r,E*0,e,f) #predicted means
      theta.new=mat.vect(theta)
      mu.new=mat.vect(mu)
      t = which(is.na(theta.new[,1]) & is.na(theta.new[,2]))
      theta.new[t,] = rmnorm(length(t), 0, matrix(se*c(1,rho,rho,1),2,2)) + mu.new[t,]
      t=which(is.na(theta.new[,1]))
      theta.new[t,1] = rnorm(length(t), mu.new[t,1] + rho*(mu.new[t,2] - theta.new[t,2]), sqrt(se*(1-rho^2)))
      t=which(is.na(theta.new[,2]))
      theta.new[t,2] = rnorm(length(t), mu.new[t,2] + rho*(mu.new[t,1] - theta.new[t,1]), sqrt(se*(1-rho^2)))
      theta.new = vect.mat(theta.new)
      diag(theta.new) = diag(theta)
      theta = theta.new
      
      if(fam=="binomial") Y.hat[is.na(Y)] = 1*(theta[is.na(Y)]>0)                                
      
    }
    ## END IMPUTING MISSING VALUES  
    
    ## AUGMENT LATENT VARIABLES FOR THE PROBIT MODEL
    tau=c(-Inf, 0, Inf)  
    if(fam=="binomial"){ ## if using probit, generate latent variables theta 
      mu<-theta.betaX.d.srE.ef(beta.d,Xd,s,r,E*0,e,f)
      
      m = mat.vect(mu) 
      y = mat.vect(Y.hat)
      
      theta.l = rtnorm(length(m[,1]), m[,1], 1, lower=tau[y[,1]+1], upper=tau[y[,1]+2])
      theta.u = rtnorm(length(m[,2]), m[,2] + rho*(theta.l - m[,1]), sqrt(1-rho^2), lower=tau[y[,2]+1], upper=tau[y[,2]+2])
      
      theta = vect.mat(cbind(theta.l, theta.u))
    } 
    
    ###Update regression part
    tmp<-uv.E(theta-e%*%t(f)) #the regression part
    u<-tmp$u                  #u=yij+yji,  i<j
    v<-tmp$v                  #v=yij-yji,  i<j
    
    # Update SIGMA_gamma
    
    if (fam=="gaussian"){
      su<-rse.beta.d.gibbs(pi.s2u[1],pi.s2u[2],u,XTu,s,r,beta.d)  #for rho, se
      sv<-rse.beta.d.gibbs(pi.s2v[1],pi.s2v[2],v,XTv,s,r,beta.d)
      rho<-(su-sv)/(su+sv)
      se<-(su+sv)/4
    }
    rho.post <- function(z) {
      rho <- fisher.inv(z)
      sum(dnorm(E.star[,2], rho*E.star[,1],sqrt(1- rho^2), log=TRUE)) + dnorm(z, 0, 1, log=TRUE)
    }
    
    fisher <- function(rho) {.5*log((1+rho)/(1-rho))}
    fisher.inv <- function(z) {(exp(2*z) - 1)/(exp(2*z) + 1)}
    
    
    if (fam=="binomial") {
      E<-theta-theta.betaX.d.srE.ef(beta.d,Xd,s,r,E*0,e,f) # update error matrix
      E.star = mat.vect(E)
      cand = fisher(rho) + rnorm(1, 0, .05)
      a = min(0,  rho.post(cand) - rho.post(fisher(rho)))
      if (runif(1) < exp(a)) rho = fisher.inv(cand)
      se = 1
      sv = 2 - 2*rho
      su = 4 - sv
    }                 
    
    sr.hat<-X.u%*%beta.u    #Sab  
    a<-s-sr.hat[1:n]
    b<-r-sr.hat[n+(1:n)]
    Sab<-rSab.gibbs(a,b,Sab0,v0)
    
    #dyad specific regression coef and unit level effects
    mu<-c(pim.bd, X.u%*%beta.u)    #"prior" mean for (beta.d,s,r)
    beta.d.sr <- rbeta.d.sr.gibbs(u,v,su,sv,piS.bd,mu,Sab) 
    beta.d <- beta.d.sr$beta.d ; s<-beta.d.sr$s ; r<-beta.d.sr$r
    #regression coef for unit level effects
    beta.u<-rbeta.sr.gibbs(s,r,X.u,pim.b0sr,piS.b0sr,Sab)           
    
    ### bilinear effects Z
    if(k>0){
      
      #update variance
      s2e<-1/rgamma(k,pi.s2z[,1]+n/2,pi.s2z[,2]+diag( t(e)%*%e)/2)
      s2f<-1/rgamma(k,pi.s2z[,1]+n/2,pi.s2z[,2]+diag( t(f)%*%f)/2)
      
      #Gibbs for zs, using regression
      res<-theta-theta.betaX.d.srE.ef(beta.d,Xd,s,r,0*E,0*e,0*f) 
      s2u.res<-2*se*(1+rho)
      s2v.res<-2*se*(1-rho)
      
      for(i in sample(1:n)){
        u.res<-(res[i,-i]+res[-i,i])
        v.res<-(res[i,-i]-res[-i,i])
        
        alp.j<-cbind( f[-i,],e[-i,] )
        gam.j<-cbind( f[-i,],-e[-i,])
        
        Sef<-chol2inv(chol(   diag( 1/c(s2e,s2f),nrow=2*k)  +
          t(alp.j)%*%alp.j/s2u.res + t(gam.j)%*%gam.j/s2v.res ))
        
        muef<-Sef%*%( t(alp.j)%*%u.res/s2u.res +
          t(gam.j)%*%v.res/s2v.res  )
        ef<-t(rmvnorm(muef,Sef))
        e[i,]<-ef[1,1:k] ; f[i,]<-ef[1,k+1:k]
      }
    }
    
    ###
    if(k==0){sz<-NULL}
    
    if(ns > burn & (ns-burn)%%odens==0){  
      n.samp = (ns-burn)/odens
      E<-theta-theta.betaX.d.srE.ef(beta.d,Xd,s,r,E*0,e, f)
      tmp<-uv.E(theta-e%*%t(f)) #the regression part
      u<-tmp$u                  #u=yij+yji,  i<j
      v<-tmp$v                  #v=yij-yji,  i<j
      lpy.th<-sum(dnorm(u,0,sqrt(su),log=T) + dnorm(v,0,sqrt(sv),log=T))
      
      out<-round(c(k,ns,lpy.th,beta.d,beta.u,Sab[1,1],Sab[1,2],Sab[2,2],
                   se,rho,s2e[0:k], s2f[0:k]),oround)
      
      if(owrite==T) { 
        if(n.samp==1) { write.table(t(out),file=out.path, quote=F, row.names=F, col.names=cnames)}
        if(n.samp > 1)  { write.table(t(out),file=out.path, append=T, quote=F, row.names=F,col.names=F) }
      }
      
      if(owrite==F) {cat(out,"\n") }
      
      if(zwrite==T) { 
        write.table(signif(e,sdigz),file=u.path, append=T*(n.samp>1), quote=F, row.names=F, col.names=F) 
        write.table(signif(f,sdigz),file=v.path, append=T*(n.samp>1), quote=F, row.names=F, col.names=F)        
                      }
      
      if(awrite==T){
        write.table(round(t(a),oround), file=a.path, append=T*(n.samp>1), quote=F,row.names=F, col.names=F)  
      }
      
      if(bwrite==T){write.table(round(t(b),oround),
                                file=b.path, append=T*(n.samp>1), quote=F, row.names=F,col.names=F) 
      }
      
      yhat[nst,,] = theta - E
      uTv[nst,,] = e%*%t(f)
      nst = nst + 1
    } ## end of output function
    
    if(ns%%(NS/10)==0){cat(100*ns/NS,"%  ", sep="")}
    
  } ## end of MCMC function
  
  setwd(initwd) # comeback to the parent directory
  save(yhat, file=yhat.path)
  save(uTv, file=uTv.path)
  
} #####################End of main function : below are helper functions


#### SAMPLE Y from posterior predictive distribtuion
pred.y=function(theta, rho, se, fam="gaussian"){
  e = rmnorm(n*(n-1)/2, mean = c(0,0), varcov = se*matrix(c(1,rho,rho,1),2,2))
  E = theta
  E[t(lower.tri(E))] = e[,2]
  E = t(E)
  E[upper.tri(E)] = e[,1]
  Y = theta + E
  diag(Y) = NA
  if (fam=="binomial") Y = 1*(Y>0)
  Y
}


####
TuTv<-function(n){
  Xu<-Xv<-NULL
  for(i in 1:(n-1)){
    tmp<-tmp<-NULL
    if( i >1 ){ for(j in 1:(i-1)){ tmp<-cbind(tmp,rep(0,n-i)) } }
    tmp<-cbind(tmp,rep(1,n-i)) 
    tmpu<-cbind(tmp,diag(1,n-i)) ; tmpv<-cbind(tmp,-diag(1,n-i))
    Xu<-rbind(Xu,tmpu) ; Xv<-rbind(Xv,tmpv)
  }
  
  list(Tu=cbind(Xu,Xu),Tv=cbind(Xv,-Xv))
}
####

XuXv<-function(X){
  Xu<-Xv<-NULL
  if(dim(X)[3]>0){
    for(r in 1:dim(X)[3]){
      xu<-xv<-NULL
      for(i in 1:(n-1)){
        for(j in (i+1):n){ xu<-c(xu,X[i,j,r]+X[j,i,r])
                           xv<-c(xv,X[i,j,r]-X[j,i,r]) }}
      Xu<-cbind(Xu,xu)
      Xv<-cbind(Xv,xv)  } 
  }
  list(Xu=Xu,Xv=Xv)}

###
uv.E<-function(E){
  u<- c(  t( (  E + t(E) )  *UT ) )
  u<-u[!is.na(u)]
  v<-c(  t( (  E - t(E) )  *UT ) )
  v<-v[!is.na(v)]
  list(u=u,v=v)
}
####


####
theta.betaX.d.srE.z<-function(beta.d,X.d,s,r,E,z){
  m<-dim(X.d)[3]
  mu<-matrix(0,nrow=length(s),ncol=length(s))
  if(m>0){for(l in 1:m){ mu<-mu+beta.d[l]*X.d[,,l] }}
  tmp<-mu+re(s,r,E,z)
  diag(tmp)<-0
  tmp
  }
####


theta.betaX.d.srE.ef<-function(beta.d,X.d,s,r,E,e,f){
  m<-dim(X.d)[3]
  mu<-matrix(0,nrow=length(s),ncol=length(s))
  if(m>0){for(l in 1:m){ mu<-mu+beta.d[l]*X.d[,,l] }}
  tmp<-mu+reef(s,r,E,e,f)
  diag(tmp)<-0
  tmp}


####
re<-function(a,b,E,z){
  n<-length(a)
  matrix(a,nrow=n,ncol=n,byrow=F)+matrix(b,nrow=n,ncol=n,byrow=T)+E+z%*%t(z) 
}
####

reef<-function(a,b,E,e,f){
  n<-length(a)
  matrix(a,nrow=n,ncol=n,byrow=F)+matrix(b,nrow=n,ncol=n,byrow=T)+E+e%*%t(f) }
####


####
rbeta.d.sr.gibbs<-function(u,v,su,sv,piS.bd,mu,Sab){
  del<-Sab[1,1]*Sab[2,2]-Sab[1,2]^2
  iSab<-rbind(cbind( diag(rep(1,n))*Sab[2,2]/del ,-diag(rep(1,n))*Sab[1,2]/del),
              cbind( -diag(rep(1,n))*Sab[1,2]/del,diag(rep(1,n))*Sab[1,1]/del) )
  rd<-dim(as.matrix(piS.bd))[1]
  
  if(dim(piS.bd)[1]>0){
    cov.beta.sr<-matrix(0,nrow=rd,ncol=2*n)
    iS<-rbind(cbind(solve(piS.bd),cov.beta.sr),
              cbind(t(cov.beta.sr),iSab)) }
  else{iS<-iSab}
  
  Sig<-chol2inv(chol(iS + tXTuXTu/su + tXTvXTv/sv))
  #this may have a closed form expression
  
  M<-Sig%*%(t((u%*%XTu)/su + (v%*%XTv)/sv) + iS%*%mu)
  
  beta.sr<-rmvnorm(M, Sig)
  list(beta.d=beta.sr[(rd>0):rd],s=beta.sr[rd+1:n],r=beta.sr[rd+n+1:n]) }
####


####
rse.beta.d.gibbs<-function(g0,g1,x,XTx,s,r,beta.d){
  n<-length(s)
  1/rgamma(1, g0+choose(n,2)/2,g1+.5*sum( (x-XTx%*%c(beta.d,s,r))^2 ) ) }
####


####
rbeta.sr.gibbs<-function(s,r,X.u,pim.b0sr,piS.b0sr,Sab) {
  del<-Sab[1,1]*Sab[2,2]-Sab[1,2]^2
  iSab<-rbind(cbind( diag(rep(1,n))*Sab[2,2]/del ,-diag(rep(1,n))*Sab[1,2]/del),
              cbind( -diag(rep(1,n))*Sab[1,2]/del,diag(rep(1,n))*Sab[1,1]/del) )
  
  S<-solve( solve(piS.b0sr) + t(X.u)%*%iSab%*%X.u )
  mu<-S%*%(  solve(piS.b0sr)%*%pim.b0sr+ t(X.u)%*%iSab%*%c(s,r))
  rmvnorm( mu,S)
}
####

####
rSab.gibbs<-function(a,b,S0,v0){
  n<-length(a)
  ab<-cbind(a,b)
  Sn<-S0+ (t(ab)%*%ab)
  solve(rwish(solve(Sn),v0+n) )
}

####
rmvnorm<-function(mu,Sig2){
  R<-t(chol(Sig2))
  R%*%(rnorm(length(mu),0,1)) + mu}


####
rwish<-function(S0,nu){ 
  S<-S0*0
  for(i in 1:nu){ z<-rmvnorm(rep(0,dim(as.matrix(S0))[1]), S0)
                  S<-S+z%*%t(z)  }
  S 
}

###  Procrustes transformation: rotation and reflection
proc.rr<-function(Y,X){
  k<-dim(X)[2]
  A<-t(Y)%*%(X%*%t(X))%*%Y
  eA<-eigen(A,symmetric=T)
  Ahalf<-eA$vec[,1:k]%*%diag(sqrt(eA$val[1:k]),nrow=k)%*%t(eA$vec[,1:k])
  t(t(X)%*%Y%*%solve(Ahalf)%*%t(Y)) 
}

####
rbeta.d.s.gibbs<-function(u,su,piS.bd,mu,s2a){
  iSa<-diag(rep(1/s2a,n))
  
  if(dim(piS.bd)[1]>0){
    cov.beta.s<-matrix(0,nrow=rd,ncol=n)
    iS<-rbind(cbind(solve(piS.bd),cov.beta.s),
              cbind(t(cov.beta.s),iSa)) }
  else{iS<-iSa}
  
  Sig<-chol2inv( chol(iS + tXTuXTu/su ) )
  #this may have a closed form expression
  
  theta<-Sig%*%( t(  (u%*%XTu)/su) + iS%*%mu)
  
  beta.s<-rmvnorm(theta,Sig)
  list(beta.d=beta.s[(rd>0):rd],s=beta.s[rd+1:n]) }
####

####
rbeta.s.gibbs<-function(s,X.u,pim.b0s,piS.b0s,s2a) {
  iSa<-diag(rep(1/s2a,n))
  S<-solve( solve(piS.b0s) + t(X.u)%*%iSa%*%X.u )
  mu<-S%*%(  solve(piS.b0s)%*%pim.b0s+ t(X.u)%*%iSa%*%s)
  rmvnorm( mu,S)
}



mat.vect <- function(Y){ ## transform NxN matrix into N*(N-1)x2 vector
  Y.l = t(Y)[upper.tri(t(Y))]
  Y.u = Y[upper.tri(Y)]
  Y.new=cbind(Y.l, Y.u)
  Y.new
}

vect.mat <- function(Y){ ## back-transform N*(N-1)x2 vector into NxN matrix
  n = (sqrt((8*nrow(Y)+1))+1)/2
  M = matrix(NA, n, n)
  M[t(lower.tri(M))] = Y[,1]
  M = t(M)
  M[upper.tri(M)] = Y[,2] 
  M
}
###Preconditions of the main function "gbme"

##objects with no defaults
#Y : a square matrix (n*n)

##objects with defaults
#Xd : an n*n*rd array representing dyad-specific predictors
#Xs : an n*rs matrix of sender-specific predictors
#Xr : an n*rr matrix of receiver-specific predictors

# fam : "gaussian" "binomial", or "poisson"
# if fam="binomial" then an (n*n) matrix N of trials is needed
#
# pi.Sab = (s2a0,sab0,s2b0,v0) : parameters of inverse wishart dist
# pi.s2u = vector of length 2: parameters of an inv-gamma distribution
# pi.s2v = vector of length 2: parameters of an inv-gamma distribution
# pi.s2z = k*2 matrix : parameters of k inv-gamma distributions
#
# pim.bd  : vector of length rd, the prior mean for beta.d
# piS.bd  : a rd*rd pos def matrix, the prior covariance for beta.d
#
# pim.b0sr : vector of length 1+rs+rr, prior mean for (beta0,beta.s,beta.r)
# piS.b0sr  : a (1+rs+rr)*(1+rs+rr) pos def matrix, 
#             the prior variance for (beta0,beta.s,beta.r)


# starting values
# beta.d   #a vector of length rd
# beta.u   #a vector of length 1+rs+rr
# s        #a vector of length n
# r        #a vector of length n 
# z        #an n*k matrix

###the main  function

library(magic)
library(msm)
library(lme4)
library(mnormt)


gbme<-function(
  Y=matrix(nrow(Y), ncol(Y)),  
  Xd=array(dim=c(nrow(Y),nrow(Y),0)),
  Xs=matrix(nrow=nrow(Y),ncol=0),
  Xr=matrix(nrow=nrow(Y),ncol=0), 
  
  #model specification
  fam="gaussian",         
  N=matrix(1, ncol(Y), ncol(Y)),
  k=0,
  priors=NULL, # the list of priors
  startv=NULL, # the list of starting values
  #random seed, 
  seed=0,
  burn=0, #the size of burn-in
  pred=FALSE, #sample from predicted distribution; NO by default
  
  #details of output
  NS=100,         #number of scans of mcmc to run
  odens=1,         #output density
  owrite=T,          #write output to a file? 
  ofilename="out",   #name of output file 
  oround=3,          #rounding of output
  zwrite=(k>0),      #write z to  file 
  zfilename="Z",     #outfile for z 
  sdigz=3,           #rounding for z
  awrite=F,
  bwrite=F,
  afilename="A",
  bfilename="B",
  write.all=FALSE,
  out.name=NULL
  )
{
  
  ###set seed
  set.seed(seed)                                                                 
  
  ###dimensions of everything
  n<<-ncol(Y)
  rd<-dim(Xd)[3]
  rs<-dim(Xs)[2]
  rr<-dim(Xr)[2]   
  diag(Y)<-rep(0,n)
  ###
  
  ## create a folder for the output
  
  ##
  if (write.all==TRUE){ 
    
    awrite=TRUE
    bwrite=TRUE
    zwrite=TRUE
    
    if (fam=="gaussian"){folder = paste("output", "y", sep=".")}
    if (fam=="binomial"){folder = paste("output", "m", sep=".")}
    
    initwd = getwd()
    wpath  = paste(getwd(),"/",folder, sep="")  
    dir.create(wpath)
    setwd(wpath)
    f.dir = function(x){  dir.create(paste(wpath,x, sep="/"))}
    f.path=function(x) {paste(wpath,x,paste(x, out.name, sep="."), sep="/")}
    
    sapply(c("out", "a", "b", "u", "v", "yhat"), f.dir)
    
    out.path  = f.path("out")
    a.path  = f.path("a")
    b.path  = f.path("b")
    u.path  = f.path("u")
    v.path = f.path("v")
    yhat.path = f.path("yhat")
    
  }
  
  ###column names for output file
  
  cnames<-c("k","scan","ll",paste("bd",seq(1,rd,length=rd),sep="")[0:rd],"b0",
            paste("bs",seq(1,rs,length=rs),sep="")[0:rs],
            paste("br",seq(1,rr,length=rr),sep="")[0:rr],
            "s2a","sab","s2b","s2e","rho", paste("s2z",seq(1,k,length=k),sep="")[0:k] )                           
  
  ###design matrix for unit-specific predictors
  
  X.u <- cbind(rep(.5,2*n), adiag(Xs,Xr))
  
  ###construct an upper triangular matrix (useful for later computations)
  tmp<-matrix(1,n,n)
  tmp[lower.tri(tmp,diag=T)]<-NA 
  UT<<-tmp
  ###
  
  if (is.null(startv)){
    X = apply(Xd, 3, c)
    s=rep(1:n,n)
    r=rep(1:n, each=n)
    cat("Using MLE to calculate starting values", '\n')
    options(warn=-1)
    if (fam=="gaussian") mle  <- glmer(c(Y) ~ X + (1|s) + (1|r))
    if (fam=="binomial") mle  <- glmer(c(Y) ~ X + (1|s) + (1|r), family=binomial(link=probit))
    startv$beta.d <- fixef(mle)[-1]
    startv$beta.u <- c(fixef(mle)[1], rep(0, ncol(X.u)-1))
    startv$s <- ranef(mle)$s[,1]
    startv$r <- ranef(mle)$r[,1]
    startv$z <- matrix(0, n, max(k,1))
  }
  
  
  if (is.null(priors)){
    pi.Sab<- c(100,0,100,4) # inv-wishart
    pi.s2u<- c(2, 1/.1) # inv-gamma  
    pi.s2v<- c(2, 1/.1) # inv-gamma
    pi.s2z<- matrix(c(2,1/.1),max(k,1),2, byrow=TRUE) # inv-gamma for z's
    pim.bd<- rep(0, rd) 
    piS.bd<- 100*diag(rd)
    pim.b0sr<- rep(0, 1 + rs + rr) 
    piS.b0sr<- 100*diag(1 + rs + rr) 
  }
  
  beta.d<-startv$beta.d ; beta.u<-startv$beta.u 
  s<-startv$s ; r<-startv$r ; z<-startv$z
  su <- sv <- pi.s2u[2]
  rho <-0
  se <-1
  
  
  ###Matrices for ANOVA  on Xd, s, r
  tmp<-TuTv(n)   #unit level effects design matrix for u=yij+yji, v=yij-yji
  Tu<-tmp$Tu
  Tv<-tmp$Tv
  
  tmp<-XuXv(Xd)   #regression design matrix for u=yij+yji, v=yij-yji
  Xu<-tmp$Xu  
  Xv<-tmp$Xv
  
  XTu<<-cbind(Xu,Tu)
  XTv<<-cbind(Xv,Tv)
  
  tXTuXTu<<-t(XTu)%*%XTu
  tXTvXTv<<-t(XTv)%*%XTv
  ###
  
  ###redefining hyperparams
  Sab0<-matrix( c(pi.Sab[1],pi.Sab[2],pi.Sab[2],pi.Sab[3]),nrow=2,ncol=2) 
  v0<-pi.Sab[4]   
  ###
  
  ###initializing error matrix
  E<-matrix(0,nrow=n,ncol=n)
  ###
  
  theta<- Y.hat <- Y 
  
  yhat=array(0, dim=c((NS-burn)/odens, n, n)) 
  nst = 1
  
  ##### The Markov chain Monte Carlo algorithm
  cat("MCMC sampling", '\n')
  cat("Progress: ")
  
  for(ns in 1:NS){  
    
    main.time<-proc.time()
    
    ###impute any missing values 
    if(any(is.na(Y))) {  
      mu<-theta.betaX.d.srE.ef(beta.d,Xd,s,r,E*0,e, f) #predicted means
      theta.new=mat.vect(theta)
      mu.new=mat.vect(mu)
      t = which(is.na(theta.new[,1]) & is.na(theta.new[,2]))
      theta.new[t,] = rmnorm(length(t), 0, matrix(se*c(1,rho,rho,1),2,2)) + mu.new[t,]
      t=which(is.na(theta.new[,1]))
      theta.new[t,1] = rnorm(length(t), mu.new[t,1] + rho*(mu.new[t,2] - theta.new[t,2]), sqrt(se*(1-rho^2)))
      t=which(is.na(theta.new[,2]))
      theta.new[t,2] = rnorm(length(t), mu.new[t,2] + rho*(mu.new[t,1] - theta.new[t,1]), sqrt(se*(1-rho^2)))
      theta.new = vect.mat(theta.new)
      diag(theta.new) = diag(theta)
      theta = theta.new
      
      if(fam=="binomial") Y.hat[is.na(Y)] = 1*(theta[is.na(Y)]>0)                                
      
    }
    ## END IMPUTING MISSING VALUES  
    
    ## AUGMENT LATENT VARIABLES FOR THE PROBIT MODEL
    tau=c(-Inf, 0, Inf)  
    if(fam=="binomial"){ ## if using probit, generate latent variables theta 
      mu<-theta.betaX.d.srE.ef(beta.d,Xd,s,r,E*0,e,f)
      
      m = mat.vect(mu) 
      y = mat.vect(Y.hat)
      
      theta.l = rtnorm(length(m[,1]), m[,1], 1, lower=tau[y[,1]+1], upper=tau[y[,1]+2])
      theta.u = rtnorm(length(m[,2]), m[,2] + rho*(theta.l - m[,1]), sqrt(1-rho^2), lower=tau[y[,2]+1], upper=tau[y[,2]+2])
      
      theta = vect.mat(cbind(theta.l, theta.u))
    } 
    
    ###Update regression part
    tmp<-uv.E(theta-z%*%t(z)) #the regression part
    u<-tmp$u                  #u=yij+yji,  i<j
    v<-tmp$v                  #v=yij-yji,  i<j
    
    # Update SIGMA_gamma
    
    if (fam=="gaussian"){
      su<-rse.beta.d.gibbs(pi.s2u[1],pi.s2u[2],u,XTu,s,r,beta.d)  #for rho, se
      sv<-rse.beta.d.gibbs(pi.s2v[1],pi.s2v[2],v,XTv,s,r,beta.d)
      rho<-(su-sv)/(su+sv)
      se<-(su+sv)/4
    }
    rho.post <- function(z) {
      rho <- fisher.inv(z)
      sum(dnorm(E.star[,2], rho*E.star[,1],sqrt(1- rho^2), log=TRUE)) + dnorm(z, 0, 1, log=TRUE)
    }
    
    fisher <- function(rho) {.5*log((1+rho)/(1-rho))}
    fisher.inv <- function(z) {(exp(2*z) - 1)/(exp(2*z) + 1)}
    
    
    if (fam=="binomial") {
      E<-theta-theta.betaX.d.srE.ef(beta.d,Xd,s,r,E*0,e, f) # update error matrix
      E.star = mat.vect(E)
      cand = fisher(rho) + rnorm(1, 0, .05)
      a = min(0,  rho.post(cand) - rho.post(fisher(rho)))
      if (runif(1) < exp(a)) rho = fisher.inv(cand)
      se = 1
      sv = 2 - 2*rho
      su = 4 - sv
    }                 
    
    sr.hat<-X.u%*%beta.u    #Sab  
    a<-s-sr.hat[1:n]
    b<-r-sr.hat[n+(1:n)]
    Sab<-rSab.gibbs(a,b,Sab0,v0)
    
    #dyad specific regression coef and unit level effects
    mu<-c(pim.bd, X.u%*%beta.u)    #"prior" mean for (beta.d,s,r)
    beta.d.sr <- rbeta.d.sr.gibbs(u,v,su,sv,piS.bd,mu,Sab) 
    beta.d <- beta.d.sr$beta.d ; s<-beta.d.sr$s ; r<-beta.d.sr$r
    #regression coef for unit level effects
    beta.u<-rbeta.sr.gibbs(s,r,X.u,pim.b0sr,piS.b0sr,Sab)           
    
    ### bilinear effects Z
    if(k>0){
      
      #update variance
    
      s2e<-1/rgamma(k,pi.s2z[,1]+n/2,pi.s2z[,2]+diag( t(e)%*%e)/2)
      s2f<-1/rgamma(k,pi.s2z[,1]+n/2,pi.s2z[,2]+diag( t(f)%*%f)/2)
      
      #Gibbs for zs, using regression
      res<-theta-theta.betaX.d.srE.ef(beta.d,Xd,s,r,0*E,0*e, 0*f) 
      
      s2u.res<-2*se*(1+rho)
      s2v.res<-2*se*(1-rho)
      
      
      for(i in sample(1:n))      {
        u.res<-(res[i,-i]+res[-i,i])
        v.res<-(res[i,-i]-res[-i,i])
        
        alp.j<-cbind( f[-i,],e[-i,] )
        gam.j<-cbind( f[-i,],-e[-i,])
        
        Sef<-chol2inv(chol(   diag( 1/c(s2e,s2f),nrow=2*k)  +
          t(alp.j)%*%alp.j/s2u.res + t(gam.j)%*%gam.j/s2v.res ))
        
        muef<-Sef%*%( t(alp.j)%*%u.res/s2u.res +
          t(gam.j)%*%v.res/s2v.res  )
        ef<-t(rmvnorm(muef,Sef))
        e[i,]<-ef[1,1:k] ; f[i,]<-ef[1,k+1:k]
      }
    }
    
    ###
    if(k==0){sz<-NULL}
    
    if(ns > burn & (ns-burn)%%odens==0){  

      n.samp = (ns-burn)/odens
      E<-theta-theta.betaX.d.srE.ef(beta.d,Xd,s,r,E*0,e,f)
      tmp<-uv.E(theta-z%*%t(z)) #the regression part
      u<-tmp$u                  #u=yij+yji,  i<j
      v<-tmp$v                  #v=yij-yji,  i<j
      lpy.th<-sum(dnorm(u,0,sqrt(su),log=T) + dnorm(v,0,sqrt(sv),log=T))
      
out<-round(c(k,ns,lpy.th,beta.d,beta.u,Sab[1,1],Sab[1,2],Sab[2,2],
                   se,rho,s2e[0:k],s2f[0:k]),oround)
      
      if(owrite==T) { 
        if(n.samp==1) { write.table(t(out),file=out.path, quote=F, row.names=F, col.names=cnames)}
        if(n.samp > 1)  { write.table(t(out),file=out.path, append=T, quote=F, row.names=F,col.names=F) }
      }
      
      if(owrite==F) {cat(out,"\n") }
      if(zwrite==T) { write.table(signif(z,sdigz),file=z.path, append=T*(n.samp>1), quote=F, row.names=F, col.names=F) }
      
      
      if(awrite==T){
        write.table(round(t(a),oround), file=a.path, append=T*(n.samp>1), quote=F,
                    row.names=F, col.names=F)  
      }
      
      if(bwrite==T){write.table(round(t(b),oround),
                                file=b.path, append=T*(n.samp>1), quote=F, row.names=F,col.names=F) 
      }
      
      yhat[nst,,] = theta - E
      nst = nst + 1
    } ## end of output function
    
  
  #  if(ns%%(NS/10)==0){cat(100*ns/NS,"%%  ", sep="")}

    
  } ## end of MCMC function

  
  setwd(initwd) # comeback to the parent directory
  save(a, file="yhat")
  
  } 
#####################End of main function : below are helper functions


#### SAMPLE Y from posterior predictive distribtuion
pred.y=function(theta, rho, se, fam="gaussian"){
  e = rmnorm(n*(n-1)/2, mean = c(0,0), varcov = se*matrix(c(1,rho,rho,1),2,2))
  E = theta
  E[t(lower.tri(E))] = e[,2]
  E = t(E)
  E[upper.tri(E)] = e[,1]
  Y = theta + E
  diag(Y) = NA
  if (fam=="binomial") Y = 1*(Y>0)
  Y
}


####
TuTv<-function(n){
  Xu<-Xv<-NULL
  for(i in 1:(n-1)){
    tmp<-tmp<-NULL
    if( i >1 ){ for(j in 1:(i-1)){ tmp<-cbind(tmp,rep(0,n-i)) } }
    tmp<-cbind(tmp,rep(1,n-i)) 
    tmpu<-cbind(tmp,diag(1,n-i)) ; tmpv<-cbind(tmp,-diag(1,n-i))
    Xu<-rbind(Xu,tmpu) ; Xv<-rbind(Xv,tmpv)
  }
  
  list(Tu=cbind(Xu,Xu),Tv=cbind(Xv,-Xv))
}
####

XuXv<-function(X){
  Xu<-Xv<-NULL
  if(dim(X)[3]>0){
    for(r in 1:dim(X)[3]){
      xu<-xv<-NULL
      for(i in 1:(n-1)){
        for(j in (i+1):n){ xu<-c(xu,X[i,j,r]+X[j,i,r])
                           xv<-c(xv,X[i,j,r]-X[j,i,r]) }}
      Xu<-cbind(Xu,xu)
      Xv<-cbind(Xv,xv)  } 
  }
  list(Xu=Xu,Xv=Xv)}

###
uv.E<-function(E){
  u<- c(  t( (  E + t(E) )  *UT ) )
  u<-u[!is.na(u)]
  v<-c(  t( (  E - t(E) )  *UT ) )
  v<-v[!is.na(v)]
  list(u=u,v=v)
}
####


####
theta.betaX.d.srE.z<-function(beta.d,X.d,s,r,E,z){
  m<-dim(X.d)[3]
  mu<-matrix(0,nrow=length(s),ncol=length(s))
  if(m>0){for(l in 1:m){ mu<-mu+beta.d[l]*X.d[,,l] }}
  tmp<-mu+re(s,r,E,z)
  diag(tmp)<-0
  tmp
}
####

####
re<-function(a,b,E,z){
  n<-length(a)
  matrix(a,nrow=n,ncol=n,byrow=F)+matrix(b,nrow=n,ncol=n,byrow=T)+E+z%*%t(z) 
}
####

####
rbeta.d.sr.gibbs<-function(u,v,su,sv,piS.bd,mu,Sab){
  del<-Sab[1,1]*Sab[2,2]-Sab[1,2]^2
  iSab<-rbind(cbind( diag(rep(1,n))*Sab[2,2]/del ,-diag(rep(1,n))*Sab[1,2]/del),
              cbind( -diag(rep(1,n))*Sab[1,2]/del,diag(rep(1,n))*Sab[1,1]/del) )
  rd<-dim(as.matrix(piS.bd))[1]
  
  if(dim(piS.bd)[1]>0){
    cov.beta.sr<-matrix(0,nrow=rd,ncol=2*n)
    iS<-rbind(cbind(solve(piS.bd),cov.beta.sr),
              cbind(t(cov.beta.sr),iSab)) }
  else{iS<-iSab}
  
  Sig<-chol2inv(chol(iS + tXTuXTu/su + tXTvXTv/sv))
  #this may have a closed form expression
  
  M<-Sig%*%(t((u%*%XTu)/su + (v%*%XTv)/sv) + iS%*%mu)
  
  beta.sr<-rmvnorm(M, Sig)
  list(beta.d=beta.sr[(rd>0):rd],s=beta.sr[rd+1:n],r=beta.sr[rd+n+1:n]) }
####


####
rse.beta.d.gibbs<-function(g0,g1,x,XTx,s,r,beta.d){
  n<-length(s)
  1/rgamma(1, g0+choose(n,2)/2,g1+.5*sum( (x-XTx%*%c(beta.d,s,r))^2 ) ) }
####


####
rbeta.sr.gibbs<-function(s,r,X.u,pim.b0sr,piS.b0sr,Sab) {
  del<-Sab[1,1]*Sab[2,2]-Sab[1,2]^2
  iSab<-rbind(cbind( diag(rep(1,n))*Sab[2,2]/del ,-diag(rep(1,n))*Sab[1,2]/del),
              cbind( -diag(rep(1,n))*Sab[1,2]/del,diag(rep(1,n))*Sab[1,1]/del) )
  
  S<-solve( solve(piS.b0sr) + t(X.u)%*%iSab%*%X.u )
  mu<-S%*%(  solve(piS.b0sr)%*%pim.b0sr+ t(X.u)%*%iSab%*%c(s,r))
  rmvnorm( mu,S)
}
####

####
rSab.gibbs<-function(a,b,S0,v0){
  n<-length(a)
  ab<-cbind(a,b)
  Sn<-S0+ (t(ab)%*%ab)
  solve(rwish(solve(Sn),v0+n) )
}

####
rmvnorm<-function(mu,Sig2){
  R<-t(chol(Sig2))
  R%*%(rnorm(length(mu),0,1)) + mu}


####
rwish<-function(S0,nu){ 
  S<-S0*0
  for(i in 1:nu){ z<-rmvnorm(rep(0,dim(as.matrix(S0))[1]), S0)
                  S<-S+z%*%t(z)  }
  S 
}

###  Procrustes transformation: rotation and reflection
proc.rr<-function(Y,X){
  k<-dim(X)[2]
  A<-t(Y)%*%(X%*%t(X))%*%Y
  eA<-eigen(A,symmetric=T)
  Ahalf<-eA$vec[,1:k]%*%diag(sqrt(eA$val[1:k]),nrow=k)%*%t(eA$vec[,1:k])
  t(t(X)%*%Y%*%solve(Ahalf)%*%t(Y)) 
}

####
rbeta.d.s.gibbs<-function(u,su,piS.bd,mu,s2a){
  iSa<-diag(rep(1/s2a,n))
  
  if(dim(piS.bd)[1]>0){
    cov.beta.s<-matrix(0,nrow=rd,ncol=n)
    iS<-rbind(cbind(solve(piS.bd),cov.beta.s),
              cbind(t(cov.beta.s),iSa)) }
  else{iS<-iSa}
  
  Sig<-chol2inv( chol(iS + tXTuXTu/su ) )
  #this may have a closed form expression
  
  theta<-Sig%*%( t(  (u%*%XTu)/su) + iS%*%mu)
  
  beta.s<-rmvnorm(theta,Sig)
  list(beta.d=beta.s[(rd>0):rd],s=beta.s[rd+1:n]) }
####

####
rbeta.s.gibbs<-function(s,X.u,pim.b0s,piS.b0s,s2a) {
  iSa<-diag(rep(1/s2a,n))
  S<-solve( solve(piS.b0s) + t(X.u)%*%iSa%*%X.u )
  mu<-S%*%(  solve(piS.b0s)%*%pim.b0s+ t(X.u)%*%iSa%*%s)
  rmvnorm( mu,S)
}



mat.vect <- function(Y){ ## transform NxN matrix into N*(N-1)x2 vector
  Y.l = t(Y)[upper.tri(t(Y))]
  Y.u = Y[upper.tri(Y)]
  Y.new=cbind(Y.l, Y.u)
  Y.new
}

vect.mat <- function(Y){ ## back-transform N*(N-1)x2 vector into NxN matrix
  n = (sqrt((8*nrow(Y)+1))+1)/2
  M = matrix(NA, n, n)
  M[t(lower.tri(M))] = Y[,1]
  M = t(M)
  M[upper.tri(M)] = Y[,2] 
  M
}