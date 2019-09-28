source('setup.R')

###
load('intake/stratInterestMatrices.rda')

latSpaceGen = function(var){
  ###analysis of regression coef and variance components
  outName = paste0("intake/",var,"_2005_OUT")
  OUT<-read.table(outName,header=T)             #read in output
  
  ###analysis of latent positions
  zName = paste0("intake/",var,"_2005_Z")
  Z<-read.table(zName)
  
  ###  Procrustes transformation: rotation and reflection
  proc.rr<-function(Y,X){
    k<-dim(X)[2]
    A<-t(Y)%*%(  X%*%t(X)  )%*%Y
    eA<-eigen(A,symmetric=T)
    Ahalf<-eA$vec[,1:k]%*%diag(sqrt(eA$val[1:k]),nrow=k)%*%t(eA$vec[,1:k])
    t(t(X)%*%Y%*%solve(Ahalf)%*%t(Y)) }  
  
  #convert to an array
  nss<-dim(OUT)[1]
  n<-dim(Z)[1]/nss
  k<-dim(Z)[2]
  PZ<-array(dim=c(n,k,nss))
  for(i in 1:nss) { PZ[,,i]<-as.matrix(Z[ ((i-1)*n+1):(i*n) ,])  }
  
  PZ<-PZ[,,-(1:round(nss/2))]     #drop first half for burn in
  
  #find posterior mean of Z %*% t(Z)
  ZTZ<-matrix(0,n,n)
  for(i in 1:dim(PZ)[3] ) { ZTZ<-ZTZ+PZ[,,i]%*%t(PZ[,,i]) }
  ZTZ<-ZTZ/dim(PZ)[3] 
  
  #a configuration that approximates posterior mean of ZTZ
  tmp<-eigen(ZTZ)
  Z.pm<-tmp$vec[,1:k]%*%sqrt(diag(tmp$val[1:k]))
  
  #now transform each sample Z to a common orientation
  for(i in 1:dim(PZ)[3] ) { PZ[,,i]<-proc.rr(PZ[,,i],Z.pm) }
  
  # a two dimensional plot of "mean" latent locations
  # and marginal confidence regions
  r<-atan2(Z.pm[,2],Z.pm[,1])
  r<-r+abs(min(r))
  r<-r/max(r)
  g<-1-r
  b<-(Z.pm[,2]^2+Z.pm[,1]^2)
  b<-b/max(b)
  
  par(mfrow=c(1,1))
  plot(
    Z.pm[,1],Z.pm[,2],
    xlab="",ylab="",type="n",
    xlim=range(PZ[,1,]),
    ylim=range(PZ[,2,]),
    axes=FALSE
    )
  abline(h=0,lty=2);abline(v=0,lty=2)
  
  for(i in 1:n) { points( PZ[i,1,],PZ[i,2,],pch=46,col=rgb(r[i],g[i],b[i]) ) }
  
  cntries = countrycode(
    rownames(mats[[var]]),
    'cown', 'iso3c'
  )
  cntries[is.na(cntries)] = ''
  text(Z.pm[,1],Z.pm[,2], cntries, cex=.5)   #add labels here  
}

for(v in names(mats)){
  jpeg(paste0('floats/figure2_',v,'.jpg'))
  latSpaceGen(v)
  dev.off()
}