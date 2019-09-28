source('setup.R')
################################################################

################################################################
load('intake/stratInterestMatrices.rda')
################################################################

################################################################
# lat space plotter
latSpaceGen = function(var){
  # read in gen out for dims
  outName = paste0("intake/",var,"_2005_OUT")
  OUT<-read.table(outName,header=T)             #read in output
  
  # pull in lat positions
  zName = paste0("intake/",var,"_2005_Z")
  Z<-read.table(zName)
  
  # proc transformation
  proc.rr<-function(Y,X){
    k<-dim(X)[2]
    A<-t(Y)%*%(  X%*%t(X)  )%*%Y
    eA<-eigen(A,symmetric=T)
    Ahalf<-eA$vec[,1:k]%*%diag(sqrt(eA$val[1:k]),nrow=k)%*%t(eA$vec[,1:k])
    t(t(X)%*%Y%*%solve(Ahalf)%*%t(Y)) }  
  
  # convert to an array
  nss<-dim(OUT)[1]
  n<-dim(Z)[1]/nss
  k<-dim(Z)[2]
  PZ<-array(dim=c(n,k,nss))
  for(i in 1:nss) { PZ[,,i]<-as.matrix(Z[ ((i-1)*n+1):(i*n) ,])  }
  
  # burn and turn
  PZ<-PZ[,,-(1:round(nss/2))]
  
  # mean latent positions
  ZTZ<-matrix(0,n,n)
  for(i in 1:dim(PZ)[3] ) { ZTZ<-ZTZ+PZ[,,i]%*%t(PZ[,,i]) }
  ZTZ<-ZTZ/dim(PZ)[3] 
  
  # a configuration that approximates posterior mean of ZTZ
  tmp<-eigen(ZTZ)
  Z.pm<-tmp$vec[,1:k]%*%sqrt(diag(tmp$val[1:k]))
  
  # proc so we can plot with variance
  for(i in 1:dim(PZ)[3] ) { PZ[,,i]<-proc.rr(PZ[,,i],Z.pm) }
  
  # colors for plotting
  r<-atan2(Z.pm[,2],Z.pm[,1])
  r<-r+abs(min(r))
  r<-r/max(r)
  g<-1-r
  b<-(Z.pm[,2]^2+Z.pm[,1]^2)
  b<-b/max(b)
  
  # empty plot with dims
  par(mfrow=c(1,1))
  plot(
    Z.pm[,1],Z.pm[,2],
    xlab="",ylab="",type="n",
    xlim=range(PZ[,1,]),
    ylim=range(PZ[,2,]),
    axes=FALSE
    )
  abline(h=0,lty=2);abline(v=0,lty=2)
  
  # add points to show variance
  for(i in 1:n) { points( PZ[i,1,],PZ[i,2,],pch=46,col=rgb(r[i],g[i],b[i]) ) }
  
  # add in country abbreviations
  cntries = countrycode(
    rownames(mats[[var]]),
    'cown', 'iso3c'
  )
  cntries[is.na(cntries)] = ''
  text(Z.pm[,1],Z.pm[,2], cntries, cex=.5)
}
################################################################

################################################################
# run fn to get lat space plots
for(v in names(mats)){
  jpeg(paste0('floats/figure2_',v,'.jpg'))
  latSpaceGen(v)
  dev.off()
}
################################################################