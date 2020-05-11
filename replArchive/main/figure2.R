setwd('C:/Users/Owner/Research/foreignAid/replArchive/main/')
source('setup.R')
################################################################

################################################################
load('intake/stratInterestMatrices.rda')
################################################################
var = names(mats)[1]
bw=FALSE
geo=TRUE
################################################################
# lat space plotter
latSpaceGen = function(var, bw=FALSE, geo=TRUE){
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

  # add in country abbreviations
  load('intake/panel.rda')
  panel = unique(panel[,c('cname','ccode')])
  rownames(panel) = panel$ccode
  panel = panel[rownames(mats[[var]]), ]

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
  if(!bw){
    # colors for plotting
    r<-atan2(Z.pm[,2],Z.pm[,1])
    r<-r+abs(min(r))
    r<-r/max(r)
    g<-1-r
    b<-(Z.pm[,2]^2+Z.pm[,1]^2)
    b<-b/max(b)
    for(i in 1:n) { points( PZ[i,1,],PZ[i,2,],pch=46,col=rgb(r[i],g[i],b[i]) ) }
  }
  if(bw){
    for(i in 1:n) { points( PZ[i,1,],PZ[i,2,],pch=46,col='lightgrey' ) }
  }
  if(!bw & geo){

    loadPkg('cshapes')
    cmap = wmap = cshp(date=as.Date('2001-1-1'))
    coords=coordinates(wmap)
    rownames(coords)=wmap$ISO1AL3

    # Create colors
    rlon = pi*coords[,1]/180
    rlat = pi*coords[,2]/180

    slon =  (rlon-min(rlon))/(max(rlon)-min(rlon))
    slat =  (rlat-min(rlat))/(max(rlat)-min(rlat))
    ccols = rgb( slon^2,slat^2,(1-sqrt(slon*slat))^2)
    cmap$mcolor = ccols

    # Generate legend map
    fname=paste0('floats/map',var,'.jpg')
    jpeg(file=fname)
    plot(cmap, col=cmap$mcolor)
    dev.off()

    #
    cmap$cname = countrycode(
      char(cmap$CNTRY_NAME),
      'country.name', 'country.name' )

    panel$mcolor = cmap$mcolor[match(panel$cname, cmap$cname)]
    panel$mcolor[is.na(panel$mcolor)] = 'grey'
    panel$abb = char(cmap$ISO1AL3)[match(panel$cname, cmap$cname)]
    panel$abb[is.na(panel$abb)] = ''

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
    for(i in 1:n) { points(
      PZ[i,1,],PZ[i,2,],
      pch=46,
      col=alpha(panel$mcolor[i], .4) ) }
  }

  #
  text(
    Z.pm[,1],
    Z.pm[,2],
    panel$abb, cex=.5, col='black')

}
################################################################

################################################################
# run fn to get lat space plots
for(v in names(mats)){
  jpeg(paste0('floats/figure2_',v,'.jpg'))
  latSpaceGen(v, bw=FALSE)
  dev.off()
}
################################################################
