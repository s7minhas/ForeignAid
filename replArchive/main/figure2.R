setwd('C:/Users/Owner/Research/foreignAid/replArchive/main/')
source('setup.R')
loadPkg('cshapes')
loadPkg('broom')
################################################################

################################################################
load('intake/stratInterestMatrices.rda')
################################################################
var = names(mats)[1]
bw=FALSE
geo=TRUE
makeMap=TRUE
################################################################
# lat space plotter
# latSpaceGen = function(var){
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

  #
  cmap = wmap = cshp(date=as.Date('2001-1-1'))
  coords=coordinates(wmap)
  rownames(coords)=wmap$ISO1AL3

  # Create colors
  rlon = pi*coords[,1]/180
  rlat = pi*coords[,2]/180

  slon =  (rlon-min(rlon))/(max(rlon)-min(rlon))
  slat =  (rlat-min(rlat))/(max(rlat)-min(rlat))
  ccols = rgb( slon^2,slat^2,(1-sqrt(slon*slat))^2)
  names(ccols) = rownames(coords)
  cmap$mcolor = ccols
  head(Z.pm)

  # Generate legend map
  ggmap = suppressWarnings(tidy(cmap, region='ISO1AL3'))
  map=ggplot() +
    geom_polygon(
      data=ggmap,
      aes(
        x=long,
        y=lat,
        group=group,
        fill=id
      )
    ) +
    coord_fixed(1.3) +
    theme_void() +
    scale_fill_manual(values=ccols) +
    guides(fill='none')

  #
  cmap$cname = countrycode(
    char(cmap$CNTRY_NAME),
    'country.name', 'country.name' )

  #
  panel$mcolor = cmap$mcolor[match(panel$cname, cmap$cname)]
  panel$mcolor[is.na(panel$mcolor)] = 'grey'
  panel$abb = char(cmap$ISO1AL3)[match(panel$cname, cmap$cname)]
  panel$abb[is.na(panel$abb)] = ''

  #
rownames(mats[[1]])
length(rownames(mats[[1]]))
dim(PZ)
  head(panel)
  dimnames(PZ)[[1]] = panel$abb[match(rownames(mats[[var]]), panel$ccode)]
  dimnames(PZ)[[2]] = c('X1', 'X2')
  toDrop=which(dimnames(PZ)[[1]]=='')
  PZ = PZ[-toDrop,,]

  #
  ggPos = lapply(
    dimnames(PZ)[[1]], function(ctry){
      slice=t(PZ[ctry,,])
      slice = data.frame(slice, stringsAsFactors=FALSE)
      slice$abb = ctry
      return(slice) } ) %>%
    do.call('rbind', .)

  #
  ggPos$abb = factor(ggPos$abb)
  ccols = ccols[levels(ggPos$abb)]

  #
  ggPosAvg = ggPos %>%
    dplyr::group_by(abb) %>%
    dplyr::summarize(
      X1=mean(X1),
      X2=mean(X2)
    )

  #
  latMap = ggplot(
    data=ggPos, aes(x=X1, y=X2, group=abb)) +
    geom_point(
      aes(color=abb),
      alpha=.05
    ) +
    scale_color_manual(values=ccols) +
    geom_text(
      data=ggPosAvg,
      aes(
        x=X1,y=X2,
        label=abb
      )
    ) +
    guides(
      color='none'
    ) +
    theme_void()

#   return(list(map,latMap))
# }
latMap
map
################################################################

################################################################
# run fn to get lat space plots
pps = lapply(names(mats), function(v){
  latSpaceGen(v)
})

lats = list(
  pps[[1]][[2]],
  pps[[2]][[2]],
  pps[[3]][[2]]
)
map = pps[[1]][[1]]
################################################################
