# Functions include:
# nulldplot 
# subsample
# concomp
# netdist 
# degrees 
# rcmeans
# circplot
# xnet
# netplot
# addlines
# el2sm
# sm2el
# gofstats 

# Data includes:
# dutchcollege
# addhealth3 
# addhealth9 
# YX_nrm
# butland_ppi
# conflict90s 
# comtrade 
# yeast 
# lazegalaw
# sampsonmonks
# coldwar
# casia
# gfriends
# htmanagers

#' @title Null distribution plot
#' 
#' @description 
#' A plot comparing a null distribution to a test statistic. 
#' 
#' @param to (scalar) The observed value of the test statistic. 
#' @param tH (vector) Values of the statistic from the null distribution. 
#' @param xlab (character) Label for the x-axis. 
#' @param ncol (character) Color of the null histogram. 
#' @param ocol (character) Color for the observed value. 
#' @param otype (character) Color for the observed value line type. 
#' @param xlim (vector) Range for the x-axis.  
#' @param \ldots Additional plotting characters. 
#' 
#' @details 
#' This function produces a plot that facilitates comparison 
#' between a distribution of values of a scalar (e.g. a null 
#' distribution of a statistic) to a particular value (e.g. 
#' the observed value of the statistic). 
#'  
#' @author Peter Hoff
#' 
#' @examples
#' nulldplot(2,rnorm(1000)) 
#'  
#' @export nulldplot
nulldplot<-function(to,tH,xlab="tH",ncol="lightblue",ocol="red",otype=1,
   xlim=range(c(to,tH)),...)
{ 
  hist(tH,xlim=xlim,main="",prob=TRUE,col=ncol,xlab=xlab,...)
  abline(v=to,col=ocol,lty=otype)
}



#' @title Sampling 
#' 
#' @description 
#' Subsample from a vector. 
#' 
#' @param x (vector) A vector of values to be sampled. 
#' @param size (integer) The number of values to sample. 
#' @param \ldots additional arguments taken by \code{sample}. 
#' 
#' @details 
#' This function is essentially the \code{sample} command but without the 
#' bug that sampling a vector \code{x} of length one returns a permutation 
#' of \code{1:x}. This function was taken from an old help file for \code{sample}.
#'  
#' @return
#' A vector. 
#' 
#' @author Peter Hoff
#' 
#' @examples
#' sample(1:4) 
#' subsample(1:4) 
#' 
#' sample(4)
#' subsample(4) 
#' 
#' @export subsample
subsample<-function(x, size, ...)
{
  if(length(x) <= 1) { if(!missing(size) && size == 0) x[FALSE] else x }
  else sample(x, size, ...)
}





#' @title Connected components of a network
#' 
#' @description 
#' Obtain the connected components of a network. 
#' 
#' @param Y (matrix) The sociomatrix. Any positive values of \code{Y} are taken 
#' to indicate a link. Negative values are not taken to be links. 
#'
#' @details 
#' Connected components are identified essentially by identifying the 
#' existence of directed paths between nodes. Note that the connected 
#' components of \code{Y} and \code{t(Y)} may differ. 
#'  
#' @return
#' A list in which each element is a vector of nodes that belong to a 
#' connected component. 
#' 
#' @author Peter Hoff
#' 
#' @examples
#' 
#' data(addhealth3) 
#' Y<-el2sm(addhealth3$E)
#' concomp(Y)  
#' concomp(t(Y)) 
#' concomp(Y+t(Y)) 
#' 
#' @export concomp
concomp<-function(Y) 
{
  Y0<-1*(Y>0) ; diag(Y0)<-1 ; Y1<-Y0
  for(i in 1:dim(Y0)[1]) { Y1<-1*(Y1%*%Y0>0) }
  cc<-list() ; idx<-1:dim(Y1)[1]
  while(dim(Y1)[1]>0)
  {
    c1<-which(Y1[1,]==1)
    cc<-c(cc,list(idx[c1]))
    Y1<-Y1[ -c1,-c1,drop=FALSE ]  ; idx<-idx[-c1]
  }
  cc[ order(-sapply(cc,length))  ]
}




#' @title Network distance 
#' 
#' @description 
#' Minimal directed pathlength between nodes in a network. 
#' 
#' @param Y (matrix) The sociomatrix. Any positive values of \code{Y} are taken 
#' to indicate a link. Negative values are not taken to be links. 
#' @param countdown (logical) If true, the progress of the algorithm is 
#' printed out. 
#'
#' @details 
#' The directed distance between nodes of a binary network are computed, 
#' making use of the fact that the sociomatrix raised to a given power 
#' indicates the existence of paths of a given length between nodes. 
#'  
#' @return
#' A matrix of the same dimension as \code{Y}, giving the distance (minimal
#' path length) between nodes. 
#' 
#' @author Peter Hoff
#' 
#' @examples
#' 
#' data(addhealth3) 
#' Y<-el2sm(addhealth3$E)
#' netdist(Y) 
#' netdist(Y+t(Y)) 
#' 
#' @export netdist
netdist<-function(Y,countdown=FALSE)
{
  Y<-1*(Y>0) 
  n<-dim(Y)[1] ; Y0<-Y ; diag(Y0)<-0 ; Ys<-Y0
  D<-Y ; D[Y==0]<- n+1 ; diag(D)<-0
  s<-2
  while(any(D==n+1) & s<n)
  {
    Ys<-1*( Ys%*%Y0 >0 )
    D[Ys==1]<- ((s+D[Ys==1])-abs(s-D[Ys==1]))/2
    s<-s+1
    if(countdown){ cat(n-s,"\n") }
  }
  D[D==n+1]<-Inf
  D
}




#' @title Outdegree and indegrees
#' 
#' @description 
#' Calculation of the outdegrees and indegrees of a sociomatrix. 
#' 
#' @param Y (matrix) The sociomatrix. 
#' @param dichotomize (logical) If true, converts any non-zero entries of \code{Y} to 1 for calculation of the degrees. 
#'
#' @details 
#' The outdegree of a binary sociomatrix may be defined as the row sums 
#' of the non-diagonal elements of the sociomatrix. If the relations are 
#' valued, then the outdegree may be defined as either the outdegree of 
#' of a dichotomized version of the sociomatrix (\code{dichotomize=TRUE}),
#' or as the row sums of the original sociomatrix (\code{dichotomize=FALSE}).
#' Note that if non-diagonal elements of a row are missing, the degree for 
#' that row is missing as well. The indegrees are computed analogously. 
#' 
#' @return
#' A list consisting of the row degrees (\code{rdeg}) and the 
#' column degrees (\code{cdeg}). 
#' 
#' @author Peter Hoff
#' 
#' @examples
#' 
#' data(conflict90s) 
#' # For valued relations, 'dochotomize' makes a difference
#' degrees(conflict90s$conflicts) 
#' degrees(conflict90s$conflicts,dichotomize=TRUE)
#' 
#' @export degrees
degrees<-function(Y,dichotomize=TRUE)
{
  if(all(is.na(diag(Y)))) { diag(Y)<-0 }      
  if(dichotomize) { Y<-1*(Y!=0) }
  list(rdeg=apply(Y,1,sum), cdeg=apply(Y,2,sum) )
}

#' @title Row and column means
#' 
#' @description 
#' Computes the row and column means of a matrix. 
#'  
#' @param Y (matrix) a real-valued matrix. 
#' 
#' @details 
#' An alternative to calculating the degress, \code{rcmeans} 
#' computes the mean of the non-missing entries of each row and 
#' each column of \code{Y}. 
#' 
#' @return 
#' A list consisting of the row means (\code{rmean}) and the 
#' column means (\code{cmean}). 
#' 
#' @author Peter Hoff
#' 
#' @examples
#' data(comtrade) 
#' rcmeans(comtrade[,,1,1]) 
#' rcmeans(comtrade) 
#' 
#' @export rcmeans
rcmeans<-function(Y)
{  
  list(rmean=apply(Y,1,mean,na.rm=TRUE), cmean=apply(Y,2,mean,na.rm=TRUE) )
}

#' @title Circular network plot
#' 
#' @description Produce a circular network plot. 
#' 
#' @param Y (matrix) m by n relational matrix.
#' @param U (matrix) m by 2 matrix of row factors of Y.
#' @param V (matrix) n by 2 matrix of column factors of Y.
#' @param row.names (character vector) names of the row objects. 
#' @param col.names (character vector) names of the columns objects.
#' @param plotnames (logical) plot row and column names.
#' @param vscale (scalar) scaling factor for V coordinates.
#' @param pscale (scalar) scaling factor for plotting characters.
#' @param lcol (scalar or vector) line color(s) for the nonzero elements of Y.
#' @param rcol (scalar or vector) node color(s) for the rows.
#' @param ccol (scalar or vector) node color(s) for the columns.
#' @param pch (integer) plotting character.
#' @param lty (integer) line type.
#' @param jitter (scalar) a number to control jittering of nodes. 
#' @param bty (character) bounding box type.  
#' 
#' @details 
#' This function creates a circle plot of a relational matrix or social network.
#' If not supplied via \code{U} and \code{V}, two-dimensional row factors and 
#' column factors are computed from the SVD of \code{Y}, scaled versions of 
#' which are used to plot positions on the outside edge (\code{U}) and inside
#' edge (\code{V}) of the circle plot. The magnitudes of the plotting characters
#' are determined by the magnitudes of the rows of \code{U} and \code{V}. 
#' Segments are drawn between each row object \code{i} and column object 
#' \code{j} for which \code{Y[i,j]!=0}. 
#'  
#' @return
#' NULL
#' 
#' @author Peter Hoff
#' 
#' @examples
#' data(conflict90s) 
#' circplot(conflict90s$conflicts)
#' 
#' @export 
circplot<-function(Y,U=NULL,V=NULL,row.names=rownames(Y),col.names=colnames(Y),
                   plotnames=TRUE,vscale=.8,pscale=1.75,
                   lcol="gray",rcol="brown",ccol="blue",pch=16,lty=3,
                   jitter=.1*(nrow(Y)/(1+nrow(Y))) ,bty="n" )
{

  if(is.null(U))
  {
    a<-rowMeans(Y,na.rm=TRUE) ; b<-colMeans(Y,na.rm=TRUE)
    Y0<-Y ; Y0[is.na(Y)]<-(outer(a,b,"+"))[is.na(Y)] ; Y0<-Y0-mean(Y0)

    if(!all(Y==t(Y),na.rm=TRUE))
    {
      sY<-svd(Y0)
      U<-sY$u[,1:2] ; V<-sY$v[,1:2]
      mu<-sqrt( apply(U^2,1,sum) )
      mv<-sqrt( apply(V^2,1,sum) )
      u<-diag(1/mu)%*%U
      v<-diag(1/mv)%*%V*vscale
    }

    if( all(Y==t(Y),na.rm=TRUE) )
    {
      eY<-eigen(Y0)
      bv<-which(abs(eY$val)>= sort(abs(eY$val),decreasing=TRUE)[2])[1:2]
      U<-eY$vec[,bv]
      mu<-sqrt( apply(U^2,1,sum) )
      u<-diag(1/mu)%*%U
      mv<-mu ; v<-u
      ccol<-rcol
    }
  }

  if(!is.null(U))
  {
      mu<-sqrt( apply(U^2,1,sum) )
      mv<-sqrt( apply(V^2,1,sum) )
      u<-diag(1/mu)%*%U
      v<-diag(1/mv)%*%V*vscale
  } 

  ju<-1+jitter*(  rank(mu)/(nrow(Y)+1)  -.5 ) 
  u<-u*ju ; v<-v*ju 
  if(is.null(V)){ V<-U ;  mv<-mu ; v<-u ; ccol<-rcol }


  rsum<-apply(abs(Y),1,sum,na.rm=TRUE)
  csum<-apply(abs(Y),2,sum,na.rm=TRUE)

  par(mfrow=c(1,1),mar=c(1,1,1,1) )
  plot(u*1.2,type="n",xaxt="n",yaxt="n",xlab="",ylab="",bty=bty)
  links<-which(Y!=0, arr.ind = TRUE)
  segments( u[links[,1],1],u[links[,1],2],
            v[links[,2],1],v[links[,2],2], col=lcol,lty=lty)

  if(plotnames)
  { 
    if(is.null(row.names)){ row.names<-as.character(1:nrow(Y)) } 
    if(is.null(col.names)){ col.names<-as.character(1:rcol(Y)) } 
    text(u[rsum>0,] , row.names[rsum>0],cex=pscale*(mu[rsum>0])^.3,col=rcol)
    text(v[csum>0,] , col.names[csum>0],cex=pscale*(mv[csum>0])^.3,col=ccol)
  }

  if(!plotnames)
  {
    points(u[rsum>0,],cex=pscale*(mu[rsum>0])^.3,col=rcol,pch=pch)
    points(v[csum>0,],cex=pscale*(mv[csum>0])^.3,col=ccol,pch=pch)
  }
}




#' @title Network embedding
#' 
#' @description 
#' Compute an embedding of a sociomatrix into a two-dimensional space. 
#' 
#' @param Y (square matrix) The sociomatrix. 
#' @param fm (logical scalar) If TRUE, the Fruchterman-Reingold layout will 
#' be used (requires the network package). 
#' @param seed (integer) The random seed (the FR layout is random).  
#' 
#' @details 
#' Coordinates are obtained using the Fruchterman-Reingold layout if the 
#' package \code{network} is installed, and otherwise uses the first two 
#' eigenvectors the sociomatrix. 
#' 
#' @return (matrix) A matrix of two-dimensional coordinates. 
#' 
#' @author Peter Hoff
#' 
#' @examples
#' data(addhealth3) 
#' Y<-el2sm(addhealth3$E) 
#' X<-xnet(Y) 
#' netplot(Y,X) 
#' 
#' @export xnet
xnet<-function(Y,fm=suppressWarnings(require("network")),seed=1)
{
  if(!is.null(seed)) { set.seed(seed) }
  if(fm)
  {
    x<-as.network(Y)
    n <- network.size(x)
    d <- as.matrix.network(x, matrix.type = "adjacency")
    d[is.na(d)] <- 0
    d <- as.network(matrix(as.numeric(d > 0), n, n))
    U<-network.layout.fruchtermanreingold(d,layout.par=NULL)
  }
  if(!fm)
  {
    Y0<-Y ; diag(Y0)<-1
    U<-Re(eigen(Y0)$vec[,1:2])
  }
  U<-sweep(U,2,apply(U,2,mean)) ; U<-sweep(U,2,apply(U,2,sd),"/")
  U
}

#' Network plotting
#' 
#' Plot the graph of a sociomatrix 
#' 
#' @usage netplot(Y,X=NULL,xaxt="n",yaxt="n",xlab="",ylab="",
#'  lcol="gray",ncol="black",lwd=1,lty=1,pch=16,bty="n",plotnames=FALSE,
#'  seed=1,
#'  plot.iso=TRUE,directed=NULL,add=FALSE,...)
#' @param Y a sociomatrix 
#' @param X coordinates for plotting the nodes
#' @param xaxt x-axis type
#' @param yaxt y-axis type
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param lcol edge color
#' @param ncol node color (can be node-specific)
#' @param lwd  line width
#' @param lty  line type 
#' @param pch plotting character for nodes (can be node-specific) 
#' @param bty bounding box type
#' @param plotnames plot rownames of Y as node labels 
#' @param seed random seed
#' @param plot.iso include isolates in plot 
#' @param directed draw arrows
#' @param add add to an existing plot region
#' @param \ldots additional plotting parameters
#' @author Peter Hoff
#' @examples
#' data(addhealth3) 
#' Y<-el2sm(addhealth3$E) 
#' X<-xnet(Y) 
#' netplot(Y,X) 
#' 
#' @export netplot
netplot<-function(Y,X=NULL,xaxt="n",yaxt="n",xlab="",ylab="",
                  lcol="gray49",ncol="black",lwd=1,lty=1,pch=16,
                  bty="n",plotnames=FALSE,seed=1,plot.iso=TRUE,
                  directed=NULL,add=FALSE,...)
{
  if(nrow(Y)!=ncol(Y))
  { 
    if(is.null(directed)){Y0<-el2sm(Y);directed<-(sum(Y0*t(Y0),na.rm=TRUE)!=0)}
    Y<-el2sm(Y,directed)
  }

  if(is.null(X)) { X<-xnet(Y,seed=seed) }
  if(is.null(directed)){ directed<-!all(Y==t(Y),na.rm=TRUE) }

  if(!plot.iso)
  {
    deg<-apply(Y,1,sum,na.rm=TRUE) + apply(Y,2,sum,na.rm=TRUE)
    Y<-Y[deg>0,deg>0]
    X<-X[deg>0,]
  }

  if(!add){plot(X,type="n",xaxt=xaxt,yaxt=yaxt,xlab=xlab,ylab=ylab,bty=bty,...)}

  if(!directed) {addlines(Y,X,lwd=lwd,lty=lty,col=lcol,...)}
  if(directed){addlines(Y,X,lwd=lwd,lty=lty,col=lcol,arr.length=.2,...) }

  if(!plotnames) {points(X[,1],X[,2],col=ncol,pch=pch,...) }
  if(plotnames)  
  {
    if(is.null(rownames(Y))) { rownames(Y)<-1:nrow(Y)  } 
    text(X[,1],X[,2],rownames(Y),col=ncol,...)
   }

}


#' Add lines 
#' 
#' Add lines to a network plot
#' 
#' @usage addlines(Y,X,col="lightblue",alength=0,...)
#' @param Y a sociomatrix 
#' @param X coordinates of nodes
#' @param col color of lines. Can be a vector of length equal to the number of edges to be drawn
#' @param alength length of arrows to be drawn
#' @param \ldots additional plotting parameters
#' @author Peter Hoff
#' @examples
#'
#' data(addhealth3) 
#' Y<-el2sm(addhealth3$E) 
#' X<-xnet(Y) 
#' netplot(Y,X) 
#' addlines(Y,X,col=Y[Y!=0]) 
#' 
#' @export addlines
addlines<-function(Y,X,col="lightblue",alength=0,...)
{
  # add links between nodes of a sociomatrix 
  links<-which(Y!=0,arr.ind=TRUE)
  links = links[ links[,1] != links[,2], ]
  suppressWarnings( 
  Arrows(X[links[,1],1],X[links[,1],2],X[links[,2],1],X[links[,2],2],
           col=col,length=alength,...) 
                   )
}


#' Edgelist to sociomatrix 
#' 
#' Construction of a sociomatrix from an edgelist
#' 
#' @usage el2sm(el,directed=TRUE,nadiag=all(el[,1]!=el[,2]))
#' @param el a matrix in which each row contains the indices of an edge and possibly the weight for the edge
#' @param directed if FALSE, then a relation is placed in both entry ij and ji of the sociomatrix, for each edge ij (or ji)
#' @param nadiag put NAs on the diagonal 
#' @return a sociomatrix 
#' @author Peter Hoff
#' @examples
#' 
#' Y<-matrix(rpois(10*10,.5),10,10) ; diag(Y)<-NA
#' E<-sm2el(Y) 
#' el2sm(E) - Y 
#' 
#' @export el2sm
el2sm<-function(el,directed=TRUE,nadiag=all(el[,1]!=el[,2]))
{  
  w<-rep(1,nrow(el)) 
  if(ncol(el)>2){ w<-el[,3] } 

  if( is.numeric(el) && all(round(el[,1:2])==el[,1:2])  ) { nodes<-1:max(el) } 
  if(!(is.numeric(el) && all(round(el[,1:2])==el[,1:2]))) 
  { 
    nodes<-sort(unique(c(el[,1:2]))) 
  }

  el<-cbind( match(el[,1],nodes) ,  match(el[,2],nodes) ) 

  n<-max(el[,1:2]) 
  sm <- matrix(0,n,n)            # construct sociomatrix 
  sm[el[,1:2]]<-w                # fill in 
  if(nadiag) { diag(sm) <- NA  } # set diagonal to NA 
  if(!directed){ sm<-sm+t(sm) } 
  dimnames(sm)[[1]]<-dimnames(sm)[[2]]<-nodes 
  sm 
}


#' Sociomatrix to edgelist
#' 
#' Construction of an edgelist from a sociomatrix
#' 
#' @usage sm2el(sm,directed=TRUE)
#' @param sm a sociomatrix with possibly valued relations
#' @param directed if TRUE, only use the upper triangular part of the matrix to enumerate edges 
#' @return an edglist 
#' @author Peter Hoff
#' @examples
#' 
#' Y<-matrix(rpois(10*10,.5),10,10) ; diag(Y)<-NA
#' E<-sm2el(Y) 
#' el2sm(E) - Y 
#' 
#' @export sm2el
sm2el<-function(sm,directed=TRUE)
{ 
  if(!directed){ sm[lower.tri(sm)]<-0 }
  el<-which(sm!=0,arr.ind=TRUE) 
  w<-sm[el]  
  if(var(w)>0) { el<-cbind(el,w) }  
  el[order(el[,1]),] 
}


#' Goodness of fit statistics
#' 
#' Goodness of fit statistics evaluating second and third-order dependence
#' patterns
#' 
#' 
#' @usage gofstats(Y)
#' @param Y a relational data matrix
#' @return a vector of gof statistics
#' @author Peter Hoff
#' @examples
#' 
#' data(YX_nrm) 
#' 
#' gofstats(YX_nrm$Y) 
#' 
#' @export gofstats
gofstats<-function(Y)
{
  sd.rowmean<-sd(rowMeans(Y,na.rm=TRUE) ,na.rm=TRUE)
  sd.colmean<-sd(colMeans(Y,na.rm=TRUE) ,na.rm=TRUE)

  dyad.dep<- cor( c(Y),c(t(Y)) , use="complete.obs")

  E<-Y-mean(Y,na.rm=TRUE) ;  D<-1*(!is.na(E)) ; E[is.na(E)]<-0
  triad.dep<- sum(diag(E%*%E%*%E))/( sum(diag(D%*%D%*%D)) * sd(c(Y),na.rm=TRUE)^3)

  gof<-c(sd.rowmean,sd.colmean, dyad.dep , triad.dep )
  names(gof)<-c("sd.rowmean","sd.colmean","dyad.dep","triad.dep")
  gof
}

################################################## Data

#' @title Dutch college data
#'
#' @description
#' Longitudinal relational measurements and nodal characteristics 
#' of Dutch college students, described in 
#' van de Bunt, van Duijn, and Snijders (1999). 
#' 
#' @format A list consisting of a socioarray \code{Y} and a matrix 
#' \code{X}  of static nodal attributes. The relational 
#' measurements range from -1 to 4, indicating the following:
#' \itemize{
#' \item -1 a troubled or negative relationship
#' \item  0 don't know
#' \item  1 neutral relationship
#' \item  2 friendly
#' \item  3 friendship
#' \item  4 best friends
#' }
#' 
#' @source \url{http://moreno.ss.uci.edu/data.html#vdb}
#' 
#' @name dutchcollege
NULL


#' AddHealth community 3 data
#'
#' A valued edgelist (E) and matrix of nodal attributes (X) for 
#' students in community 3 of the AddHealth study. 
#' \itemize{
#' \item E: An edge list in which the value of the edge corresponds to an ad-hoc measure of intensity of the relation. Note that students were only allowed to nominate up to 5 male friends and 5 female friends. 
#' \item X: Matrix of students attributes, including sex, race (1=white, 2=black, 3=hispanic, 4=asian, 5=mixed/other) and grade. 
#' } 
#' See \url{http://moreno.ss.uci.edu/data.html#adhealth} for more details. 
#' @docType data
#' @keywords datasets
#' @format list 
#' @name addhealth3 
#' @usage data(addhealth3)
NULL

#' AddHealth community 9 data
#'
#' A valued edgelist (E) and matrix of nodal attributes (X) for 
#' students in community 9 of the AddHealth study. 
#' \itemize{
#' \item E: An edge list in which the value of the edge corresponds to an ad-hoc measure of intensity of the relation. Note that students were only allowed to nominate up to 5 male friends and 5 female friends. 
#' \item X: Matrix of students attributes, including sex, race (1=white, 2=black, 3=hispanic, 4=asian, 5=mixed/other) and grade. 
#' } 
#' See \url{http://moreno.ss.uci.edu/data.html#adhealth} for more details.
#' @docType data
#' @keywords datasets
#' @format list 
#' @name addhealth9 
#' @usage data(addhealth9)
NULL



#' Normal relational data and covariates. 
#' 
#' A synthetic dataset that includes continuous (normal) relational data as
#' well as information on eight covariates
#' 
#' @name YX_nrm
#' @docType data
#' @usage data(YX_nrm)
#' @format A list consisting of a sociomatrix \code{Y} and a matrix of nodal attributes \code{X}
#' @keywords datasets
NULL


#' Protein-protein interaction data. 
#' 
#' A network of protein-protein interactions (bindings) from 
#' Butland et al (2005) ``Interaction network containing conserved
#'     and essential protein complexes in Escherichia coli''. 
#' Obtained from 
#' \url{http://pil.phys.uniroma1.it/~gcalda/cosinsite/extra/data/proteins/}. 
#'
#' @name butland_ppi
#' @docType data
#' @usage data(butland_ppi)
#' @format An edgelist for a directed, binary relation 
#' @keywords datasets
#' @examples
#' 
#' data(butland_ppi)
#' gofstats(el2sm(butland_ppi))
#' 
#' 
NULL


#' @title Conflicts in the 90s
#'
#' @description 
#' A relational dataset recording the total number of militarized disputes
#' or events between countries in the 1990s, along with nodal and dyadic 
#' covariates. 
#'
#' @format A list consisting of a sociomatrix \code{conflicts}, an array of 
#' dyadic covariates \code{dyadvars} and nodal covariates \code{nodevars}. 
#' 
#' @source Michael Ward.
#' 
#' @name conflict90s
NULL


#' @title Comtrade data
#'
#' @description 
#' Summary of trade flows between countries over a ten year period. 
#'
#' @format 
#' A four-way array of yearly change in log trade between countries,
#' measured in 2000 US dollars across several commodity classes. The 
#' four dimensions of the array index exporting nation, importing nation, 
#' commidity and year, respectively. 
#' 
#' @source \url{http://comtrade.un.org/}
#' 
#' @name comtrade
NULL

#' @title Yeast protien interactome
#'
#' @description 
#' Protein-protein interaction network in the yeast S. cerevisiae. 
#'
#' @format 
#' A binary, directed sociomatrix indicating which proteins bind to other 
#' target protiens. Note that the diagonal is not missing. 
#' 
#' @source 
#' \url{http://interactome.dfci.harvard.edu/S_cerevisiae/download/Ito_core.txt}
#' 
#' @name yeast
NULL

#' @title Lazega's law firm data
#'
#' @description 
#' Several nodal and dyadic variables measured on 71 attorneys in a law firm. 
#'
#' @format 
#' A list consisting of a socioarray \code{Y} and a nodal attribute matrix \code{X}. 
#' 
#' The dyadic variables in \code{Y} include three binary networks: advice, friendship
#' and co-worker status. 
#' 
#' The categorical nodal attributes in \code{X} are coded as follows: 
#' \itemize{
#' \item status (1=partner, 2=associate)
#' \item office (1=Boston, 2=Hartford, 3=Providence) 
#' \item practice (1=litigation, 2=corporate) 
#' \item law school (1=Harvard or Yale, 2=UConn, 3=other)  
#'  }
#' \code{seniority} and \code{age} are given in years, and \code{female} is 
#' a binary indicator. 
#' 
#' @source 
#' \url{http://moreno.ss.uci.edu/data.html#lazega}
#' 
#' @name lazegalaw
NULL

#' @title Sampson's monastery data
#'
#' @description 
#' Several dyadic variables measured on 18 members of a monastery. 
#'
#' @format 
#' A socioarray whose dimensions represent nominators, nominatees and relations. 
#' Each monk was asked to rank up to three other monks on a variety of positive 
#' and negative relations. A rank of three indicates the "highest" ranking for 
#' a particular relational variable. The relations \code{like_m2} and \code{like_m1}
#' are evaluations of likeing at one and two timepoints previous to when the 
#' other relations were measured. 
#' 
#' @source 
#' \url{http://moreno.ss.uci.edu/data.html#sampson}
#' 
#' @name sampsonmonks
NULL

#' @title Read's highland tribe data
#'
#' @description 
#' Positive and negative relations between 16 New Guinean tribes. 
#'
#' @format 
#' A sixteen by sixteen by 2 socioarray
#' 
#' @source 
#' \url{http://http://vlado.fmf.uni-lj.si/pub/networks/data/ucinet/ucidata.htm#gama}
#' 
#' @name tribes
NULL

#' @title Cold War data
#'
#' @description 
#' Positive and negative relations between countries during the cold war
#'
#' @format 
#' A list including the following dyadic and nodal variables:
#' \itemize{
#' \item \code{cc}: a socioarray of ordinal levels of military 
#' cooperation (positive) and conflict (negative), every 5 years; 
#' \item \code{distance}: between-country distance; 
#' \item \code{gdp}: country gdp every 5 years; 
#' \item \code{polity}: country polity every 5 years.  
#' }
#' @source 
#' Xun Cao : \url{http://polisci.la.psu.edu/people/xuc11}
#' 
#' @name coldwar
NULL



#' @title Central Asia Cooperation
#'
#' @description 
#' Counts of between-country military cooperation in Central Asia 
#'
#' @format 
#' A list including the following dyadic and nodal variables:
#' \itemize{
#' \item \code{coop}: a sociomatrix of counts of cooperative events; 
#' \item \code{dist}: between-country distance; 
#' \item \code{pop}: country population; 
#' }
#' @source 
#' Originally from the KEDS project, see 
#' P.D. Hoff (2005) "Bilinear mixed-effects models for dyadic data" for 
#' details. 
#' 
#' @name casia
NULL

#' @title Girls high school friendship data
#'
#' @description 
#' Directed inditicator of friendship between 144 high school girls 
#'
#' @format 
#' A list including the following dyadic and nodal variables:
#' \itemize{
#' \item \code{Y}: an undirected sociomatrix of friendship ties;
#' \item \code{X}: nodal covariates including grade and normalized gpa and smoking scores.
#' }
#' 
#' @name gfriends
NULL


#' @title Krackhardt's  high-tech managers 
#'
#' @description 
#' Self-reported binary relations among 21 managers of a high-tech
#' company, as well as several nodal attributes. 
#'
#' @format 
#' A list consisting of the following:
#' \itemize{
#' \item \code{Y}: a directed socioarray representing to whom the ego 
#'  goes to for advice, whom the ego considers a friend, and to whom the 
#'  ego reports to. 
#' \item \code{X}: nodal covariates including age, length of service, level in the corporate hierarchy and department. 
#' }
#' 
#' @source 
#' \url{http://moreno.ss.uci.edu/data.html#krackht}
#' @name htmanagers
NULL


#' @title Sheep dominance data
#'
#' @description 
#' Number of dominance encounters between 28 female bighorn sheep. 
#' Cell (i,j) records the number of times sheep i dominated sheep j. 
#' From Hass (1991). 
#'  
#' @format 
#' A list consisting of the following:
#' \itemize{
#' \item \code{dom}: a directed socioarray recording the number of 
#' dominance encounters. 
#' \item \code{age}: the age of each sheep in years. 
#' }
#' 
#' @source 
#' \url{http://moreno.ss.uci.edu/data.html#sheep}
#' @name sheep
NULL



