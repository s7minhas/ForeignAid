###
alm.ALS<-function(Y,X,tol=1e-5,imax=100,verbose=FALSE,seed=1)
{
  set.seed(seed)
  Y0<-Y

  m<-dim(Y) ; p<-dim(X) ; K<-length(m)-1
  B<-list() ; for(k in 1:K){ B[[k]]<-rsan(c(m[k],p[k]))/prod(p) }

  EY<-tsum(X,B) 
  Y[is.na(Y0)]<-EY[is.na(Y0)]

  XS<-list() ; for(k in 1:K){ XS[[k]]<-apply(X,c(k,K+1),sum) } 

  i<-0 ; SSE1<-mean( (Y0-EY)^2,na.rm=TRUE ) ; SSE0<-Inf
  while( (SSE0-SSE1)/SSE1 > tol & i < imax )
  {
    SSE0<-SSE1

    for(k in sample(1:K))
    {
      E<-Y
      for(l in (1:K)[-k]){ E<-sweep(E,c(l,K+1),B[[l]]%*%XS[[l]] )}

      y<-apply(E,c(k,K+1),sum)
      x<-XS[[k]]

      Syx<-y%*%t(x)
      Sxx<-x%*%t(x) * prod(m[(1:K)[-k]])  

      eS<-eigen(Sxx) 
      ieV<-1/eS$val ; ieV[ieV==Inf]<-0 ; iSxx<-eS$vec%*%diag(ieV)%*%t(eS$vec)

      B[[k]]<-  Syx%*%iSxx
    }

    EY<-tsum(X,B) 
    Y[is.na(Y0)]<-EY[is.na(Y0)]

    i<-i+1
    E<-Y0 ; for(k in 1:K){ E<-sweep(E,c(k,K+1),B[[k]]%*%XS[[k]]) }
    SSE1<-mean(E^2,na.rm=TRUE)
    if(verbose){ cat(i, SSE1, (SSE0-SSE1)/SSE1,"\n") }
   }
   for(k in 1:K){ dimnames(B[[k]])<-list(dimnames(Y)[[k]],dimnames(X)[[k]]) }
B
}
### 


### 
tsum<-function(X,B)
{
  ### needs work
  m<-sapply(B,function(x){dim(x)[1] } )
  K<-length(m)
  XB<-array(0,dim=c(m,dim(X)[-(1:length(m))]))

  for(k in 1:K)
  {
    XB<-sweep(XB, c(k,K+1), B[[k]]%*%apply(X,c(k,K+1),sum),"+" )
  }
 XB
}
###



###
mlm.ALS<-function(Y,X,tol=1e-5,imax=100,verbose=FALSE,seed=NULL)
{
  Y0<-Y
  Y[is.na(Y)]<-0
  m<-dim(Y) ; p<-dim(X) ; K<-length(m)-1

  if(is.null(seed))
  {
    B0<-B<-list()
    for(k in 1:K)
    {
      B0[[k]]<-matrix(1/p[k],m[k],p[k])
      if(m[k]==p[k])  { B0[[k]]<-diag(m[k]) }
    }
    for(k in 1:K)
    {
      Xk<-mat( tprod(X,B0,(1:K)[-k]),k)
      Yk<-mat(Y,k)
      B[[k]]<- Yk%*%t(Xk)%*%solve(Xk%*%t(Xk))
    }
  }

  if(!is.null(seed))
  {
    set.seed(seed)
    B<-list() ; for(k in 1:K){ B[[k]]<-rsan(c(m[k],p[k]))/prod(p) }
  }

  EY<-tprod(X,B)
  Y[is.na(Y0)]<-EY[is.na(Y0)]

  i<-0 ; SSE1<-mean( (Y0-EY)^2,na.rm=TRUE ) ; SSE0<-Inf
  while( (SSE0-SSE1)/SSE1 > tol & i < imax )
  {
    SSE0<-SSE1

    for(k in sample(1:K))
    {
      Xk<-mat( tprod(X,B,(1:K)[-k]),k)
      Yk<-mat(Y,k)
      B[[k]]<- Yk%*%t(Xk)%*%solve(Xk%*%t(Xk))
    }

    EY<-tprod(X,B)
    Y[is.na(Y0)]<-EY[is.na(Y0)]

    i<-i+1
    SSE1<-mean( (Y0-EY)^2,na.rm=TRUE )
    if(verbose){ cat(i, SSE1, (SSE0-SSE1)/SSE1,"\n") }

  }
  for(k in 1:K){ dimnames(B[[k]])<-list(dimnames(Y)[[k]],dimnames(X)[[k]]) }
B
}
###

