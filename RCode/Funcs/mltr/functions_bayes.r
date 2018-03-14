####
rBSm_fc<-function(Y, X, B0=NULL)
{ 
  if(!is.null(B0)) { Y<-Y-B0%*%X }

  m<-nrow(Y) ; p<-nrow(X) ; n<-ncol(Y) 

  iIXX<- solve( diag(p)+ tcrossprod(X) )
  YX<-tcrossprod(Y,X)
  YY<-tcrossprod(Y)

  iS<-rwish( solve(diag(m)+ YY-YX%*%iIXX%*%t(YX) ), m+1+n )
  iSh<-mhalf(iS)

  B<- YX%*%iIXX + solve(iSh)%*%rsan(c(m,p))%*%mhalf(iIXX)
 
  if(!is.null(B0)) { B<-B+B0 } 

  list(B=B,iSh=iSh)
}
####


####
rBSa_fc<-function(Y,X,B,S,B0=NULL)
{ 
  
  K<-length(B) 
  iSh<-lapply(lapply(S,solve),mhalf) 

  ## simulate s2    
  s2<-1/rgamma(1,(length(Y)+1)/2,(sum(tprod(Y-tprod(X,B),iSh)^2)+1)/2)

  ## update mode specific parameters
  for(k in sample(1:K))
  {
    Yk<-mat( tprod(Y,iSh,modes=(1:K)[-k]), k)/sqrt(s2)
    Xk<-mat( tprod(X, mapply("%*%",iSh,B,SIMPLIFY=FALSE), 
                      modes=(1:K)[-k] ), k)/sqrt(s2)
    BS<-rBSm_fc(Yk,Xk,B0=B0[[k]])  
    B[[k]]<-BS$B ; iSh[[k]]<-BS$iSh 
  }  

  S<-lapply(lapply(iSh,solve),function(x){x%*%x})

  list(B=B,S=S,s2=s2)
}
####


####
B_scaled<-function(B)
{
  K<-length(B)  
  MB<-sapply(B, function(x){ sqrt(mean(x^2)) } )
  sB<- prod(MB)^(1/K)/MB 
  BS<-mapply("*",B,sB)  

  for(k in 1:(K-1))
  { 
    if(nrow(BS[[k]])==ncol(BS[[k]])){ sk<- sign(median(diag(BS[[k]]))) }
    if(nrow(BS[[k]])!=ncol(BS[[k]])){ sk<- sign(BS[[k]][1,1]) }
    BS[[k]]<-BS[[k]]*sk ; BS[[k+1]]<-BS[[k+1]]*sk
  }

  dimnames(BS)<-dimnames(B) 
  BS 
}
#### 


####
S_scaled<-function(S,s2)
{
  K<-length(S) 
  m<-sapply(S,nrow) 
  sS<-m/sapply(S,tr)  
  s2s<-s2/prod(sS) 
  SS<-mapply("*",S,sS)   
  dimnames(SS)<-dimnames(S) 
  list(S=SS,s2=s2s) 
}
####

