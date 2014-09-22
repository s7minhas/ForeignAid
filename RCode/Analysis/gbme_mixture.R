gbme.mix <- function(

  Y,  
  Xd=array(dim=c(nrow(Y),nrow(Y),0)),
  Xs=matrix(nrow=nrow(Y),ncol=0),
  Xr=matrix(nrow=nrow(Y),ncol=0), 
  
  #model specification
  N=matrix(1, ncol(Y), ncol(Y)),
  k=0,
  priors=NULL, # the list of priors
  startv=NULL, # the list of starting values
  #random seed, 
  seed=0,
  pred=FALSE, #sample from predicted distribution; NO by default
  
  #details of output
  NS=100,         #number of scans of mcmc to run
  odens=1,         #output density
  burn=0, #the size of burn-in
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
  ){
   
  cat("Zero inflated log-normal network model", '\n')
  cat("", '\n')
  
Y.star <- Y
n <- ncol(Y)
M     <- matrix(1, n, n)
M[Y==0] <- 0
M[is.na(Y)] <- NA
Y.star[Y==0] <- NA

cat("Fitting binary model", '\n')  
  
out.m <- gbme.ef(Y=M, Xd=Xd[[1]], Xs=Xs, Xr=Xr, fam="binomial", NS=NS, odens=odens, burn=burn, k=k, pred=pred, write.all=write.all, out.name=out.name) 

cat(" ",'\n')
cat(" ",'\n')

cat("Fitting continuous model", '\n')

out.y <- gbme.ef(Y=log(Y.star), Xd=Xd[[2]], Xs=Xs, Xr=Xr, fam="gaussian", NS=100, odens=1, burn=10, k=k, pred=pred, write.all=write.all, out.name=out.name)

diag(Y) <- NA  
out <- list(Y = Y, Xs = Xs, Xd = Xd)
 
class(out) <- "mcmc"
cat("", '\n')
cat("", '\n')
  
out
}



predict.mcmc <- function(object, ...){

  y <- exp(object$y$sim)*pnorm(object$m$sim)
  m <- apply(y, c(2,3), mean)
  se <- apply(y, c(2,3), sd)
  return(list(mean=m, se=se))
    
}

# Helper function for augmenting the design matrix with z's
get.Xd <- function(X, t){
  
  if(t==1) return(list(X, X))
  if(t > 1) {
    X = array(X, dim=c(ncol(X), ncol(X),2))
    X=list(X, X)
  }
  
  get.z <-function(z, t, k){
    rownames(z) <- colnames(z) <- rownames(out[[t-1]]$Y)
    z.new <- Y*0
    z.new[is.na(z.new)] <- 0
    set   <- intersect(rownames(z), rownames(z.new))
    z.new[set,set] <- z[set,set]
    if (t == 2) return(z.new)
    if (t >  2) {
      z.old <- (out[[t-1]]$Xd[[k]])[,,2]
      rownames(z.old) <- colnames(z.old) <- rownames(out[[t-1]]$Y)
      z.new[set, set] <- z.new[set, set] + z.old[set, set]   
      }
    z.new
  }  
  
  load(paste("output.m/uTv/uTv", yrs[t-1], sep="."))
  uTv.m = apply(uTv, 2:3, mean)
  load(paste("output.y/uTv/uTv", yrs[t-1], sep="."))
  uTv.y = apply(uTv, 2:3, mean)
  rm("uTv")
  
  X[[1]][,,2] <- get.z(uTv.m, t, 1)
  X[[2]][,,2] <- get.z(uTv.y, t, 2)
  
  X

}