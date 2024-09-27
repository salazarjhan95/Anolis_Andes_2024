findCI<-function(phy,x){
  
  library(numDeriv)
  
  x<-as.matrix(x)
  
  N<-nrow(x)
  
  p<-ncol(x)
  
  C<-vcv.phylo(phy)
  
  C<-C[rownames(x),rownames(x)]
  
  a.obs<-colSums(solve(C))%*%x/sum(solve(C))  
  
  D<-matrix(0,N*p,p)
  
  for(i in 1:(N*p)) for(j in 1:p) if((j-1)*N<i&&i<=j*N) D[i,j]=1.0
  
  y<-as.matrix(as.vector(x))
  
  one<-matrix(1,N,1)
  
  R.obs<-t(x-one%*%a.obs)%*%solve(C)%*%(x-one%*%a.obs)/N
  
  lik<-function(R.obs,y,D,a.obs,C,N,p){
    
    logL1<--t(y-D%*%t(a.obs))%*%solve(kronecker(R.obs,C))%*%(y-D%*%t(a.obs))/2-N*p*log(2*pi)/2-determinant(kronecker(R.obs,C))$modulus[1]/2
    
  }
  
  H<-hessian(lik,R.obs,y=y,D=D,a.obs=a.obs,C=C,N=N,p=p)   #Hessian of likelihood fxn with parameters
  
  std.err.all<-sqrt(diag(solve(-H)))    #standard errors for parameters (NOTE: from full R matrix)
  
  std.err.all[is.na(std.err.all)]<-0.0
  
  std.err<-diag(matrix(std.err.all,nrow=p))/sqrt(N)    #divide sqrt(N) for std. err
  
  R.val<-diag(R.obs)
  
  error <- qnorm(0.975)*std.err  #Compute 95% CI and compare across traits
  
  CI.l <- R.val-error; CI.u <- R.val+error  #lower and upper CI
  
  return(list(R=R.obs,CI.Lo=CI.l,CI.Hi=CI.u))
  
}