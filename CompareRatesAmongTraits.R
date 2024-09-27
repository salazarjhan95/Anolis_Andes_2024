#Function to compare phylogenetic evolutionary rates among traits 
#using likelihood
#Dean C. Adams
#From Adams, D.C. 2013. Comparing evolutionary rates for different 
#phenotypic traits on a phylogeny using 
#likelihood. Systematic Biology. 62:181-192. 
# 6 April, 2013: Corrected null Rate matrix for TraitCov=F to be 
#diagonal matrix
CompareRates.multTrait<-
  function(phy,x,TraitCov=T,ms.err=NULL,ms.cov=NULL){
    #Compares LLik of R-matrix vs. LLik of R-matrix with constrained 
    #diagonal
    #TraitCov = TRUE assumes covariation among traits (default)
    #ms.err allows the incorporation of within-species measurement error. 
    #Input is a matrix of species (rows) by within-species 
    #variation for each trait (columns).
    #ms.cov allows the incorporation of within-species covariation between 
    #traits. Input is a matrix of species (rows) by within-species 
    #covariation for each pair of traits (columns). These must be 
    #provided in a specific order, beginning with covariation between trait 
    #1 
    #and the rest, then trait 2 and the rest, etc. For instance, for 4 
    #traits, the columns are: cov_12, cov_13, cov_14, cov_23, cov_24 
    #cov_34. 
    #Some calculations adapted from 'evol.vcv' in phytools (Revell, 2012)
    library(MASS)
    x<-as.matrix(x)
    N<-nrow(x)
    p<-ncol(x)
    C<-vcv.phylo(phy)
    C<-C[rownames(x),rownames(x)]
    if (is.matrix(ms.err)){ 
      ms.err<-as.matrix(ms.err[rownames(x),])}
    if (is.matrix(ms.cov)){ 
      ms.cov<-as.matrix(ms.cov[rownames(x),])}
    #Cholesky decomposition function for diagonal-constrained VCV
    build.chol<-function(b){
      c.mat<-matrix(0,nrow=p,ncol=p)
      c.mat[lower.tri(c.mat)] <- b[-1] 
      c.mat[p,p]<-exp(b[1])
      c.mat[1,1]<-sqrt(sum((c.mat[p,])^2))
      if(p>2){
        for (i in 2:(p-1)){
          c.mat[i,i]<-ifelse( (c.mat[1,1]^2-sum((c.mat[i,])^2) )>0,
                              sqrt(c.mat[1,1]^2-sum((c.mat[i,])^2)), 0)
        }}
      return(c.mat) 
    }
    #Fit Rate matrix for all traits: follows code of L. Revell (evol.vcv)
    a.obs<-colSums(solve(C))%*%x/sum(solve(C)) 
    D<-matrix(0,N*p,p)
    for(i in 1:(N*p)) for(j in 1:p) if((j-1)*N<i&&i<=j*N) 
      D[i,j]=1.0
    y<-as.matrix(as.vector(x))
    one<-matrix(1,N,1)
    R.obs<-t(x-one%*%a.obs)%*%solve(C)%*%(x-one%*%a.obs)/N
    if (TraitCov==F) #for TraitCov = F
    { R.obs<-diag(diag(R.obs),p) }
    #Calculate observed likelihood with or without measurement error
    LLik.obs<-ifelse(is.matrix(ms.err)==TRUE, 
                     -t(y-D%*%t(a.obs))%*%ginv((kronecker(R.obs,C)+ 
                                                  diag(as.vector(ms.err))))%*%(y-D%*%t(a.obs))/2-N*p*log(2*pi)/2- 
                       determinant((kronecker(R.obs,C)+ diag(as.vector(ms.err))))
                     $modulus[1]/2 , 
                     -t(y-D%*%t(a.obs))%*%ginv(kronecker(R.obs,C))%*%(y-D%*%t(a.obs))/
                       2-N*p*log(2*pi)/2- 
                       determinant(kronecker(R.obs,C))$modulus[1]/2
    ) 
    #Fit common rate for all traits; search over parameter space 
    sigma.mn<-mean(diag(R.obs)) #reasonable start value for diagonal
    #Within-species measurement error matrix
    if(is.matrix(ms.err)){m.e<-diag(as.vector(ms.err))}
    #Within-species measurement error and trait covariation matrix
    if (is.matrix(ms.err) && is.matrix(ms.cov)){
      within.spp<-cbind(ms.err,ms.cov)
      rc.label<-NULL
      for (i in 1:p){ rc.label<-rbind(rc.label,c(i,i)) }
      for (i in 1:p){
        for (j in 2:p){ if (i!=j && i<j){rc.label<-
          rbind(rc.label,c(i,j))} }}
      m.e<-NULL
      for (i in 1:p){
        tmp<-NULL
        for (j in 1:p){
          for (k in 1:nrow(rc.label)){
            if(setequal(c(i,j),rc.label[k,])==T) {tmp<-
              cbind(tmp,diag(within.spp[,k]))}
          }
        }
        m.e<-rbind(m.e,tmp)
      }
    }
    #likelihood optimizer for no trait covariation
    lik.covF<-function(sigma){ 
      R<-matrix(0,nrow=p,ncol=p)
      diag(R)<-sigma
      LLik<-ifelse(is.matrix(ms.err)==TRUE, 
                   -t(y-D%*%t(a.obs))%*%ginv((kronecker(R,C)+ m.e))%*%(y-D%*%t(a.obs))/2-N*p*log(2*pi)/2- 
                     determinant((kronecker(R,C)+ m.e))$modulus[1]/2 , 
                   -t(y-D%*%t(a.obs))%*%ginv(kronecker(R,C))%*%(y-D%*%t(a.obs))/2-
                     N*p*log(2*pi)/2- 
                     determinant(kronecker(R,C))$modulus[1]/2
      ) 
      if (LLik == -Inf) { LLikk <- -1e+10 }
      return(-LLik)
    }
    #likelihood optimizer with trait covariation
    lik.covT<-function(sigma){ 
      low.chol<-build.chol(sigma)
      R<-low.chol%*%t(low.chol)
      LLik<-ifelse(is.matrix(ms.err)==TRUE, 
                   -t(y-D%*%t(a.obs))%*%ginv((kronecker(R,C)+ m.e))%*%(y-D%*%t(a.obs))/2-N*p*log(2*pi)/2- 
                     determinant((kronecker(R,C)+ m.e))$modulus[1]/2 , 
                   -t(y-D%*%t(a.obs))%*%ginv(kronecker(R,C))%*%(y-D%*%t(a.obs))/2-
                     N*p*log(2*pi)/2- 
                     determinant(kronecker(R,C))$modulus[1]/2
      ) 
      if (LLik == -Inf) {LLikk <- -1e+10 }
      return(-LLik)
    }
    ##Optimize for no trait covariation
    if (TraitCov==F) 
    { model1<-optim(sigma.mn,fn=lik.covF,method="L-BFGS-B",lower=c(0.0))}
    ##Optimize with trait covariation
    R.offd<-rep(0,(p*(p-1)/2))
    if (TraitCov==T) 
    {model1<-optim(par=c(sigma.mn,R.offd),fn=lik.covT,method="L-BFGS-B")}
    #### Assemble R.constrained
    if (TraitCov==F){R.constr<-diag(model1$par,p)}
    if (TraitCov==T){ 
      chol.mat<-build.chol(model1$par)
      R.constr<-chol.mat%*%t(chol.mat)}
    if(model1$convergence==0)
      message<-"Optimization has converged."
    else
      message<-"Optim may not have converged. Consider changing 
start value or lower/upper limits."
    LRT<- (-2*((-model1$value-LLik.obs)))
    LRT.prob<-pchisq(LRT, (p-1),lower.tail=FALSE) #df = Nvar-1
    AIC.obs<- -2*LLik.obs+2*p+2*p #(2p twice: 1x for rates, 1x for 
    #anc. states)
AIC.common<- -2*(-model1$value)+2+2*p #(2*1: for 1 rate 2p for 
#anc. states)
return(list(Robs=R.obs, 
            Rconstrained=R.constr,Lobs=LLik.obs,Lconstrained=(-
                                                                model1$value),LRTest=LRT,Prob=LRT.prob,
            AICc.obs=AIC.obs,AICc.constrained=AIC.common,optimmessage=message)) 
  }