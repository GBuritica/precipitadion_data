
library(ExtDist)
library(extRemes)
library(evd)
library(latex2exp)


par(mfrow=c(3,3))

ville <- c("Brest", "Lanveoc", "Quimper", "Bormes", "Le Luc", "Hyeres", "Nancy", "Metz", "Roville")
region <- c("northwest", "south", "northeast")
for(i in 1:3){
   bu<- bu0[i]
   kp<- kp0[i]
   j <- 3
  #sample0  <-  mARMAX1(al=4,par0=c(0.7,0.8,0.8),n0=n,dep0=0.9,d0=3)
  for(ind in 1:3){
    quantiles  <- NULL
    quantiles2 <- NULL
    quantiles3 <- NULL
    ## Creates sample
    sample     <- na.omit(cbind( pre[index[,j],((i-1)*3+2)] , pre[index[,j],((i-1)*3+3)] , pre[index[,j],((i-1)*3+4)] ) )
    sample     <- na.omit(sample); n      <- length(sample[,1] )
    sample     <- sapply(1:n, function(k) max(sample[k, ind]) )
    
    ## Computes index of regular variation 
    alpha      <-   1/alphaestimator(sample,k1=kp)$xi
    if(alpha < 0) alpha <- 10
    
    ## Creates suma - max samples
    sumaalpha  <- sapply(1:floor(n/bu), function(k) sum(sample[((k-1)*bu + 1):(k*bu) ]^alpha) )
    
    ## Sorted
    sorted     <- sort(sample, decreasing=T)
    sortedsuma <- sort(sumaalpha, decreasing=T)
    
    ## A
    ymax       <- length(sortedsuma)
    quantiles  <-  rbind( quantiles, (sortedsuma[1:ymax])^(1/alpha))
    quantiles3 <- rbind(quantiles3, sorted[1:ymax] )
    
    plot(quantiles3[1,], quantiles[1,],pch=1,cex=1, xlab = "sample",ylab=TeX('$\\alpha$ - norm') , main = region[i] )
    mtext(ville[(i-1)*3 + ind] , cex =.8)
    abline(0,1, col="grey", lty=2)
    
    
    ## Repeats for the norm
    ind <- 1:3
    sample     <- na.omit(cbind( pre[index[,j],((i-1)*3+2)] , pre[index[,j],((i-1)*3+3)] , pre[index[,j],((i-1)*3+4)] ) )
    sample     <- na.omit(sample); n      <- length(sample[,1] )
    sample     <- sapply(1:n, function(k) max(sample[k, ind]) )
    
    alpha <-   1/alphaestimator(sample,k1=kp)$xi
    sumaalpha  <- sapply(1:floor(n/bu), function(k) sum(sample[((k-1)*bu + 1):(k*bu) ]^alpha) )
    maxalpha   <- sapply(1:floor(n/bu), function(k) max(sample[((k-1)*bu + 1):(k*bu) ]^alpha) )
    
    sorted     <- sort(sample, decreasing=T)
    sortedsuma <- sort(sumaalpha, decreasing=T)
    sortedmax  <- sort(maxalpha,decreasing=T)
    
    ymax <- length(sortedsuma)
    
    quantiles <- quantiles2 <- quantiles3 <- NULL    
    quantiles <-  rbind( quantiles, (sortedsuma[1:ymax])^(1/alpha))
    quantiles2 <- rbind(quantiles2, sortedmax[1:ymax]^(1/alpha))
    quantiles3 <- rbind(quantiles3, sorted[1:ymax] )
    
    #points( quantiles3[1,], quantiles2[1,],pch=16,cex=.8, col = "green")
    points( quantiles3[1,], quantiles[1,],pch=16,cex=.8, col = "skyblue2")
    
    abline(0,1, col="grey")
    
  }
}

par(mfrow=c(1,3))
ylim0 <- xlim0 <- c(44,140)
MULT <- F
bu0<- c(165,135,70)
kp0 <- c(350,150,350)
for(i in 3){
  #i <- 1
  bu<- bu0[i]
  kp<- kp0[i]
  j <- 3
  #sample0  <-  mARMAX1(al=4,par0=c(0.7,0.8,0.8),n0=n,dep0=0.9,d0=3)

  sample0     <- na.omit(cbind( pre[index[,j],((i-1)*3+2)] , pre[index[,j],((i-1)*3+3)] , pre[index[,j],((i-1)*3+4)] ) )
  sample0     <- na.omit(sample0); n      <- length(sample0[,1] )
  sample     <- sapply(1:n, function(k) max(sample0[k, ]) )
  records    <- sort(sample, decreasing=T)
  
  alpha      <-   1/alphaestimator(sample,k1=kp)$xi
  sumaalpha  <- sapply(1:floor(n/bu), function(k) sum(sample[((k-1)*bu + 1):(k*bu) ]^alpha) )
  ymax       <- length(sumaalpha)
  #mat        <- stableconfintervals(sumaalpha,alpha,ymax*1/min(moments))
  bb               <- stable.fit.mle.restricted(sumaalpha, c(1,1,0,0), c(1,1,0,0))
  
  
  ind        <- which(sample > records[floor( n*0.05) ] )
  moments    <- sapply(1:3, function(d) mean( (sample0[ind,d]/sample[ind] )^alpha ) )
  print(moments)
  
  nmax       <- floor(0.02*n)
  mat0      <-   stableconfintervals2(sumaalpha,alpha,moments)
  
  #mat        <- stableconfintervals(sumaalpha,alpha,moments)
  
  
  if(MULT){ for(ind in 1:3){
    ## Creates sample
    sample     <- na.omit(cbind( pre[index[,j],((i-1)*3+2)] , pre[index[,j],((i-1)*3+3)] , pre[index[,j],((i-1)*3+4)] ) )
    sample     <- na.omit(sample); n      <- length(sample[,1] )
    sample     <- sapply(1:n, function(k) max(sample[k, ind]) )
    
    ## Computes index of regular variation 
    alpha0      <-   1/alphaestimator(sample,k1=kp)$xi
    if(alpha0 < 0) alpha0 <- 10
    
    ## Creates suma - max samples
    sumaalpha0  <- sapply(1:floor(n/bu), function(k) sum(sample[((k-1)*bu + 1):(k*bu) ]^alpha0) )
    sorted      <-  sort(sample,decreasing=T)
    mat0        <- stableconfintervals(sumaalpha0,alpha0)
    ## Sorted
    
    
    ## A
    ymax       <- length(sumaalpha0)
    quantiles  <-  mat0[,2]
    quantiles3 <-  rev(sorted[1:ymax]) 
    quantiles4 <-  rev(sorted[1:ymax]) 
    #plot(NA,NA,pch=1,cex=1, xlab = "observations",ylab=TeX('$1/\\widehat{\\alpha}^n$ - stable quantiles'), main = region[i], ylim =ylim0,xlim = xlim0 )
    #polygon( x=c( quantiles4[1:ymax], rev(quantiles4[1:ymax]) ), y=c(  mat0[,1]^(1/alpha0), rev( mat0[,3]^(1/alpha0)) ), density=20, col = "lightblue" , angle = 90)
    #points(quantiles4, quantiles, col = 1, pch=1, cex=1)
    #abline(0,1, lty=2, col = "grey")
    #mtext("univariate", cex=0.8)
    
    
    sample0     <- na.omit(cbind( pre[index[,j],((i-1)*3+2)] , pre[index[,j],((i-1)*3+3)] , pre[index[,j],((i-1)*3+4)] ) )
    sample0     <- na.omit(sample0); n      <- length(sample0[,1] )
    sample     <- sapply(1:n, function(k) max(sample0[k, ]) )
    
    mom        <- moments[ind]
    mat        <- stableconfintervals(sumaalpha,alpha,mom)
    
    
    quantiles2 <- sapply(1:floor(ymax*mom), function(k) mat[k,2])
    ymax2       <- length(quantiles2)
    quantiles3 <-  rev(sorted[1:ymax2]) 
    
    plot(NA,NA,pch=1,cex=1, xlab = "observations",ylab=TeX('$1/\\widehat{\\alpha}^n$ - stable quantiles') , main = region[i], ylim =ylim0,xlim = xlim0 )
    points(quantiles4, quantiles, col = "grey", pch=1, cex=1)
    lines(quantiles4, mat0[,1], col="grey",lty=3);lines(quantiles4, mat0[,3], col = "grey",lty=3)
    
    #polygon( x=c( quantiles3[1:ymax2], rev(quantiles3[1:ymax2]) ), y=c( sapply(1:floor(ymax*mom), function(k) mat[k,1])
    #                                                                  , rev(sapply(1:floor(ymax*mom), function(k)  mat[k,3] ))), density=20, col = "lightblue" , angle = 90)
    points(quantiles3, quantiles2, type="p" ,cex=1, pch=1)
    lines(quantiles3, mat[,1],lty=3);lines(quantiles3, mat[,3], lty=3)
    
    abline(0,1,lty=2, col = "grey")
    mtext(ville[(i-1)*3 + ind], cex=0.8)
  }}
  else{ 
    for(ind in 1:3){
    
    #sample     <- na.omit( pre[index[,j],((i-1)*3+ind+1)] )
    #sample     <- na.omit(sample)
    sample <- sample0[,ind]
    quantiles4 <-  rev((sort(sample)))[nmax:1] 
    
    plot(NA,NA,pch=1,cex=1, xlab = "return levels",ylab= " " , #TeX('$1/\\widehat{\\alpha}^n$ - stable quantiles') , 
         main = region[i], ylim =ylim0,xlim = log(xlim0) ,xaxt="n", yaxt="n", frame=F)
    qb   <-  sapply( 1:nmax , function(k) qstable( ( ( 1-  k/(moments[ind]*n) )^bn ) , alpha=bb[1], beta=bb[2], gamma=bb[3], delta=bb[4] )^(1/alpha)) ## quantiles

    qb2   <- sapply( c(1,2,5,10,20,50,100,200), function(k) qstable( ( ( 1-  4/(moments[ind]*365*k) )^bn ) , alpha=bb[1], beta=bb[2], gamma=bb[3], delta=bb[4] )^(1/alpha)) ## quantiles
    
    segments( log(qb2[6]), 0, log(qb2[6]) ,qb2[ 6] , col = "grey" , lty=2)
    segments( log(qb2[6]), qb2[6], log(xlim0[2]), qb2[ 6], col="grey" , lty=2)
    
    #abline(0,1,lty=1, col = "grey")
    lines( log(seq(xlim0[1], xlim0[2], by =1) ) , seq(xlim0[1], xlim0[2], by =1)   )
    
    points(log( rev(qb)) , quantiles4 )
    lines( log(mat0[ ((ind-1)*8+ 1:8),2]), mat0[((ind-1)*8+ 1:8),1], lty=2)
    lines( log(mat0[((ind-1)*8+ 1:8),2]),  mat0[((ind-1)*8+ 1:8),3], lty=2)
    
    
    #lines( mat[1:nmax,2], mat[1:nmax,3] , lty = 2)
    #lines( mat[1:nmax,2], mat[1:nmax,1] , lty = 2)
    #abline(0,1)
    #lines( quantiles4, mat[1:nmax,1], lty=2 )
    #lines( quantiles4, mat[1:nmax,3], lty=2 )
    #qb   <- sapply( c(5,10,20,50,100,200), function(Ti) qstable( rev( ( 1-  4/(moments[ind]*365*Ti) )^bn ) , alpha=bb[1], beta=bb[2], gamma=bb[3], delta=bb[4] )^(1/alpha)) ## quantiles
    axis(1, at=log(qb2),labels=c("1 yr","2 yr","5 yr","10 yr","20 yr", "50 yr", "100 yr ","200 yr"),
         col.axis="black", las=2, cex.axis=0.9 ,tck=-.01,srt = 45)
    axis(4, 
         col.axis="darkgrey", las=1, cex.axis=0.8, tck=-.02,lwd = 0.5)
   
    mtext(ville[(i-1)*3 + ind], cex=0.8) 
  } 
  }
}
ylim0 <- xlim0 <- c(90,270)
ylim0 <- xlim0 <- c(35,115)

ylim0 <- xlim0 <- c(4,15)



#################################################################
#################################################################
## mARMAX
msample0  <-  mARMAX1(al=4,par0=c(0.7,0.7,0.7),n0=8000,dep0=0.1,d0=3); n <- 8000
RP <- c(5,20,50)       ## period of years.
k0 <- c(350,floor(n^0.9))     ## k parameters to compute
sample        <- msample0
sample         <- na.omit(sample); n          <- length(sample[,1] )
sample        <- sapply(1:n, function(k) max(sample[k,]) )
infostat      <- stable_return_level(RP,k0,sample)
infoclassical   <- classical_return_level(RP,k0,sample)
ylim1           <- c(0,50)
s               <- 3   ## Fix RP
par(mfrow=c(1,3));plot_ci(infostat,infoclassical,real=real)

bu0<- c(40)
kp0 <- floor(n^0.9)
ylim0 <- xlim0 <- c(0,15)
par(mfrow=c(1,3))
for(i in 1){
  
  bu<- bu0[i]
  kp<- kp0[i]
  j <- 3
  #sample0  <-  mARMAX1(al=4,par0=c(0.7,0.8,0.8),n0=n,dep0=0.9,d0=3)
  
  sample0     <- msample0#na.omit(cbind( pre[index[,j],((i-1)*3+2)] , pre[index[,j],((i-1)*3+3)] , pre[index[,j],((i-1)*3+4)] ) )
  sample0     <- na.omit(sample0); n      <- length(sample0[,1] )
  sample     <- sapply(1:n, function(k) max(sample0[k, ]) )
  records    <- sort(sample, decreasing=T)
  
  alpha <-   1/alphaestimator(sample,k1=kp)$xi
  sumaalpha  <- sapply(1:floor(n/bu), function(k) sum(sample[((k-1)*bu + 1):(k*bu) ]^alpha) )
  ymax       <- length(sumaalpha)
  #mat        <- stableconfintervals(sumaalpha,alpha,ymax*1/min(moments))
  
  
  ind        <- which(sample > records[floor(n*0.06) ] )
  moments    <- sapply(1:3, function(d) mean( (sample0[ind,d]/sample[ind])^alpha ) )
  print(moments)
  
  for(ind in 1:3){
    ## Creates sample
    sample     <- msample0#na.omit(cbind( pre[index[,j],((i-1)*3+2)] , pre[index[,j],((i-1)*3+3)] , pre[index[,j],((i-1)*3+4)] ) )
    sample     <- na.omit(sample); n      <- length(sample[,1] )
    sample     <- sapply(1:n, function(k) max(sample[k, ind]) )
    
    ## Computes index of regular variation 
    alpha0      <-   1/alphaestimator(sample,k1=kp)$xi
    if(alpha0 < 0) alpha0 <- 10
    
    ## Creates suma - max samples
    sumaalpha0  <- sapply(1:floor(n/bu), function(k) sum(sample[((k-1)*bu + 1):(k*bu) ]^alpha0) )
    #mat0        <- stableconfintervals(sumaalpha0,alpha0)
    ## Sorted
    sorted     <- sort(sample,decreasing=T)

    sample0     <- na.omit(cbind( pre[index[,j],((i-1)*3+2)] , pre[index[,j],((i-1)*3+3)] , pre[index[,j],((i-1)*3+4)] ) )
    sample0     <- na.omit(sample0); n      <- length(sample0[,1] )
    sample     <- sapply(1:n, function(k) max(sample0[k, ]) )
    
    mom        <- moments[ind]
    mat        <- stableconfintervals(sumaalpha,alpha,mom)
    
    
    quantiles2 <- sapply(1:floor(ymax*mom), function(k) mat[k,2])
    ymax2       <- length(quantiles2)
    quantiles3 <-  rev(sorted[1:ymax2]) 
    
    plot(NA,NA,pch=1,cex=1, xlab = "observations",ylab=TeX('$1/\\widehat{\\alpha}^n$ - stable quantiles') , main = region[i], ylim =ylim0,xlim = xlim0 )
    polygon( x=c( quantiles3[1:ymax2], rev(quantiles3[1:ymax2]) ), y=c( sapply(1:floor(ymax*mom), function(k) mat[k,1])
                                                                        , rev(sapply(1:floor(ymax*mom), function(k)  mat[k,3] ))), density=20, col = "lightblue" , angle = 90)
    points(quantiles3, quantiles2, type="p" ,cex=1, pch=1)
    # lines(quantiles3,sapply(1:floor(ymax*mom), function(k) mat[(floor(k/mom)),1]),lty=2,col=1)
    #lines(quantiles3,sapply(1:floor(ymax*mom), function(k) mat[(floor(k/mom)+1),3]),lty=2,col=1)
    abline(0,1,lty=2, col = "grey")
    mtext(ville[(i-1)*3 + ind], cex=0.8)
  }
  
  
}



nmax <- floor(0.02*n)

stableconfintervals <- function(sumaalpha,alpha,mom=1){
  
  n2               <- length(sumaalpha)
  bb               <- stable.fit.mle.restricted(sumaalpha, c(1,1,0,0), c(1,1,0,0))
  qb               <- sapply( mom, function(m) qstable( rev( ( 1-  1:nmax/(m*n) )^bn ) , alpha=bb[1], beta=bb[2], gamma=bb[3], delta=bb[4] )) ## quantiles
  qb               <- sapply( 1:length(mom), function(m) sign(qb[,m] )*abs(qb[,m] )^(1/alpha))
  
  ran.gen.stable <- function(data, param) rstable( length(data), alpha=param$al,beta=1,gamma=param$gam,delta=param$del)
  ### Statistic
  statisticQ     <- function(data){
    d   <-  data 
    b   <-  stable.fit.mle.restricted(d, c(0,1,0,0), c(0,1,0,0))  ## fits a stable distribution with beta = 1
    res1 <-  sapply( mom, function(m)
               qstable( rev( ( 1-  1:nmax/(m*n) )^bn ),  alpha=  b[1], beta= b[2], 
                               gamma = b[3], delta=b[4])  )
    res <- NULL 
    for( m in 1:length(mom) ) res <- cbind( res, sign(res1[,m])*abs(res1[,m])^(1/alpha) )
    return(res) 
  }
  ### Bootstrap iterates
  b   <- boot(sumaalpha, statistic = statisticQ, R = 50, sim="parametric",ran.gen=ran.gen.stable,
              mle = list(al = bb[1],gam=bb[3],del=bb[4]) )
  ## CI
  mat <- NULL
  for(l in 1:length(mom)){
    for( j in 1:nmax ){
      b1  <- boot.ci(b, alpha = 0.05, type = "perc", index = ((l-1)*nmax+j) )
      mat <- rbind( mat, c(b1$percent[4],qb[j,l],b1$percent[5]) )
    }
  }

  
  return(mat)
}

stableconfintervals2 <- function(sumaalpha,alpha,mom=1){
  
  n2               <- length(sumaalpha)
  bb               <- stable.fit.mle.restricted(sumaalpha, c(1,1,0,0), c(1,1,0,0))
  qb               <- sapply( mom, function(m) qstable( rev( ( 1-  4/(m*365*c(1,2,5,10,20,50,100,200) ) )^bn ) , alpha=bb[1], beta=bb[2], gamma=bb[3], delta=bb[4] )) ## quantiles
  qb               <- sapply( 1:length(mom), function(m) sign(qb[,m] )*abs(qb[,m] )^(1/alpha))
  
  ran.gen.stable <- function(data, param) rstable( length(data), alpha=param$al,beta=1,gamma=param$gam,delta=param$del)
  ### Statistic
  statisticQ     <- function(data){
    d   <-  data 
    b   <-  stable.fit.mle.restricted(d, c(0,1,0,0), c(0,1,0,0))  ## fits a stable distribution with beta = 1
    res1 <-  sapply( mom, function(m)
      qstable( rev( ( 1-  4/(m*365*c(1,2,5,10,20,50,100,200)) )^bn ),  alpha=  b[1], beta= b[2], 
               gamma = b[3], delta=b[4])  )
    res <- NULL 
    for( m in 1:length(mom) ) res <- cbind( res, sign(res1[,m])*abs(res1[,m])^(1/alpha) )
    return(res) 
  }
  ### Bootstrap iterates
  b   <- boot(sumaalpha, statistic = statisticQ, R = 1000, sim="parametric",ran.gen=ran.gen.stable,
              mle = list(al = bb[1],gam=bb[3],del=bb[4]) )
  ## CI
  mat <- NULL
  for(l in 1:length(mom)){
    for( j in 1:8 ){
      b1  <- boot.ci(b, alpha = 0.05, type = "perc", index = ((l-1)*8+j) )
      mat <- rbind( mat, c(b1$percent[4],qb[j,l],b1$percent[5]) )
    }
  }
  
  
  return(mat)
}
##### Computes index of spatial clustering
i <- 1; j <- 3
sample     <- na.omit(cbind( pre[index[,j],((i-1)*3+2)] , pre[index[,j],((i-1)*3+3)] , pre[index[,j],((i-1)*3+4)] ) )
sample     <- na.omit(sample); n      <- length(sample[,1] )
sample0    <- sample


par(mfrow=c(1,3))
for(N in 1:1){
n <- 4000
kp <- n^0.9
bu <- 120
for(ind in 1:3){
  quantiles  <- NULL
  quantiles2 <- NULL
  quantiles3 <- NULL
  sample0  <-  mARMAX1(al=4,par0=c(0.7,0.8,0.9),n0=n,dep0=0.6,d0=3)
  for(N in 1:1){
        sample      <- sapply(1:n, function(k) max(sample0[k, ind]) )
        
        ## alpha
        alpha      <-   1/alphaestimator(sample,k1=kp)$xi
        if(alpha < 0) alpha <- 10
        sumaalpha  <- sapply(1:floor(n/bu), function(k) sum(sample[((k-1)*bu + 1):(k*bu) ]^alpha) )
        maxalpha   <- sapply(1:floor(n/bu), function(k) max(sample[((k-1)*bu + 1):(k*bu) ]^alpha) )
        
        sorted     <- sort(sample, decreasing=T)
        sortedsuma <- sort(sumaalpha, decreasing=T)
        sortedmax  <- sort(maxalpha,decreasing=T)
        
        ymax <- length(sortedsuma)
        
        means <- NULL
        
        for(k in 1:ymax){
          means <- c(means, mean(sorted[k:n]) )
        }
        quantiles <-  rbind( quantiles, (sortedsuma[1:ymax]-means[1:ymax])^(1/alpha))
        quantiles3 <- rbind(quantiles3, sorted[1:ymax] )
      
      
    
    
  }
  for(N in 1:1) plot(quantiles3[N,], quantiles[N,],pch=1,cex=.8, ylim = c(0,15), xlim = c(0,15),
                     xlab = "sample",ylab=TeX('$\\alpha$ - norm') ,main = ind)
  abline(0,1, col="grey")
    
  
  quantiles  <- NULL
  quantiles2 <- NULL
  quantiles3 <- NULL
  
  sample     <- sapply(1:n, function(k) max(sample0[k, 1:3]) )
  
  ## alpha
  alpha <-   1/alphaestimator(sample,k1=kp)$xi
  
  if( ind==1){
    bu   <- 32
    flag <- TRUE
    while( bu < 150 && flag ){
      suma        <- sapply(1:floor(n/bu), function(k) sum(sample[((k-1)*bu + 1 ):(k*bu)]^alpha  ) )
      fit         <- stable.fit.mle.restricted(suma, c(0,1,0,0), c(0,1,0,0))
      bb1         <- stable.fit.mle.restricted(suma, c(1,1,0,0), c(1,1,0,0))
      stat           <- -2*(stable.loglik(suma,bb1)-stable.loglik(suma,fit))
      if(pchisq(stat,1) < 0.95) flag <- FALSE
      if(flag) bu <- bu +1
    } 
    ## 2. Finds the following 20 times the test is accepted
    v    <- rbind(NULL,c(pchisq(stat,1),bu))
    flag <- TRUE
    b    <- bu+1
    while(flag){
      suma        <- sapply(1:floor(n/b), function(k) sum(sample[((k-1)*b + 1 ):(k*b)]^alpha  ) )
      fit         <- stable.fit.mle.restricted(suma, c(0,1,0,0), c(0,1,0,0))
      bb1         <- stable.fit.mle.restricted(suma, c(1,1,0,0), c(1,1,0,0))
      stat           <- -2*(stable.loglik(suma,bb1)-stable.loglik(suma,fit))
      if(pchisq(stat,1) < 0.95) v <- rbind(v,c(pchisq(stat,1),b) )
      b <- b+1
      if(b > 200 || length(v[,1]) == 20) flag <- FALSE
    }
    #print(v)
    ## 3. Chooses the minimum
    id <- which.min(v[,1])
    bu <- v[id,2]
  }
  print(bu)
  
  sumaalpha  <- sapply(1:floor(n/bu), function(k) sum(sample[((k-1)*bu + 1):(k*bu) ]^alpha) )
  maxalpha   <- sapply(1:floor(n/bu), function(k) max(sample[((k-1)*bu + 1):(k*bu) ]^alpha) )
  
  
  
  sorted     <- sort(sample, decreasing=T)
  sortedsuma <- sort(sumaalpha, decreasing=T)
  sortedmax  <- sort(maxalpha,decreasing=T)
  
  ymax <- length(sortedsuma)
  
  means <- NULL
  
  for(k in 1:ymax){
    means <- c(means, mean(sorted[k:n]) )
  }
  quantiles <-  rbind( quantiles, (sortedsuma[1:ymax]-means[1:ymax])^(1/alpha))
  quantiles2 <- rbind(quantiles2, sortedmax[1:ymax]^(1/alpha))
  quantiles3 <- rbind(quantiles3, sorted[1:ymax] )
  
  
  points( quantiles3[1,], quantiles2[1,],pch=1,cex=.8, col = "green")
  points( quantiles3[1,], quantiles[1,],pch=1,cex=.8, col = "skyblue2")
  abline(0,1, col="grey")
}
}






### Plot of exceedances
sample <- abs( arima.sim(n = 4000, list(ar=0.8, ma=0), rand.gen=function(n) rt(n,df=4)) )
#alpha  <- 1/alphaestimator(sample, k1 = 1000, R0 = 11, plot=TRUE, ylim0=c(-0.3,0.7))$xi

q      <- quantile(sample, 0.9)
sample <- sample[1:200]
n      <- length(sample)
plot(x=NA, y =NA, xlim = c(0,n), ylim=c(0,10), ylab="", xlab="")
segments(1:n,rep(0,n),1:n,sample, col="lightblue", ylab = "")
for(k in 1:length(sample)) if(sample[k] > q) segments( k, q,k, sample[k])
abline( h=q, col ="lightblue" )  




