
library(ExtDist)
library(extRemes)
library(evd)
library(latex2exp)

par(mfrow=c(3,3))

bu0<- c(165,105,70)
kp0 <- c(350,250,350)
ville <- c("BREST", "LANVEOC", "QUIMPER", "BORMES", "LE.LUC", "HYERES", "NANCY", "METZ", "ROVILLE")
region <- c("North West", "South", "North East")
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
    ymax <- length(sortedsuma)
    quantiles <-  rbind( quantiles, (sortedsuma[1:ymax])^(1/alpha))
    quantiles3 <- rbind(quantiles3, sorted[1:ymax] )
    
    plot(quantiles3[1,], quantiles[1,],pch=16,cex=.8, xlab = "sample",ylab=TeX('$\\alpha$ - norm') , main = region[i] )
    mtext(ville[(i-1)*3 + ind] , cex =.8)
    abline(0,1, col="grey")
    
    
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

sample     <- sapply(1:n, function(k) max(sample0[k, ]) )
ind        <- which(sample > records[floor(n*0.06) ] )
moments    <- sapply(1:3, function(d) mean( (sample0[ind,d]/sample[ind])^alpha ) )
print(moments)




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




