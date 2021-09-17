
library(ExtDist)
library(extRemes)
library(evd)

par(mfrow=c(1,1))
i <- 3; j <- 3
n <- 4000
bu <- 90
sample0  <-  mARMAX1(al=4,par0=c(0.7,0.8,0.8),n0=n,dep0=0.9,d0=3)
for(ind in 1){
{
quantiles  <- NULL
quantiles2 <- NULL
quantiles3 <- NULL
for(N in 1:1){
#tail(qevd(1:n/(n+1), loc=1,scale=0.25,shape=0.25))#sample#
{
sample     <- na.omit(cbind( pre[index[,j],((i-1)*3+2)] , pre[index[,j],((i-1)*3+3)] , pre[index[,j],((i-1)*3+4)] ) )
sample     <- na.omit(sample); n      <- length(sample[,1] )
sample     <- sapply(1:n, function(k) max(sample[k, ind]) )

#n <- 4000


alpha      <-   1/alphaestimator(sample)$xi
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
quantiles2 <- rbind(quantiles2, sortedmax[1:ymax]^(1/alpha))
quantiles3 <- rbind(quantiles3, sorted[1:ymax] )
}

}

#par(mfrow=c(1,1))
sample2 <- sort(qfrechet(1:n/(n+1), loc=0,scale=1,shape=4 ), decreasing=T)
#boxplot(quantiles[,1:10], ylim = c(2,15));abline(h=sample2[1:5],lty=2,col="red")
#boxplot(quantiles2[,1:10],ylim = c(2,15));abline(h=sample2[1:5],lty=2,col="red")
#boxplot(quantiles3[,1:10],ylim = c(2,15));abline(h=sample2[1:5],lty=2,col="red")

#plot(quantiles[1,], sample2[1:floor(n/bu)], ylim = c(0,150),xlim = c(0,150));abline(0,1)
#for(l in 1:1) points(quantiles[l,], sample2[1:floor(n/bu)]);abline(0,1)


#plot(quantiles2[1,], sample2[1:floor(n/bu)],ylim = c(0,150),xlim = c(0,150));abline(0,1)
#for(l in 1:1) points(quantiles2[l,], sample2[1:floor(n/bu)]);abline(0,1)


#plot(quantiles3[1,], sample2[1:floor(n/bu)],ylim = c(0,150),xlim = c(0,150));abline(0,1)
#for(l in 1:1) points(quantiles3[l,], sample2[1:floor(n/bu)]);abline(0,1)

#qqplot(quantiles, quantiles3);abline(0,1)
#points(quantiles2, quantiles3, col = "green" )
#for(l in 1:100) points(quantiles3[l,], sample2[1:floor(n/bu)]);abline(0,1)
}
#quantiles4 <- rev(qfrechet( (1:n/(n+1)) , shape=4))
#quantiles4 <- rbind(NULL,quantiles4[1:ymax])
plot(quantiles3[1,], quantiles[1,],pch=1,cex=1,ylim = c(10,150),xlim = c(10,150))
#points(quantiles3[1,],quantiles3[1,], col = "green")
abline(0,1, col="grey")
ind <- 1:3
{
  quantiles  <- NULL
  quantiles2 <- NULL
  quantiles3 <- NULL
  for(N in 1:1){
    #sample  <-  (ARMAX1(0.9,n,al=4))
    #tail(qevd(1:n/(n+1), loc=1,scale=0.25,shape=0.25))#sample#
    {
      sample     <- na.omit(cbind( pre[index[,j],((i-1)*3+2)] , pre[index[,j],((i-1)*3+3)] , pre[index[,j],((i-1)*3+4)] ) )
      sample     <- na.omit(sample); n      <- length(sample[,1] )
      sample     <- sapply(1:n, function(k) max(sample[k, ind]) )
      
      #n <- 4000
      
      
      alpha <-   1/alphaestimator(sample)$xi
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
    }
    
  }
  
  #par(mfrow=c(1,1))
  
  #boxplot(quantiles[,1:10], ylim = c(2,15));abline(h=sample2[1:5],lty=2,col="red")
  #boxplot(quantiles2[,1:10],ylim = c(2,15));abline(h=sample2[1:5],lty=2,col="red")
  #boxplot(quantiles3[,1:10],ylim = c(2,15));abline(h=sample2[1:5],lty=2,col="red")
  
  #plot(quantiles[1,], sample2[1:floor(n/bu)], ylim = c(0,150),xlim = c(0,150));abline(0,1)
  #for(l in 1:1) points(quantiles[l,], sample2[1:floor(n/bu)]);abline(0,1)
  
  
  #plot(quantiles2[1,], sample2[1:floor(n/bu)],ylim = c(0,150),xlim = c(0,150));abline(0,1)
  #for(l in 1:1) points(quantiles2[l,], sample2[1:floor(n/bu)]);abline(0,1)
  
  
  #plot(quantiles3[1,], sample2[1:floor(n/bu)],ylim = c(0,150),xlim = c(0,150));abline(0,1)
  #for(l in 1:1) points(quantiles3[l,], sample2[1:floor(n/bu)]);abline(0,1)
  
  #qqplot(quantiles, quantiles3);abline(0,1)
  #points(quantiles2, quantiles3, col = "green" )
  #for(l in 1:100) points(quantiles3[l,], sample2[1:floor(n/bu)]);abline(0,1)
}
points( quantiles3[1,], quantiles[1,],pch=16,cex=1, col = "skyblue2",ylim = c(10,150),xlim = c(10,150))
abline(0,1, col="grey")
}
for(ind in 2:3){
  {
    quantiles  <- NULL
    quantiles2 <- NULL
    quantiles3 <- NULL
    for(N in 1:1){
      #tail(qevd(1:n/(n+1), loc=1,scale=0.25,shape=0.25))#sample#
      {
        sample     <- na.omit(cbind( pre[index[,j],((i-1)*3+2)] , pre[index[,j],((i-1)*3+3)] , pre[index[,j],((i-1)*3+4)] ) )
        sample     <- na.omit(sample); n      <- length(sample[,1] )
        sample     <- sapply(1:n, function(k) max(sample[k, ind]) )
        
        #n <- 4000
        
        
        alpha      <-   1/alphaestimator(sample)$xi
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
        quantiles2 <- rbind(quantiles2, sortedmax[1:ymax]^(1/alpha))
        quantiles3 <- rbind(quantiles3, sorted[1:ymax] )
      }
      
    }
    
    #par(mfrow=c(1,1))
    sample2 <- sort(qfrechet(1:n/(n+1), loc=0,scale=1,shape=4 ), decreasing=T)
    #boxplot(quantiles[,1:10], ylim = c(2,15));abline(h=sample2[1:5],lty=2,col="red")
    #boxplot(quantiles2[,1:10],ylim = c(2,15));abline(h=sample2[1:5],lty=2,col="red")
    #boxplot(quantiles3[,1:10],ylim = c(2,15));abline(h=sample2[1:5],lty=2,col="red")
    
    #plot(quantiles[1,], sample2[1:floor(n/bu)], ylim = c(0,150),xlim = c(0,150));abline(0,1)
    #for(l in 1:1) points(quantiles[l,], sample2[1:floor(n/bu)]);abline(0,1)
    
    
    #plot(quantiles2[1,], sample2[1:floor(n/bu)],ylim = c(0,150),xlim = c(0,150));abline(0,1)
    #for(l in 1:1) points(quantiles2[l,], sample2[1:floor(n/bu)]);abline(0,1)
    
    
    #plot(quantiles3[1,], sample2[1:floor(n/bu)],ylim = c(0,150),xlim = c(0,150));abline(0,1)
    #for(l in 1:1) points(quantiles3[l,], sample2[1:floor(n/bu)]);abline(0,1)
    
    #qqplot(quantiles, quantiles3);abline(0,1)
    #points(quantiles2, quantiles3, col = "green" )
    #for(l in 1:100) points(quantiles3[l,], sample2[1:floor(n/bu)]);abline(0,1)
  }
  #quantiles4 <- rev(qfrechet( (1:n/(n+1)) , shape=4))
  #quantiles4 <- rbind(NULL,quantiles4[1:ymax])
  points(quantiles3[1,], quantiles[1,],pch=1,cex=1,ylim = c(10,100),xlim = c(10,100))
  #points(quantiles3[1,],quantiles3[1,], col = "green")
  abline(0,1, col="grey")
  ind <- 1:3
  {
    quantiles  <- NULL
    quantiles2 <- NULL
    quantiles3 <- NULL
    for(N in 1:1){
      #sample  <-  (ARMAX1(0.9,n,al=4))
      #tail(qevd(1:n/(n+1), loc=1,scale=0.25,shape=0.25))#sample#
      {
        sample     <- na.omit(cbind( pre[index[,j],((i-1)*3+2)] , pre[index[,j],((i-1)*3+3)] , pre[index[,j],((i-1)*3+4)] ) )
        sample     <- na.omit(sample); n      <- length(sample[,1] )
        sample     <- sapply(1:n, function(k) max(sample[k, ind]) )
        
        #n <- 4000
        
        
        alpha <-   1/alphaestimator(sample)$xi
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
      }
      
    }
    
    #par(mfrow=c(1,1))
    
    #boxplot(quantiles[,1:10], ylim = c(2,15));abline(h=sample2[1:5],lty=2,col="red")
    #boxplot(quantiles2[,1:10],ylim = c(2,15));abline(h=sample2[1:5],lty=2,col="red")
    #boxplot(quantiles3[,1:10],ylim = c(2,15));abline(h=sample2[1:5],lty=2,col="red")
    
    #plot(quantiles[1,], sample2[1:floor(n/bu)], ylim = c(0,150),xlim = c(0,150));abline(0,1)
    #for(l in 1:1) points(quantiles[l,], sample2[1:floor(n/bu)]);abline(0,1)
    
    
    #plot(quantiles2[1,], sample2[1:floor(n/bu)],ylim = c(0,150),xlim = c(0,150));abline(0,1)
    #for(l in 1:1) points(quantiles2[l,], sample2[1:floor(n/bu)]);abline(0,1)
    
    
    #plot(quantiles3[1,], sample2[1:floor(n/bu)],ylim = c(0,150),xlim = c(0,150));abline(0,1)
    #for(l in 1:1) points(quantiles3[l,], sample2[1:floor(n/bu)]);abline(0,1)
    
    #qqplot(quantiles, quantiles3);abline(0,1)
    #points(quantiles2, quantiles3, col = "green" )
    #for(l in 1:100) points(quantiles3[l,], sample2[1:floor(n/bu)]);abline(0,1)
  }
  points( quantiles3[1,], quantiles[1,],pch=1,cex=0.8, col = "skyblue2",ylim = c(10,100),xlim = c(10,100))
  abline(0,1, col="grey")
}





n <- 1000
par(mfrow=c(1,3))
bu <- 32
for(N in 1:10){
  sample0  <-  mARMAX1(al=4,par0=c(0.2,0.6,0.7),n0=n,dep0=0.5,d0=3)
  for(ind in 1:3){
    {
      quantiles  <- NULL
      quantiles2 <- NULL
      quantiles3 <- NULL
      for(N in 1:1){
        #tail(qevd(1:n/(n+1), loc=1,scale=0.25,shape=0.25))#sample#
        {
          #sample     <- na.omit(cbind( pre[index[,j],((i-1)*3+2)] , pre[index[,j],((i-1)*3+3)] , pre[index[,j],((i-1)*3+4)] ) )
          #sample     <- na.omit(sample); n      <- length(sample[,1] )
          sample     <- sapply(1:n, function(k) max(sample0[k, ind]) )
          
          #n <- 4000
          
          
          alpha      <-   1/alphaestimator(sample,k1=floor(n^0.9))$xi; print(alpha)
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
          quantiles2 <- rbind(quantiles2, sortedmax[1:ymax]^(1/alpha))
          quantiles3 <- rbind(quantiles3, sorted[1:ymax] )
        }
        
      }
      
      #par(mfrow=c(1,1))
      sample2 <- sort(qfrechet(1:n/(n+1), loc=0,scale=1,shape=4 ), decreasing=T)
      #boxplot(quantiles[,1:10], ylim = c(2,15));abline(h=sample2[1:5],lty=2,col="red")
      #boxplot(quantiles2[,1:10],ylim = c(2,15));abline(h=sample2[1:5],lty=2,col="red")
      #boxplot(quantiles3[,1:10],ylim = c(2,15));abline(h=sample2[1:5],lty=2,col="red")
      
      #plot(quantiles[1,], sample2[1:floor(n/bu)], ylim = c(0,150),xlim = c(0,150));abline(0,1)
      #for(l in 1:1) points(quantiles[l,], sample2[1:floor(n/bu)]);abline(0,1)
      
      
      #plot(quantiles2[1,], sample2[1:floor(n/bu)],ylim = c(0,150),xlim = c(0,150));abline(0,1)
      #for(l in 1:1) points(quantiles2[l,], sample2[1:floor(n/bu)]);abline(0,1)
      
      
      #plot(quantiles3[1,], sample2[1:floor(n/bu)],ylim = c(0,150),xlim = c(0,150));abline(0,1)
      #for(l in 1:1) points(quantiles3[l,], sample2[1:floor(n/bu)]);abline(0,1)
      
      #qqplot(quantiles, quantiles3);abline(0,1)
      #points(quantiles2, quantiles3, col = "green" )
      #for(l in 1:100) points(quantiles3[l,], sample2[1:floor(n/bu)]);abline(0,1)
    }
    quantiles4 <- rev(qfrechet( (1:n/(n+1)) , shape=4))
    quantiles4 <- rbind(NULL,quantiles4[1:ymax])
    plot(quantiles3[1,], quantiles[1,],pch=1,cex=1)
    points(quantiles3[1,],quantiles4[1,], col = "green")
    #points(quantiles3[1,],quantiles3[1,], col = "green")
    abline(0,1, col="grey")
    ind <- 1:3
    {
      quantiles  <- NULL
      quantiles2 <- NULL
      quantiles3 <- NULL
      for(N in 1:1){
        #sample  <-  (ARMAX1(0.9,n,al=4))
        #tail(qevd(1:n/(n+1), loc=1,scale=0.25,shape=0.25))#sample#
        {
          #sample     <- na.omit(cbind( pre[index[,j],((i-1)*3+2)] , pre[index[,j],((i-1)*3+3)] , pre[index[,j],((i-1)*3+4)] ) )
          #sample     <- na.omit(sample); n      <- length(sample[,1] )
          sample     <- sapply(1:n, function(k) max(sample0[k, ind]) )
          
          #n <- 4000
          
          
          alpha <-   1/alphaestimator(sample,k1=floor(n^0.9))$xi; print(alpha)
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
        }
        
      }
      
      #par(mfrow=c(1,1))
      
      #boxplot(quantiles[,1:10], ylim = c(2,15));abline(h=sample2[1:5],lty=2,col="red")
      #boxplot(quantiles2[,1:10],ylim = c(2,15));abline(h=sample2[1:5],lty=2,col="red")
      #boxplot(quantiles3[,1:10],ylim = c(2,15));abline(h=sample2[1:5],lty=2,col="red")
      
      #plot(quantiles[1,], sample2[1:floor(n/bu)], ylim = c(0,150),xlim = c(0,150));abline(0,1)
      #for(l in 1:1) points(quantiles[l,], sample2[1:floor(n/bu)]);abline(0,1)
      
      
      #plot(quantiles2[1,], sample2[1:floor(n/bu)],ylim = c(0,150),xlim = c(0,150));abline(0,1)
      #for(l in 1:1) points(quantiles2[l,], sample2[1:floor(n/bu)]);abline(0,1)
      
      
      #plot(quantiles3[1,], sample2[1:floor(n/bu)],ylim = c(0,150),xlim = c(0,150));abline(0,1)
      #for(l in 1:1) points(quantiles3[l,], sample2[1:floor(n/bu)]);abline(0,1)
      
      #qqplot(quantiles, quantiles3);abline(0,1)
      #points(quantiles2, quantiles3, col = "green" )
      #for(l in 1:100) points(quantiles3[l,], sample2[1:floor(n/bu)]);abline(0,1)
    }
    points( quantiles3[1,], quantiles[1,],pch=1,cex=0.8, col = "skyblue2",ylim = c(10,100),xlim = c(10,100))
    abline(0,1, col="grey")
    
  }
}





par(mfrow=c(2,2))
for(i in 2^(2:7)){
  boxplot(quantiles[,i],quantiles2[,i],quantiles3[,i], ylim=c(0,15))
  abline(h=quantiles4[i],col="grey",lty=2)
}



#par(mfrow=c(1,3))
plot(quantiles2[1,], quantiles3[1,], ylim = c(2,100), xlim = c(2,100),pch=1,cex=0.5 )
sapply( 1:500, function(k) points( quantiles2[k,] , quantiles3[k,],pch=16,cex=0.5 ) )
abline(0,1, col="grey")

plot(quantiles2[1,], quantiles4[1:ymax], ylim = c(2,10), xlim = c(2,10),pch=16,cex=0.5 )
sapply( 1:500, function(k) points( quantiles2[k,] , quantiles4[1:ymax],pch=16,cex=0.5  ) )
abline(0,1, col="grey")

plot(quantiles3[1,], quantiles4[1:ymax], ylim = c(2,10), xlim = c(2,10),pch=16,cex=0.5 )
abline(0,1, col="grey")
sapply( 1:500, function(k) points( quantiles3[k,] , quantiles4[1:ymax] ,pch=16,cex=0.5 ) )


sample    <- rfrechet(n,shape=4)
quantiles <- qfrechet(1:n/(n+1), shape=4)
qqplot(sample, quantiles)




points(quantiles[2,], quantiles4 )
abline(0,1)
#plot( (sorted[1:ymax] -means)^(1/alpha),sortedsuma[1:ymax]^(1/alpha),col=1,pch=1);abline(0,1)
#points((sorted[1:ymax] )^(1/alpha), (sortedmax[1:ymax])^(1/alpha),cex=1,pch=1,col = "blue");abline(0,1);abline(0,1)


plot( (sortedsuma[1:ymax]/(sorted[1:ymax]))^(1/alpha) )
lines((sortedsuma[1:ymax]/(sorted[1:ymax]- means))^(1/alpha) )


phi <- 0.5; alpha <- 1/0.141654;
sample <- abs(arima.sim(n = n, list(ar=phi, ma=0), rand.gen=function(n) rt(n,df=alpha)))
sample <- abs(rt(n,df=alpha))
q <- quantile(sample, 0.94)

#sample <- sample[1:500]
n <- length(sample)
plot(x=NA, y =NA, xlim = c(0,n), ylim=c(0,170), ylab="", xlab="")
segments(1:n,rep(0,n),1:n,sample, col="lightblue", ylab = "")
for(k in 1:length(sample)) if(sample[k] > q) segments( k, q,k, sample[k])
abline( h=q, col ="lightblue" )


extremalindex(sample, threshold=q)

mean((sample[,1] > q)*(sample[,2] > q) )/(mean(sample[,1] > q))

alphaestimator(sample, k1 = 500, k0=500, R0 = 11, plot=TRUE, ylim0=c(-0.6,0.6))

par(mfrow=c(1,2))
n <- length(sample)
alpha <- 1/0.18
sample2 <- ( sapply(1:floor(n/37), function(k) sum( sample[((k-1)*37+1): (k*37) ]^alpha )^(1/alpha) ))
q <- quantile(sample2, 0.84)
#sample <- sample[1:500]
n <- length(sample2)
plot(x=NA, y =NA, xlim = c(0,n), ylim=c(0,200), ylab="", xlab="")
segments(1:n,rep(0,n),1:n,sample2, col="lightblue", ylab = "")
for(k in 1:length(sample2)) if(sample2[k] > q) segments( k, q,k, sample2[k])
abline( h=q, col ="lightblue" )  

n <- length(sample)
alpha <- 1/0.18
sample2 <- ( sapply(1:floor(n/37), function(k) max( sample[((k-1)*37+1): (k*37) ] ) ))
q <- quantile(sample2, 0.84)
#sample <- sample[1:500]
n <- length(sample2)
plot(x=NA, y =NA, xlim = c(0,n), ylim=c(0,200), ylab="", xlab="")
segments(1:n,rep(0,n),1:n,sample2, col="lightblue", ylab = "")
for(k in 1:length(sample2)) if(sample2[k] > q) segments( k, q,k, sample2[k])
abline( h=q, col ="lightblue" )  


c <- NULL
alpha <- 1/0.14; 
par(mfrow=c(1,3))
for(bn in 1:64){
n <- length(sample)
sample3 <- ( sapply(1:floor(n/bn), function(k) max( sample[((k-1)*bn+1): (k*bn) ]^alpha)^(1/alpha) ))
sample4 <- ( sapply(1:floor(n/bn), function(k) sum( sample[((k-1)*bn+1): (k*bn) ]^alpha )^(1/alpha) ))
sample2 <- ( sapply(1:floor(n/bn), function(k) max( sample[((k-1)*bn+1): (k*bn) ]^alpha )/sum( sample[((k-1)*bn+1): (k*bn) ]^alpha ) ))
q <- sort(sample3,decreasing = TRUE)[max(3,floor(n/bn^2))]
q2 <- sort(sample2,decreasing = TRUE)[max(3,floor(n/bn^2))]
#sample <- sample[1:500]
n <- length(sample2)
if(FALSE){
plot(x=NA, y =NA, xlim = c(0,n), ylim=c(0,200), ylab="", xlab="")
segments(1:n,rep(0,n),1:n,sample3, col="lightblue", ylab = "")
for(k in 1:length(sample2)) if(sample3[k] > q) segments( k, q,k, sample3[k])
abline( h=q, col ="lightblue" )  
print(mean(sample2[sample3 > q]))


plot(x=NA, y =NA, xlim = c(0,n), ylim=c(0,200), ylab="", xlab="")
segments(1:n,rep(0,n),1:n,sample4, col="lightblue", ylab = "")
for(k in 1:length(sample2)) if(sample3[k] > q) segments( k, q,k, sample4[k])
abline( h=q, col ="lightblue" )  
print(mean(sample2[sample3 > q]))

plot(x=NA, y =NA, xlim = c(0,n), ylim=c(0,1), ylab="", xlab="")
segments(1:n,rep(0,n),1:n,sample2, col="lightblue", ylab = "")
for(k in 1:length(sample2)) if(sample3[k] > q) segments( k, 0,k, sample2[k])
abline( h=q, col ="lightblue" )  
}
c <- c(c,mean(sample2[sample3 > q]))
}
plot.ts(c, ylim =c(0,1))


