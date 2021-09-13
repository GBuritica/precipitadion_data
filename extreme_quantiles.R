
library(ExtDist)
library(extRemes)
i <- 3; j <- 3
par(mfrow = c(1,1))
{
quantiles  <- NULL
quantiles2 <- NULL
quantiles3 <- NULL
for(N in 1:1){
sample  <-  (ARMAX1(0.9,n,al=4))
#tail(qevd(1:n/(n+1), loc=1,scale=0.25,shape=0.25))#sample#


for(bu in (c(94))){
sample     <- na.omit(cbind( pre[index[,j],((i-1)*3+2)] , pre[index[,j],((i-1)*3+3)] , pre[index[,j],((i-1)*3+4)] ) )
sample     <- na.omit(sample); n      <- length(sample[,1] )
sample     <- sapply(1:n, function(k) max(sample[k,  ]) )

#n <- 4000


alpha <- 1/alphaestimator(sample)$xi
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
quantiles <- rbind( quantiles, (sortedsuma[1:ymax]-means[1:ymax])^(1/alpha))
quantiles2 <- rbind(quantiles2, sortedmax[1:ymax]^(1/alpha))
quantiles3 <- rbind(quantiles3, sorted[1:ymax] )
}

}

par(mfrow=c(1,1))
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

qqplot(quantiles, quantiles3);abline(0,1)
points(quantiles2, quantiles3, col = "green" )
#for(l in 1:100) points(quantiles3[l,], sample2[1:floor(n/bu)]);abline(0,1)
}

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


