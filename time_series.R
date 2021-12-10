

i <- 2; j <- 3; ind <- 3
names <- c("BORMES", "LE LUC", "HYERES")

sample     <- na.omit(cbind( pre[index[,j],((i-1)*3+2)] , pre[index[,j],((i-1)*3+3)] , pre[index[,j],((i-1)*3+4)] ) )
sample     <- na.omit(sample); n      <- length(sample[,1] )
sample     <- sapply(1:n, function(k) max(sample[k, ind]) )



## plot block maxima
par(mfrow = c(1,1))
b <- 20
sorted <- sort(sample,decreasing=T)
quant  <- sorted[floor(n/20)]
sample <- sample; n <- length(sample)

plot(x=NA,y=NA, xlim=c(0,length(sample)), ylim = c(0, max(sample) ), xlab = "time", ylab = "observations")
maxsample     <- sapply(1:floor(n/b), function(k) which.max(sample[((k-1)*b + 1):(k*b)] ) )
for( k in 1:floor(n/b)){
  for(l in 1:b){
    i <- (k-1)*b + l
    if( maxsample[k] != l) segments( i,0,i,sample[i], col = "lightblue" ) 
    else segments( i,0,i,sample[i]) 
  }
}
abline(h = quant, lty=2,col ="lightgrey")


## plot peaks over threshold
par(mfrow = c(1,1))
plot(x=NA,y=NA, xlim=c(0,length(sample)), ylim = c(0, max(sample) ), xlab = "time", ylab = "observations")
ind <- which(sample > quant)
for(i in 1:n) segments( i,0,i,min(sample[i], quant), col = "lightblue") 
for(i in ind) segments( i,quant,i,sample[i], col = "black") 
abline(h = quant, lty=2,col ="lightgrey")



plot.ts(sample, xy.lines = F , main ="South")



############################### Exceedences.
#par(mfrow = c(3,1))
b <- 20

sorted <- sort(sample,decreasing=T)
quant  <- sorted[floor(n/20)]
sample <- sample; n <- length(sample)

plot(x=NA,y=NA, xlim=c(0,length(sample)), ylim = c(0, 180), xlab = " ", ylab = "observations", main = names[ind])
ind <- which(sample > quant)
for(i in 1:n) segments( i,0,i,min(sample[i], quant), col = "lightblue") 
for(i in ind) segments( i,0,i,sample[i], col = "black") 
abline(h = quant, lty=2,col ="lightgrey")



## Extremogram 
par(mfrow=c(1,3))
title  <- c("northwest","south", "northeast")
title2 <- c("Brest", "Hyeres", "Metz")
for(i in 1:3){
j <- 3; 
for(ind in 1){
  sample     <- na.omit(cbind( pre[index[,j],((i-1)*3+2)] , pre[index[,j],((i-1)*3+3)] , pre[index[,j],((i-1)*3+4)] ) )
  sample     <- na.omit(sample); n      <- length(sample[,1] )
  sample     <- sapply(1:n, function(k) max(sample[k, 1]) )
  
  #sample     <- rfrechet(n)
  
  sorted <- sort(sample,decreasing=T)
  quant  <- sorted[floor(n*0.05)]
  
  maxlag <- 45
  extremogram <- ext(sample, maxlag, q=quant)
  {
    plot(x=NULL,y=NULL, xlim = c(1,maxlag), 
                  ylim = c(0,0.23), xlab = "time lag",
                  ylab = "extremogram" , main = title[i])
  }
  for(k in 1:(maxlag+1)) segments( k,0,k,extremogram[k])
  mtext(title2[i], cex = .8)
  abline(h=0.05,lty=2)
  }
 
}


plot(x=NA,y=NA, xlim=c(0,length(sample)), ylim = c(0, 180), xlab = " ", ylab = "observations", main = names[ind])
ind <- which(sample > quant)
for(i in 1:n) segments( i,0,i,min(sample[i], quant), col = "lightblue") 
for(i in ind) segments( i,0,i,sample[i], col = "black") 
abline(h = quant, lty=2,col ="lightgrey")


ext  <- function(sample, maxlag=10, q) sapply( 1:maxlag, function(k) mean( sample[ (which(sample > q) + k) ] > q , na.rm = T) )

