## Plots These

library(plot3D)

i <- 3
j <- 3 ## season

par(mfrow = c(1,1))

for(i in 3){
  j <- 3
  sample     <- cbind( pre[index[,j],((i-1)*3+2)] , pre[index[,j],((i-1)*3+3)] , pre[index[,j],((i-1)*3+4)] ) 
  #sample     <- mARMAX1(al=4,par0=c(0.7,0.8,0.8),n0=n,dep0=dep[i],d0=3)
  sample     <- na.omit(sample); n          <- length(sample[,1] )
  sample2     <- sort(sapply(1:n, function(k) max(sample[k,]) ))
  a=1; b=3;
  
  plot(sample[sapply(1:n, function(k) max(sample[k,c(a,b)])) > sample2[floor(n*0.98)] ,c(a,b)] ,
          col = "grey", cex = 0.5, 
           pch=16,ylab=names2[(i-1)*3 + b] , xlab=names2[(i-1)*3 +a],
           main = names[i],
           ylim = c(0,max(sample[,1:3]) ) ,
           xlim = c(0,max(sample[,1:3]) )  )
  points(sample[   sapply(1:n, function(k) max(sample[k,c(a,b)])) <= sample2[floor(n*0.98)] ,c(a,b)] ,col = "lightblue", cex = 0.5, 
             pch=16,ylab=names2[(i-1)*3 + b] , xlab=names2[(i-1)*3 +a],
             main = names[i])
  segments( sample2[floor(n*0.98)], sample2[floor(n*0.98)],0,sample2[floor(n*0.98)], col="lightblue" )
  segments( sample2[floor(n*0.98)],0, sample2[floor(n*0.98)],sample2[floor(n*0.98)] , col="lightblue")

}

for(i in 1){
  j <- 3
  sample     <- cbind( pre[index[,j],((i-1)*3+2)] , pre[index[,j],((i-1)*3+3)] , pre[index[,j],((i-1)*3+4)] ) 
  #sample     <- mARMAX1(al=4,par0=c(0.7,0.8,0.8),n0=n,dep0=0.99,d0=3)
  sample     <- na.omit(sample); n          <- length(sample[,1] )
  a=1; b=3;
  maxsample <- sapply(1:n, function(k) max(sample[k,c(a,b)]) )
  sample2   <- sample[ ,c(a,b)]
  th        <- quantile(maxsample,0.95)
  plot(NA,NA, cex = 0.5, 
       pch=16,ylab=names2[(i-1)*3 + b] , xlab=names2[(i-1)*3 +a],
       main = names[i],
       col = 'grey',
       xlim = c(0,3),
       ylim=c(0,3)
       )
  #points(sample[   sapply(1:n, function(k) max(sample[k,c(a,b)])) <= sample2[floor(n*0.94)] ,c(a,b)] ,col = "lightblue", cex = 0.5, 
  #       pch=16,ylab=names2[(i-1)*3 + b] , xlab=names2[(i-1)*3 +a],
  #       main = names[i])
  #segments( sample2[floor(n*0.94)], sample2[floor(n*0.94)],0,sample2[floor(n*0.94)], col="lightblue" )
  #segments( sample2[floor(n*0.94)],0, sample2[floor(n*0.94)],sample2[floor(n*0.94)] , col="lightblue")
  newsample <- sample[  maxsample > th ,c(a,b)]/maxsample[ maxsample > th]
  ind1      <- which(newsample[,1] ==1 )
  ind2      <- which(newsample[,2] ==1 )
  points(newsample[ind2, ] ,col = "black", cex = 0.5, 
         pch='|',ylab=names2[(i-1)*3 + b] , xlab=names2[(i-1)*3 +a],
         main = names[i])
  points(newsample[ind1, ] ,col = "black", cex = 0.5, 
         pch='____',ylab=names2[(i-1)*3 + b] , xlab=names2[(i-1)*3 +a],
         main = names[i])

}


par(mfrow=c(1,1))
for(i in 3){
  j <- 3
  sample     <- cbind( pre[index[,j],((i-1)*3+2)] , pre[index[,j],((i-1)*3+3)] , pre[index[,j],((i-1)*3+4)] ) 
  #sample     <- mARMAX1(al=4,par0=c(0.7,0.8,0.8),n0=n,dep0=0.99,d0=3)
  sample     <- na.omit(sample); n          <- length(sample[,1] )
  a=3; b=1;
  maxsample <- sapply(1:n, function(k) max(sample[k,c(a,b)]) )
  sample3   <- sample[ ,c(a,b)]
  th        <- 20#quantile(maxsample,0.98)
  
  plot(sample3[maxsample>th,], cex = 1, 
       pch=16,ylab=names2[(i-1)*3 + b] , xlab=names2[(i-1)*3 +a],
       main = "",#names[i],
       col = 'lightgrey', xlim = c(0,60),ylim = c(0,60), 
       axes = FALSE)
  
  # Add X-axis
  axis(1)
  
  # Add Y-axis
  axis(2)
  
  segments(0,th,th,th,lty=1,col='darkgrey');segments(th,0,th,th,lty=1,col='darkgrey')
  
  #th <- 45
  #segments(0,th,th,th,lty=2,col='darkgrey');segments(th,0,th,th,lty=2,col='darkgrey')
  
  sample2   <- sample[  maxsample > th ,c(a,b)]
  newsample <- sample[  maxsample > th ,c(a,b)]/maxsample[ maxsample > th ]
  
  ind1      <- which(newsample[,1] ==1 )
  ind2      <- which(newsample[,2] ==1 )
  
  
  segments( x0=0,#newsample[ind2,1]*th,
            y0 =0,# newsample[ind2,2]*th , 
            x1=sample2[ind2, 1],
            y1=sample2[ind2,2], col = "grey", lty=1, cex=.1)
  
  segments( x0=0,#newsample[ind2,1]*th,
            y0 =0,# newsample[ind2,2]*th , 
            x1=sample2[ind1, 1],
            y1=sample2[ind1,2], col = "grey", lty=1, cex=.1)
  
  points(newsample[ind2, ]*th ,col = "black", cex = 1, 
         pch='l',ylab=names2[(i-1)*3 + b] , xlab=names2[(i-1)*3 +a],
         main = names[i])
  
  x <- sample[  maxsample > th ,c(a,b)]/maxsample[ maxsample >th]
  dx <- density(x[ind2,1], from = 0, to=1)
  lines(dx$x*th, dx$y*th/3+th, col ="black")
  
  
  #segments( x0=newsample[ind1,1]*th,
  #          y0 = newsample[ind1,2]*th , 
  #          x1=sample2[ind1, 1],
  #          y1=sample2[ind1,2], lty= 1, col = "grey")
  #points(newsample[ind1, ]*th ,col = "black", cex = 0.5, 
  #       pch=16,ylab=names2[(i-1)*3 + b] , xlab=names2[(i-1)*3 +a],
  #       main = names[i])
  #dx <- density(x[ind1,2], from = 0, to=1)
  #lines(dx$y*th/3+th,dx$x*th)
  

}
dev.copy2eps( bg = 'transparent')
dev.off()

x <- sample[  maxsample > th ,c(a,b)]/maxsample[ maxsample >th]

hist(x[,2], freq = FALSE, main = "Histogram and density", xlim = c(0,1))

# Calculate density
dx <- density(x, from = 0, to = 1.0)

# Add density
lines(dx, lwd = 2, fcol = "red")

# Plot the density without histogram
plot(dx, lwd = 2, col = "red",
     main = "Density")


library(boot)
library(latex2exp)
########
## Theta Plot
## iid 
par(mfrow=c(1,1))
len <- 12
for(N in 1:1){

  x <- c(rep(0,len),1,rep(0,len))
  plot(x=NA,y=NA, xlim = c(-len,len), ylim = c(0,1), 
       xlab ="", ylab = TeX('$Q_t^{(\\alpha)}$'),
       axes = FALSE,)
  axis(1, at=c(-16,0,16) ,labels = c(-16,0,16));axis(2, yaxp = c(0,1,1))
  par(las=1)
  points(-len:len, x/sum(x), pch=16, cex=.8)
  segments(x0=-len:len, y0=rep(0,length(x)), x1=-len:len,  y1=x)
  
  
  
  ## AR
  phi <- 0.5
  n   <- 5000
  alpha0 <- 1
  
  #sample <- abs(arima.sim(n=n, list(ar=phi, ma=0), rand.gen=function(n) rt(n,df=alpha0) ))
  J <- sample(x=0:len, size=1, prob=sapply(0:len, function(k) phi^k) )
  x <- c(rep(0, len-J), sapply(0:(len+J), function(k)  phi^k ) )
  plot(x=NA,y=NA, xlim = c(-len,len), ylim = c(0,1), 
       xlab ="", ylab = TeX('$Q_t^{(\\alpha)}$'),
       axes = FALSE)
  axis(1, at=c(-16,0,16) ,labels = c(-16,0,16));axis(2, yaxp = c(0,1,1))
  par(las=1)
  points(-len:len, x/sum(x), pch=16, cex=.8)
  segments(x0=-len:len, y0=rep(0,length(x)), x1=-len:len,  y1=x/sum(x))
  
  
  
  ## SRE
  gauss <- rnorm(2*len+1)
  x     <-  1#gauss[1]
  for(i in 1:len)  x <- c(x[2*i-1]*exp( gauss[2*i] - .5 ), x, x[2*i-1]*exp( gauss[2*i+1] - .5 ))
  plot(x=NA,y=NA, xlim = c(-len,len), ylim = c(0,1), 
       xlab ="", ylab = TeX('$Q_t^{(\\alpha)}$'),
       axes = FALSE)
  axis(1, at=c(-16,0,16) ,labels = c(-16,0,16));axis(2, yaxp = c(0,1,1))
  points(-len:len, x/sum(x), pch=16, cex= .8)
  segments(x0=-len:len, y0=rep(0,length(x)), x1=-len:len,  y1=x/sum(x))
  
}



########
## 
##

j <- 3
k10 <- c(350, 150,350)
l <- 3
sample    <- cbind( pre[index[,j],((l-1)*3+2)] , pre[index[,j],((l-1)*3+3)] , pre[index[,j],((l-1)*3+4)] ) 
sample <- na.omit(sample)
n      <- length(sample[,1])
sam <- sapply(1:n , function(k) max(sample[k,])) #+ abs(rnorm(n,0,0.05))


bu <- 2:40

alpha  <- (1/alphaestimator(sample,k1=k10[l])$xi) ;print(alpha)
#alpha <- alpha0

p      <- alpha#1#



stattheta         <- function(data){
    n        <- length(data)
    estim4       <-  NULL
    alpha    <- min( 8 , max(0.1, (1/alphaestimator(data,k1=k10[l])$xi) )) #; print(alpha)
    
    for(i in 1:length(bu)){
      mn  <- floor(n/bu[i])
      sumaalpha <- sapply( 1:mn, function(k) sum(   data[((k-1)*bu[i] + 1):(k*bu[i])]^alpha )^(1/alpha)  )
      kn      <-  max(1, floor( (n)/bu[i]^2) )
      th4     <-  sort(sumaalpha, partial = (mn - kn +1) )[(mn-kn+1)]
      if(!is.infinite(th4)) ind4    <-  which(sumaalpha >= th4)
      else print('infinity')
      
    
       # functions theta
      g1            <- function(vector){
        es <- (max(vector^alpha)/sum(vector^alpha) )
        #if(is.na(es)) return(0)
        return(es)
      }
      
      estim          <- mean( sapply( ind4, function(k)     g1(data[((k-1)*bu[i] +1):(k*bu[i])])  ), is.na=rm )
      if(is.na(estim))  print(c(length(ind4),' th', kn))
      estim4         <-  c( estim4,estim)
      #print( c(bu[i], estim)) 
    }
    return(estim4)
}



par(mfrow=c(1,1))
n <- length(sample[,1])

sam <- abs(arima.sim(n=n, list(ar=phi, ma=0), rand.gen=function(n) rt(n,df=alpha0) ))




l <- 2
sample     <- cbind( pre[index[,j],((l-1)*3+2)] , pre[index[,j],((l-1)*3+3)] , pre[index[,j],((l-1)*3+4)] ) 
sample     <- na.omit(sample)
n          <- length(sample[,1])
sam        <- sapply(1:n , function(k) max(sample[k,])) #+ abs(rnorm(n,0,0.05))
#(alphaestimator(sample,k1=1000, plot=T, R0=20)$xi)


b   <-  tsboot(sam,statistic=stattheta, R=1000, sim = "geom", l = 15 )

CI <- NULL
for(i in 1:length(bu)){
  boot <- boot.ci(b,  type = "perc" , index=i)
  #CI <- rbind(CI, c( boot$normal[2:3], boot$t0 ))
  CI<-  rbind(CI, c( boot$percent[4:5], boot$t0 ))
}

CI2 <- CI
par(mfrow=c(3,1))




CI <- CI2
l <-2
bn0 <- 16
bmax <- 31
plot(bu[1:bmax], CI[1:bmax,3], ylim=c(0.5,1), main= names[l],
     xlab ='block length', ylab = TeX('$\\theta_{|X|}$'), pch=16,
     axes = FALSE, col = "#4271AE")
axis(1, at=c(2,8,16,32) ,labels = c(2,8,16,32) );axis(2, at=c(0.5,round(CI[(bn0-1),1:2],2),1) ,
                                                                  labels = c(0.5,round(CI[(bn0-1),1:2],2),1), las=2 )
axis(1, at=c(2,8,16,32) ,labels = c(2,8,16,32) );axis(2, at=c(0.5,round(CI[(bn0-1),1:3],2),1) ,
                                                      labels = c(0.5,round(CI[(bn0-1),1:3],2),1), las=2 )
#abline(h=1-phi^alpha0)

lines(bu,CI[,3],lty=1, col="#4271AE")



lines(bu,CI[,1], lty=3, col ="gray")
lines( bu,CI[,2] ,lty=3, col ="gray")
#segments(2,CI[(bn0-1),1],bn0,CI[(bn0-1),1] ,lty=5)
segments(bn0,CI[(bn0-1),1] ,bn0,CI[(bn0-1),2],lty=1)
#segments(2,CI[(bn0-1),2],bn0,CI[(bn0-1),2] ,lty=5)
points( bn0, CI[(bn0-1),1],pch = "_")
points( bn0, CI[(bn0-1),2],pch="_")
#plot(b, index=40)

#library(extRemes)



l <- 1
sample     <- cbind( pre[index[,j],((l-1)*3+2)] , pre[index[,j],((l-1)*3+3)] , pre[index[,j],((l-1)*3+4)] ) 
sample <- na.omit(sample)
n      <- length(sample[,1])
sam <- sapply(1:n , function(k) max(sample[k,])) #+ abs(rnorm(n,0,0.05))






thet <- extremalindex(sam, quantile(sam,.96))
cithet <- ci.extremalindex(thet, R=2000)
abline(h=cithet[1,1:3] )

