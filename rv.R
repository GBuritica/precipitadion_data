#######################################################################
#######################################################################
#######################################################################
#######################################################################
#### Gloria Buriticá
#### Daily precipitation records: shape-parameter analysis
#######################################################################
#######################################################################
source("/Users/Buritica/Dropbox/Thèse/git/index_regular_variation/IndexofRV.R")
source("/Users/Buritica/Dropbox/Thèse/git/Auxiliar_functions/random_paths.R")
require(ggplot2)


#### loading data
precip2   <- "/Users/Buritica/Dropbox/Thèse/Data_Philippe/Précipitations/2./tototiti.txt"
pre       <- read.table(file = precip2, header = TRUE, sep = " ")
########## Code analyse par saison
dd<-as.integer(pre$y.date/10000)*10000 
dm<-pre$y.date-dd
dSpring<-(300<dm)&(dm<600)
dSummer<-(600<dm)&(dm<900)
dFall<-(900<dm)&(dm<1200) 
dWinter<-((100<dm)&(dm<300))|((1200<dm)&(dm<1300))
##################################################################3
index <- data.frame( "Winter" =  dWinter, "Summer" = dSummer, "Fall" = dFall, "Spring" = dSpring)
namesSeason <- c("Winter", "Summer", "Fall", "Spring")
names       <- c( "BREST", "LANVEOC", "QUIMPER", "BORMES", "LE.LUC", "HYERES", "NANCY", "METZ", "ROVILLE")
##################################################################
##################################################################
##################################################################
##### Site by Site analysis
##################################################################
##### Estimating rho
par(mfrow = c(3,3))
rho <- data.frame("rho" = NULL, "ville" = NULL, "season" = NULL )
kmax <- floor(n^0.8)#700
for(i in 2:10){
  j <- 1
  sample     <- na.omit( pre[index[,j],i] ); n <- length(sample) ## obtaining sample
  lsorted    <- log(sort(sample))                                ## transforming data
  krhomax    <- floor( min( (sum(!lsorted==-Inf)-1),n, 2*n/log(log(n)) ) )  ## max k for estimating rho
  rhoes      <- sapply( 2:min(krhomax,kmax) , function(l) rho_Estimate2( l, lsorted , n  ) )
  krho       <- which(!is.na(rhoes))
  #rhohat     <- rhoes[krho[length(krho)]]; print(rhohat)
  #rhohat     <- mean(rhoes[rhoes > - 10], na.rm = TRUE) ; print(c( rhohat, i,j ))
  rhohat     <-  median(rhoes, na.rm = TRUE)
  plot( rhoes, main = names[(i-1)] ,  ylim = c(-10,0), ylab = "rho", xlab = "k", pch = 16, cex = 0.4)
  rho <- rbind(rho, c(rhohat, i,j) ) 
  for(j in 2:4){
    sample     <- na.omit( pre[index[,j],i] ); n <- length(sample)
    lsorted    <- log(sort(sample))
    krhomax    <- floor( min( (sum(!lsorted==-Inf)-1),n, 2*n/log(log(n)) ) )  ## max k for estimating rho
    rhoes      <- sapply( 2:min(krhomax,n^0.6) , function(l) rho_Estimate2( l, lsorted , n  ) )
    krho       <- which(!is.na(rhoes))
    #rhohat     <- rhoes[krho[length(krho)]]; print(rhohat)
    rhohat     <- mean(rhoes[rhoes > - 10], na.rm = TRUE) ; print(c( rhohat, i,j ))
    points( rhoes, col = j, pch = 16, cex = 0.4)
    rho <- rbind(rho, c(rhohat, i,j) ) 
  }
}
names(rho) <- c("rho", "ville", "season")
ggplot(rho, aes(y=rho, color = as.factor(season), x=ville ) ) + geom_line()
##################################################################
##### Estimating alpha
par(mfrow = c(3,3))
alphaes <- data.frame("alpha"= NULL, "rho" = NULL, "ville" = NULL, "season" = NULL )
j <- 3
for(i in 2:10){
  sample     <- na.omit( pre[index[,j],i] ); n <- length(sample)
  lsorted    <- log(sort(sample))
  rhoes      <- rho$rho[(rho$ville==i)*(rho$season==j)]
  es         <- gammaes2(lsorted, n,rhoes,1:(n^0.8))
  al         <- 1/(es$hill-es$biais)[150] 
  alphaes    <- rbind(alphaes, c(al, rhoes,names[(i-1)],j))
  plot.ts((es$hill-es$biais), ylim = c(0,0.5) , main = names[(i-1)] ,xlab = 1/al, ylab = "alpha", col ="darkblue")
  #lines( (es$hill), col = "blue") 
  abline(h = 1/al, col = "darkgrey",lty=2 )
}
names(alphaes) <- c("alpha", "rho", "ville", "season")
{
  ggplot(alphaes, aes( x=as.factor(ville),y= 1/as.double(alpha),colour=season )) + 
    geom_point() +
    theme_classic() + xlab("") + ylab("")
}
##################################################################
##################################################################
##################################################################
##################################################################
##### Max Analysis
##################################################################
namesSeason <- c("Winter", "Summer", "Fall", "Spring")
names       <- c( "NO", "SUD", "NE")
########### Estimating rho
par(mfrow = c(3,1))
rho <- data.frame("rho" = NULL, "ville" = NULL, "season" = NULL )
for(i in 1:3){
  j <- 1
  sample     <- cbind( pre[index[,j],((i-1)*3+2)] , pre[index[,j],((i-1)*3+3)] , pre[index[,j],((i-1)*3+4)] ) 
  sample     <- na.omit(sample); n      <- length(sample[,1] )
  sample     <- sapply(1:n, function(k) max(sample[k,]) )
  lsorted    <- log(sort(sample))
  krhomax    <- floor( min( (sum(!lsorted==-Inf)-1),n, 2*n/log(log(n)) ) )  ## max k for estimating rho
  rhoes      <- sapply( 2:min(krhomax,kmax) , function(l) rho_Estimate2( l, lsorted , n  ) )
  #krho       <- which(!is.na(rhoes))
  #rhohat     <- rhoes[krho[length(krho)]]; print(c( rhohat, i,j ))
  #rhohat     <- mean(rhoes[rhoes > - 10], na.rm = TRUE) ; print(c( rhohat, i,j ))
  rhohat     <-  median(rhoes, na.rm = TRUE)
  plot( rhoes, main = names[(i-1)] ,  ylim = c(-10,0), ylab = "rho", xlab = "k")
  rho <- rbind(rho, c(rhohat, i,j ) )
  for(j in 2:4){
    sample     <- cbind( pre[index[,j],((i-1)*3+2)] , pre[index[,j],((i-1)*3+3)] , pre[index[,j],((i-1)*3+4)] ) 
    sample     <- na.omit(sample); n      <- length(sample[,1] )
    sample     <- sapply(1:n, function(k) max(sample[k,]) )
    lsorted    <- log(sort(sample))
    krhomax    <- floor( min( (sum(!lsorted==-Inf)-1),n, 2*n/log(log(n)) ) )  ## max k for estimating rho
    rhoes      <- sapply( 2:min(krhomax,n^0.6) , function(l) rho_Estimate2( l, lsorted , n  ) )
    #krho       <- which(!is.na(rhoes))
    #rhohat     <- rhoes[krho[length(krho)]]; print(c( rhohat, i,j ))
    rhohat     <-  mean(rhoes[rhoes > - 10], na.rm = TRUE); print(c( rhohat, i,j ))
    points(rhoes , col = j )
    rho <- rbind(rho, c( rhohat, i,j ) )
  }
}
names(rho) <- c("rho", "ville", "season")
ggplot(rho, aes(y=rho, color = as.factor(season), x=ville ) ) + geom_point() + ylim(-5,0)
############ Estimating alpha
alphaes <- data.frame("alpha"= NULL, "rho" = NULL, "ville" = NULL, "season" = NULL )
par(mfrow = c(1,3))
j <- 3; kmax <- 600;k <- 200# kW <- n^0.56
for(i in 1:3){
  #################### Creating the sample
  sample     <- cbind( pre[index[,j],((i-1)*3+2)] , pre[index[,j],((i-1)*3+3)] , pre[index[,j],((i-1)*3+4)] ) 
  sample     <- na.omit(sample); n      <- length(sample[,1] )
  sample     <- sapply(1:n, function(k) max(sample[k,]) )
  lsorted    <- log(sort(sample))   ## vector to pass to the gammaes2 function
  #################### Estimating rho parameter 
  krhomax   <- floor( min( (sum(!lsorted==-Inf)-1),n, 2*n/log(log(n)) ) ) ## limits for the k(rho)
  rhoes     <- sapply( 1:min(krhomax, kmax ) , function(l) rho_Estimate2( l, lsorted , n  ) )
  rhohat    <-  median( rhoes , na.rm=TRUE) ## estimate
  ################### Estimating gamma
  es        <- gammaes2(lsorted, n,rhohat,k0=1:kmax)        
  al        <- (es$hill-es$biais)[k]   ## Unbiased estimator
  al2       <- es$hill[k]              ## Hill estimator
  alphaes   <- rbind(alphaes, c(al, rhohat,i,j))   ## updating results
  ################## Defyining Bootstrap statistic
  stathill   <- function( data,l ){
    lpath      <- log(sort(data[l])) 
    esp        <- gammaes2(lpath,n,rhohat,1:kmax)
    return(  c( (esp$hill-esp$biais) , esp$hill ) )
  }
  ################## Bootstrap replicates
  b          <- tsboot(sample, statistic=stathill, R=100, sim = "geom", l = 100 )
  IC1        <- sapply(1:(kmax) , function(l)  boot.ci(b,  type = "perc", index = l )$percent[4:5] )
  IC2        <- sapply(1:kmax , function(l)  boot.ci(b,  type = "perc", index = (kmax + l)  )$percent[4:5] )
  ################## Plots
  plot.ts((es$hill-es$biais), ylim = c(min(IC1, IC2),max((IC1),IC2)) , main = names[i] ,xlab = al, ylab = "xi", col = "darkblue")
  lines( (es$hill), col = "grey") 
  #polygon( x=c(1:(kmax), rev(1:(kmax) ) ), y=c(IC1[2,],rev(IC1[1,]) ), col = "lightblue" , lty=1, density=55, angle = 90)
  #polygon( x=c(1:(kmax), rev(1:(kmax) ) ), y=c(IC2[2,],rev(IC2[1,]) ), col = "grey" , lty=1, density=40, angle = 90)
  sapply(1:2, function(k) lines( IC2[k,], lty = 2, col = "grey"))
  sapply(1:2, function(k) lines( IC1[k,], lty = 2, col = "darkblue"))
  #lines((es$hill), col = "black", lty = 2) 
  #lines((es$hill-es$biais), col = "black", lty=1) 
  abline(h = al , col = "blue")
  mtext(namesSeason[j])
  #abline(h = 1 , col = "red")
  #abline(h = al2 , col = "lightblue")
}
#alphaestimator(sample, plot=TRUE,k1=600,R0=100,hill=TRUE)
names(alphaes) <- c("alpha", "rho", "ville", "season")
#ggplot(alphaes, aes(y=1/alpha, color = as.factor(season), x=ville ) ) + geom_line() + ylim(0,0.6)
