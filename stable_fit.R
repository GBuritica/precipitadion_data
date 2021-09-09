#######################################################################
#######################################################################
#######################################################################
########################################################################### Gloria Buriticá
#### Data analysis - Precipitations
#### - qqplot stable fit
#### - ecdfHT
#######################################################################
source("/Users/Buritica/Dropbox/Thèse/git/index_regular_variation/IndexofRV.R")
source("/Users/Buritica/Dropbox/Thèse/git/Auxiliar_functions/random_paths.R")
source("/Users/Buritica/Dropbox/Thèse/git/return_levels/qqplot_confidence_bands.R")
########################################################################
names <- c("North West", "South", "North East")
########################################################################
#### qqstable
par(mfrow = c(2,2))
for(bui in c(60,90,120,150)) qqstable(i=3,j=3,bu=bui) 
#### ecdfHT
library(ecdfHT)
i <- 3;j <- 3
par(mfrow = c(2,2))
for(bu in c(60,90,120,150)){
  ############################################################
  ##### data
  sample     <- na.omit(cbind( pre[index[,j],((i-1)*3+2)] , pre[index[,j],((i-1)*3+3)] , pre[index[,j],((i-1)*3+4)] ) )
  sample     <- na.omit(sample); n      <- length(sample[,1] )
  sample     <- sapply(1:n, function(k) max(sample[k,]) )
  ############################################################
  ##### alpha - stable fit
  #sample     <- abs(rfrechet(n,shape=4))
  a          <- 1/alphaestimator(sample)$xi
  sample3    <- sample^a 
  suma       <- sapply( 1:floor(n/bu) ,function(k) sum(sample3[((k-1)*bu + 1 ):(k*bu)] ))
  recor      <- sort(suma, decreasing = TRUE)
  recor2     <- recor^(1/a); 
  bb         <- stable.fit.mle.restricted( suma,  c(0,1,0,0), c(0,1,0,0)) ## fit
  bb1        <- stable.fit.mle.restricted( suma,  c(1,1,0,0), c(1,1,0,0)) ## fit
  print(bb[1])
  ############################################################
  ############################################################
  ############################################################
  ### Plot
  t.info <- ecdfHT( recor[1:length(recor)], show.axes=FALSE )
  ecdfHT.axes( t.info, x.labels=c(-50,-5,0,5,50), y.labels=c(.001,.01,.1,.5,.9,.99,.999),
               show.vert.gridlines=TRUE, show.horiz.gridline=TRUE, lty=2 )
  title(paste( names[i], "bu=",bu))
  q1 <- qstable(t.info$ecdf, alpha=bb[1], beta=bb[2], gamma=bb[3], delta=bb[4])
  ecdfHT.draw( t.info, q1, t.info$ecdf, col='red',show.ci=TRUE)
  q2 <- qstable(t.info$ecdf, alpha=bb1[1], beta=bb1[2], gamma=bb1[3], delta=bb1[4])
  ecdfHT.draw( t.info, q2, t.info$ecdf, col='blue',show.ci=TRUE)
  legend("bottomright", col=c("red","blue"), legend=c("stable(1)","stable"),pch=16)
}


