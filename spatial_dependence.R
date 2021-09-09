#######################################################################
#######################################################################
#######################################################################
########################################################################### Gloria Buriticá
#### Data analysis - Precipitations
#### Spatial dependence Scatter plot
#######################################################################
source("/Users/Buritica/Dropbox/Thèse/git/index_regular_variation/IndexofRV.R")
source("/Users/Buritica/Dropbox/Thèse/git/Auxiliar_functions/random_paths.R")
source("/Users/Buritica/Dropbox/Thèse/git/return_levels/qqplot_confidence_bands.R")
########################################################################
############## Dependence
par(mfrow=c(3,3))
## e.g. mARMAX
#dep    <- c(0.9,0.6,0.3)
#set.seed(2895)
#names2 <- rep(1:3,3)
#names  <- c(TeX("$\\tau = 0.9$"), TeX("$\\tau = 0.6$"), TeX("$\\tau = 0.3$"))
names2 <- c("BREST", "LANVEOC", "QUIMPER", "BORMES", "LE LUC","HYERES", "NANCY", "METZ", "ROVILLE")
names  <- c("North West", "South", "North East")
for( i in 1:3){
  j <- 3
  sample     <- cbind( pre[index[,j],((i-1)*3+2)] , pre[index[,j],((i-1)*3+3)] , pre[index[,j],((i-1)*3+4)] ) 
  #sample     <- mARMAX1(al=4,par0=c(0.7,0.8,0.8),n0=n,dep0=dep[i],d0=3)
  sample     <- na.omit(sample); n          <- length(sample[,1] )
  sample2     <- sort(sapply(1:n, function(k) max(sample[k,]) ))
  for( a in 1:2){
    for(b in 2:3) if(a!= b) {
      plot(sample[   sapply(1:n, function(k) max(sample[k,c(a,b)])) > sample2[floor(n*0.94)] ,c(a,b)] ,col = "black", cex = 0.5, 
           pch=16,ylab=names2[(i-1)*3 + b] , xlab=names2[(i-1)*3 +a],
           main = names[i],
           ylim = c(0,max(sample[,c(a,b)]) ) ,
           xlim = c(0,max(sample[,c(a,b)]) )  )
      points(sample[   sapply(1:n, function(k) max(sample[k,c(a,b)])) <= sample2[floor(n*0.94)] ,c(a,b)] ,col = "lightblue", cex = 0.5, 
             pch=16,ylab=names2[(i-1)*3 + b] , xlab=names2[(i-1)*3 +a],
             main = names[i])
      segments( sample2[floor(n*0.94)], sample2[floor(n*0.94)],0,sample2[floor(n*0.94)], col="lightblue" )
      segments( sample2[floor(n*0.94)],0, sample2[floor(n*0.94)],sample2[floor(n*0.94)] , col="lightblue")
    }
  }
}