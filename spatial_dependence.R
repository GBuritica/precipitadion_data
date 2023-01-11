#######################################################################
#######################################################################
#######################################################################
########################################################################### Gloria Buriticá
#### Data analysis - Precipitations
#### Spatial dependence Scatter plot
#######################################################################
source("/Users/buritica/Dropbox/Thèse/git/index_regular_variation/IndexofRV.R")
source("/Users/buritica/Dropbox/Thèse/git/Auxiliar_functions/random_paths.R")
source("/Users/buritica/Dropbox/Thèse/git/return_levels/qqplot_confidence_bands.R")
########################################################################
############## Dependence
par(mfrow=c(3,3))
## e.g. mARMAX
#dep    <- c(0.9,0.6,0.3)
#set.seed(2895)
#names2 <- rep(1:3,3)
#names  <- c(TeX("$\\tau = 0.9$"), TeX("$\\tau = 0.6$"), TeX("$\\tau = 0.3$"))
names2 <- c("Brest", "Lanveoc", "Quimper", "Bormes", "Le Luc","Hyeres", "Nancy", "Metz", "Roville")
names  <- c("northwest", "south", "northeast")
for(i in 1:3){
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
           ylim = c(0,max(sample[,1:3]) ) ,
           xlim = c(0,max(sample[,1:3]) )  )
      points(sample[   sapply(1:n, function(k) max(sample[k,c(a,b)])) <= sample2[floor(n*0.94)] ,c(a,b)] ,col = "lightblue", cex = 0.5, 
             pch=16,ylab=names2[(i-1)*3 + b] , xlab=names2[(i-1)*3 +a],
             main = names[i])
      segments( sample2[floor(n*0.94)], sample2[floor(n*0.94)],0,sample2[floor(n*0.94)], col="lightblue" )
      segments( sample2[floor(n*0.94)],0, sample2[floor(n*0.94)],sample2[floor(n*0.94)] , col="lightblue")
    }
  }
}




par(mfrow=c(3,3))
for( i in 1:3){
  j <- 3
  sample     <- cbind( pre[index[,j],((i-1)*3+2)] , pre[index[,j],((i-1)*3+3)] , pre[index[,j],((i-1)*3+4)] ) 
  #sample     <- mARMAX1(al=4,par0=c(0.7,0.8,0.8),n0=n,dep0=dep[i],d0=3)
  sample     <- na.omit(sample); n          <- length(sample[,1] )
  sample2     <- sort(sapply(1:n, function(k) min(sample[k,]) ))
  for( a in 1:2){
    for(b in 2:3) if(a!= b) {
      plot(sample[   sapply(1:n, function(k) min(sample[k,c(a,b)])) > sample2[floor(n*0.95)] ,c(a,b)] ,col = "black", cex = 0.5, 
           pch=16,ylab=names2[(i-1)*3 + b] , xlab=names2[(i-1)*3 +a],
           main = names[i],
           ylim = c(0,sort(sample[,1:3],decreasing=T)[2] ) ,
           xlim = c(0,sort(sample[,1:3],decreasing=T)[2] )  )
      points(sample[   sapply(1:n, function(k) min(sample[k,c(a,b)])) <= sample2[floor(n*0.95)] ,c(a,b)] ,col = "lightblue", cex = 0.5, 
             pch=16,ylab=names2[(i-1)*3 + b] , xlab=names2[(i-1)*3 +a],
             main = names[i])
      segments(sample2[floor(n*0.95)], sample2[floor(n*0.95)], sort(sample[,1:3],decreasing=T)[2], sample2[floor(n*0.95)], col="lightblue" )
      segments(sample2[floor(n*0.95)], 0, sample2[floor(n*0.95)], sample2[floor(n*0.95)], col="lightblue" , lty=2)
      
      segments(sample2[floor(n*0.95)], sample2[floor(n*0.95)], sample2[floor(n*0.95)],sort(sample[,1:3],decreasing=T)[2] , col="lightblue" )
      segments(0,sample2[floor(n*0.95)], sample2[floor(n*0.95)],sample2[floor(n*0.95)], col="lightblue" ,lty=2)
      
    }
  }
}




par(mfrow=c(3,3))
for( i in 1:3){
  j <- 3 ## season
  sample     <- cbind( pre[index[,j],((i-1)*3+2)] , pre[index[,j],((i-1)*3+3)] , pre[index[,j],((i-1)*3+4)] ) 
  #sample    <- mARMAX1(al=4,par0=c(0.7,0.8,0.8),n0=n,dep0=dep[i],d0=3)
  sample     <- na.omit(sample); n          <- length(sample[,1] )
  sample2    <-(sapply(1:n, function(k) max(sample[k,]) ) )
  ind        <- which(sample2 > quantile(sample2, 0.95 ) )   
  
}

library(plot3D)
par(mfrow=c(1,1))
scatter3D(sample[ind,1]/sample2[ind],sample[ind,2]/sample2[ind],sample[ind,3]/sample2[ind], pch = 19, cex = 0.5,  bty='b2',
          xlab='',ylab='',zlab='',
          main = "", 
          col='lightblue')



lm(formula = y ~ I(x1^2) + I(x2^2) +I(x1*x2) + x1 + x2 ) -> lmpoly 
cat( "Equation de la nappe : \n" )
print(lmpoly)


dim = 48
x1.grid <-    seq(0,1,length.out=dim)
x2.grid <-    seq(0,1,length.out=dim)
x1_x2.grid <- expand.grid(x1=x1.grid,x2=x2.grid)
# Calcul des ordonnées des points de la nappe
y.nappe   <-  with(x1_x2.grid, (1-x1-x2)*(x1+x2<1) )
y.mat     <-  matrix(y.nappe,nrow=dim, ncol=dim,byrow=T)



# Calcul des ordonnées des points de la nappe
#y.nappe<-with(x1_x2.grid, 1-x1-x2)
#y.mat<- matrix(y.nappe,nrow=dim, ncol=dim,byrow=T)



# Programmer les couleurs
col<-c("blue","cyan","yellow","green","brown")
colfunc<-colorRampPalette(col);colors <- (colfunc(20))
# Attribuer une couleur par point de y
colorscale <- cbind(seq(0 , 1, by=1/(length(y) - 1)),colors)

# Mise en forme
main = "titre"
font <- list(family = "Courier New, monospace",size = 12,color = "#7f7f7f")
xlabel <- list(title = paste("x1"," (x)"),titlefont = font)
ylabel <- list(title = paste("x2"," (y)"),titlefont = font)
zlabel <- list(title = paste("y"," (z)"),titlefont = font)
scene =  list(xaxis = xlabel ,yaxis = ylabel ,zaxis = zlabel) 

require("plotly")
plot_ly(x = x1.grid, y = x2.grid, z = y.mat) %>%  
  layout(title = main, scene = scene)  %>% add_surface() %>% 
  add_trace(x =x1, y = x2, z = y, mode = 'markers')


#s1 <- na.omit( pre[index[,j],((1-1)*3+2)])
#s2 <- na.omit( pre[index[,j],((2-1)*3+2)])
#s3 <- na.omit( pre[index[,j],((3-1)*3+2)])
#sample     <- cbind( s1^(1/alphaestimator(s1)$xi),
#                     s2^(1/alphaestimator(s2)$xi),
#                     s3^(1/alphaestimator(s3)$xi))

sample2    <-(sapply(1:n, function(k) sum(sample[k,]) ) )
ind        <- which(sample2 > quantile(sample2, 0.95 ) )   
dim = 45
x1 <- sample[ind,1]/sample2[ind]
x2 <- sample[ind,2]/sample2[ind]
x3  <- sample[ind,3]/sample2[ind]


x <- seq(0,1,length.out = dim) 
y <- seq(0,1,length.out = dim)
f <- function(x,y){ z <- -x - y + 1 }
z <- outer(x,y,f)
z <- ifelse(z<0,NA,z)

plot_ly() %>% 
  layout(title = "northwest", scene = list(xaxis = list(title = 'Le luc'),
                                                    yaxis = list(title = 'Hyeres'),
                                                    zaxis = list(title = 'Otra') ))  %>% 
  add_surface(x=x,y=y,z=z ,colorscale = list(c(0, 1), c("lightblue", "lightblue")),
              color='lightblue',
              opacity=0.3 ) %>% 
  add_markers(x =x1, y = x2, z = x3, 
              marker = list(color = ~mpg, 
                            size = 3,
                            colorscale = c('#FFE1A1', '#683531'), 
                            showscale = TRUE)) 



library(latex2exp)
library(plotly)
i <- 3
j <- 3 ## season
sample     <- cbind( pre[index[,j],((i-1)*3+2)] , pre[index[,j],((i-1)*3+3)] , pre[index[,j],((i-1)*3+4)] ) 
#sample    <- mARMAX1(al=4,par0=c(0.7,0.8,0.8),n0=n,dep0=dep[i],d0=3)
sample     <- na.omit(sample); n          <- length(sample[,1] )

names2 <- c("Brest", "Lanveoc", "Quimper", "Bormes", "Le Luc","Hyeres", "Nancy", "Metz", "Roville")
names  <- c("northwest", "south", "northeast")


sample3    <-(sapply(1:n, function(k) max(sample[k,]) ) )
ind        <- which(sample3 > quantile(sample3, 0.95 ) ) 
indc       <- which(sample3 <= quantile(sample3, 0.95 ) ) 

sample2     <-(sapply(1:n, function(k) sort(sample[k,])[2] ) )
ind2        <- which(sample2 > quantile(sample3, 0.95 ) ) 

sample1     <-(sapply(1:n, function(k) min(sample[k,])) )
ind1        <- which(sample1 > quantile(sample3, 0.95 ) ) 



### simplex surface
x <- seq(0,1,length.out = dim) 
y <- seq(0,1,length.out = dim)
f <- function(x,y){ z <- -x - y + 1 }
z <- outer(x,y,f)
z <- ifelse(z<0,NA,z)


### plane surface1
q <- quantile(sample3, 0.95 )
x <- seq(q,max(sample3),length.out = dim) 
y <- seq(q,max(sample3),length.out = dim)
f <- function(x,y){ z <- (x>=0)*(y>=0)*q }
z <- outer(x,y,f)


q <- quantile(sample3, 0.95 )
x1 <- seq(0,q,length.out = dim) 
y1 <- seq(0,q,length.out = dim)
f <- function(x,y){ z <- (x>=0)*(y>=0)*q }
z1 <- outer(x1,y1,f)
#z <- ifelse(z<0,NA,z)


### plane surface2
xx <- rep(q,length.out = dim) 
yy <- seq(q,max(sample3),length.out = dim)
f <- function(x,y){ z <- (x>=0)*y }
zz <- outer(xx,yy,f)

xx1 <- rep(q,length.out = dim) 
yy1 <- seq(0,q,length.out = dim)
f <- function(x,y){ z <- (x>=0)*y }
zz1 <- outer(xx1,yy1,f)


### plane surface3
yyy <- rep(q,length.out = dim) 
xxx <- seq(q,max(sample3),length.out = dim)
f <- function(x,y){ z <- (y>=0)*x }
zzz <- outer(xxx,yyy,f)

yyy1 <- rep(q,length.out = dim) 
xxx1 <- seq(0,q,length.out = dim)
f <- function(x,y){ z <- (y>=0)*x }
zzz1 <- outer(xxx1,yyy1,f)



#z <- ifelse(z<0,NA,z)

plot_ly() %>% 
  layout(title = names[i], scene = list(xaxis = list(title = paste0(names2[(i-1)*3 +1]) ),
                                           yaxis = list(title = paste0(names2[(i-1)*3 +2])),
                                           zaxis = list(title = paste0(names2[(i-1)*3 +3]) )) , showlegend=F)  %>% 
  add_surface(x=x,y=y,z=z ,colorscale = list(c(0, 1), c("lightgray", "lightgray")),
              color='lightgray',showscale = FALSE,
              opacity=0.15 ) %>% 
  add_surface(x=x1,y=y1,z=z1 ,
              color='lightblue',showscale = FALSE,
              opacity=0.2 ) %>% 
  add_surface(x=xx,y=yy,z=zz ,colorscale = list(c(0, 1), c("lightgray", "lightgray")),
              color='lightblugray',showscale = FALSE,
              opacity=0.15 ) %>% 
  add_surface(x=xx1,y=yy1,z=zz1 ,colorscale = list(c(0, 1), c("lightgray", "lightgray")),
              color='lightblue',showscale = FALSE,
              opacity=0.2 ) %>% 
  add_surface(x=xxx,y=yyy,z=zzz ,colorscale = list(c(0, 1), c("lightgray", "lightgray")),
              color='lightgray',showscale = FALSE,
              opacity=0.15 ) %>% 
  add_surface(x=xxx1,y=yyy1,z=zzz1 ,colorscale = list(c(0, 1), c("lightgray", "lightgray")),
              color='lightblue',showscale = FALSE,
              opacity=0.2 ) %>% 
  add_markers(x =sample[ind,1], y = sample[ind,2], z = sample[ind,3], name="one large coordinatee",
              marker = list(color = "green", showscale = FALSE, 
                            size = 4, shape=10,
                            colorscale = c('#FFE1A1', '#683531'))) %>% 
  add_markers(x =sample[ind2,1], y = sample[ind2,2], z = sample[ind2,3], name= "two large coordinates",
            marker = list(color = "orchid", 
                          size = 4,
                          colorscale = c('lightblue', 'lightblue'), 
                          showscale = F)) %>% 
  add_markers(x =sample[ind1,1], y = sample[ind1,2], z = sample[ind1,3], name = "three large coordinates",
            marker = list(color = "black", 
                          size = 4,
                          colorscale = c('lightblue', 'lightblue'), 
                          showscale = F)) %>% 
  add_markers(x =sample[indc,1], y = sample[indc,2], z = sample[indc,3], name="none",
              marker = list(color = "lightblue", showscale = FALSE, 
                            size = 4, shape=10,
                            colorscale = c('#FFE1A1', '#683531'))) %>% 
  config(mathjax = 'cdn')


i <- 1
j <- 3 ## season
sample     <- cbind( pre[index[,j],((i-1)*3+2)] , pre[index[,j],((i-1)*3+3)] , pre[index[,j],((i-1)*3+4)] ) 
#sample    <- mARMAX1(al=4,par0=c(0.7,0.8,0.8),n0=n,dep0=dep[i],d0=3)
sample     <- na.omit(sample); n          <- length(sample[,1] )

names2 <- c("Brest", "Lanveoc", "Quimper", "Bormes", "Le Luc","Hyeres", "Nancy", "Metz", "Roville")
names  <- c("northwest", "south", "northeast")


sample3    <-(sapply(1:n, function(k) max(sample[k,]) ) )
aa         <- 1/alphaestimator(sample3)$xi
ind        <- which(sample3 > quantile(sample3, 0.95 ) ) 
indc       <- which(sample3 <= quantile(sample3, 0.95 ) ) 

sample2     <-(sapply(1:n, function(k) sort(sample[k,])[2] ) )
ind2        <- which(sample2 > quantile(sample3, 0.95 ) ) 

sample1     <-(sapply(1:n, function(k) min(sample[k,])) )
ind1        <- which(sample1 > quantile(sample3, 0.95 ) ) 



### simplex surface
x <- seq(0,1,length.out = dim) 
y <- seq(0,1,length.out = dim)
f <- function(x,y){ z <- -x - y + 1 }
z <- outer(x,y,f)
z <- ifelse(z<0,NA,z)


### plane surface1
q <- quantile(sample3, 0.95 )
x1 <- seq(0,1,length.out = dim) 
y1 <- seq(0,1,length.out = dim)
f <- function(x,y){ z <- (x>=0)*(y>=0)*1 }
z1 <- outer(x1,y1,f)
#z <- ifelse(z<0,NA,z)


### plane surface2
xx1 <- rep(1,length.out = dim) 
yy1 <- seq(0,1,length.out = dim)
f <- function(x,y){ z <- (x>=0)*y }
zz1 <- outer(xx1,yy1,f)


### plane surface3
yyy1 <- rep(1,length.out = dim) 
xxx1 <- seq(0,1,length.out = dim)
f <- function(x,y){ z <- (y>=0)*x }
zzz1 <- outer(xxx1,yyy1,f)



#z <- ifelse(z<0,NA,z)
sample  <- sample^aa
sample3 <- sample3^aa

plot_ly() %>% 
  layout(title = names[i], scene = list(xaxis = list(title = paste0(names2[(i-1)*3 +1]) ),
                                        yaxis = list(title = paste0(names2[(i-1)*3 +2])),
                                        zaxis = list(title = paste0(names2[(i-1)*3 +3]) )) , showlegend=F)  %>% 
  add_surface(x=x1,y=y1,z=z1 ,colorscale = list(c(0, 1), c("lightblue", "lightblue")),
              color='lightblue',showscale = FALSE,
              opacity=0.15 ) %>% 
  add_surface(x=xx1,y=yy1,z=zz1 ,colorscale = list(c(0, 1), c("lightblue", "lightblue")),
              color='lightblue',showscale = FALSE,
              opacity=0.2 ) %>% 
  add_surface(x=xxx1,y=yyy1,z=zzz1 ,colorscale = list(c(0, 1), c("lightblue", "lightblue")),
              color='lightblue',showscale = FALSE,
              opacity=0.2 ) %>% 
  add_markers(x =sample[ind,1]/sample3[ind], y = sample[ind,2]/sample3[ind], z = sample[ind,3]/sample3[ind], name="one large coordinatee",
              marker = list(color = "green", showscale = FALSE, 
                            size = 4, shape=10,
                            colorscale = c('#FFE1A1', '#683531'))) %>% 
  add_markers(x =sample[ind2,1]/sample3[ind2], y = sample[ind2,2]/sample3[ind2], z = sample[ind2,3]/sample3[ind2], name= "two large coordinates",
              marker = list(color = "orchid", 
                            size = 4,
                            colorscale = c('lightblue', 'lightblue'), 
                            showscale = F)) %>% 
  add_markers(x =sample[ind1,1]/sample3[ind1], y = sample[ind1,2]/sample3[ind1], z = sample[ind1,3]/sample3[ind1], name = "three large coordinates",
              marker = list(color = "black", 
                            size = 4,
                            colorscale = c('lightblue', 'lightblue'), 
                            showscale = F))



#### 
# Correlation matrix

i <- 2
j <- 3 ## season
n <- 20000
sample     <- cbind( pre[index[,j],((i-1)*3+2)] , pre[index[,j],((i-1)*3+3)] , pre[index[,j],((i-1)*3+4)] ) 
sample     <- cbind( pre[index[,j], c(8:10)] ) 


#sample    <- mARMAX1(al=4,par0=c(0.7,0.8,0.8),n0=n,dep0=dep[i],d0=3)
sample     <- na.omit(sample); n          <- length(sample[,1] ); d <- length(sample[1,])
#sample     <- sapply(1:d, function(k) ranktransform(sample[,k]) )
names2 <- c("Brest", "Lanveoc", "Quimper", "Bormes", "Le Luc","Hyeres", "Nancy", "Metz", "Roville")
names  <- c("northwest", "south", "northeast")
sapply(1:d, function(k) 1/alphaestimator(sample[,k],k1=170)$xi )

#sample         <- sapply(1:d, function(k) sample[,k]^(1/alphaestimator(sample[,k],k1=200)$xi)  )
sample3    <- (sapply(1:n, function(k) sum(sample[k,]) ) )
ind        <- which(sample3 > quantile(sample3, 0.95 ) ) 

sample2 <- (sample[ind, ]/sample3[ind])
boxplot( sample2)

library("PerformanceAnalytics")
chart.Correlation( sample2, histogram=TRUE, pch=19)
#chart.Correlation( sample, histogram=TRUE, pch=19)


#library(MCMCpack)
#draws <- rdirichlet(200, c(.3,.4,.2) )
#chart.Correlation(draws[,1:2], histogram=TRUE, pch=19)

i <- 1
j <- 3 ## season
n <- 20000
sample     <- cbind( pre[index[,j],((i-1)*3+2)] , pre[index[,j],((i-1)*3+3)] , pre[index[,j],((i-1)*3+4)] ) 
sample     <- cbind( pre[index[,j], c(8:10)] ) 

mat <- matrix(NA,nrow =9,ncol=9)
for( i in 1:9){ ## same region 
  sample1 <- pre[index[,j], (i+1)]
  sample1 <- na.omit(sample1)
  
  for(l in 1:9){
    if (i != l){
      sample2 <- pre[index[,j], (l+1)]
      sample2 <- na.omit(sample2)
      sample3 <- sample1 + sample2
      q       <- quantile(sample3,.95) 
      mat[i,l] <- round( ( mean( (sample1 > q)*(sample2 > q) )/mean( (sample2 > q) ) ),2)
    }

  }
  
}






