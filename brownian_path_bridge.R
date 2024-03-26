# Plot the brownian path (for univariate data) conditionnal on values at nodes (and tips) using brownian brdges
brownian_path <- function(data, phy, sigma=1, steps, stochastic=FALSE, plot=TRUE, ...){
 
  # brownian bridge expectation and variance
  bm <- function(s,v0,vt,t) v0 + ((vt - v0)/t)*s 
  bm_var <- function(s,t1, t2) ((t2-s)*(s-t1))/(t2-t1)

  
    ## discretize time
    bmdiscT<-function(n,dt,age,rootage){
      if(n%%dt==0){
        integ<-n%/%dt
        if(integ!=0){
          disc_ages<-rootage-(cumsum( c(0,rep(dt,integ)) )+age)
        }else{
          disc_ages<-rootage-(c(0,n%%dt)+age)
        }
      }else{
        integ<-(n%/%dt)
        if(integ!=0){
          disc_ages<-rootage-(cumsum(c(rep(dt,integ),n%%dt))+age)
          }else{
            disc_ages<-rootage-(c(0,n%%dt)+age)
        }
      }
      return(disc_ages)
  }

  # reorder the tree
  tr<-reorder(phy,"cladewise")
  Ages<-nodeHeights(tr)[,1]
  rootage<-max(nodeHeights(tr))
  paths_list <- times_list <- list()
    
  # loop across the branches
  for(i in 1:nrow(tr$edge)){
    bl <- tr$edge.length[i]
    dt <- seq(from=0, to=bl, by=steps)
    # Expected trajectory under the Brownian bridge
    expected_trajectory <- bm(dt, data[tr$edge[i,1], 1], data[tr$edge[i,2], 1], bl)
    
    if(stochastic){
    
       expected_variance <- bm_var(Ages[i]+dt,t1=Ages[i], t2=Ages[i]+bl) 
       samples_bl = rnorm(length(dt), mean=rep(0,length(dt)), sd=rep(1,length(dt)))
       paths_list[[i]] <- expected_trajectory + samples_bl*(sigma^2*expected_variance)^0.5
    }else{
       paths_list[[i]] <- expected_trajectory 
    }
    
    # save ages
    times_list[[i]] <- bmdiscT(bl, dt=steps, age=Ages[i], rootage)
  }


  ## Plot function
  plot_process<-function(X,T,...){
    arg <- list(...)
    minX<-min(sapply(X,min))
    maxX<-max(sapply(X,max))
    plot(T[[1]][1],X[[1]][1],ylim=c(minX,maxX),xlim=c(0,max(sapply(T,max))),ylab="phenotype",xlab="time")
    for(i in 1:length(X)) lines(T[[i]],X[[i]])
    if(!is.null(arg[["color"]])) cols<-arg$colors
    else cols<-"black"
    return(cols)
  }

 if(plot) plot_process(paths_list, times_list)
 
 return(list(traits=paths_list, times=times_list))
}

## ---------------------------------------- ##
## Worked example                           ##
## ---------------------------------------- ##

set.seed(1)
library(mvMORPH)
phy <- pbtree(n=10)
data <- rTraitCont(phy, sigma=0.1)

ancestral <- fastAnc(phy, data) 
# should bind the traits and ancestral states together in a matrix
DaAnc <- matrix(c(data, ancestral), ncol = 1)
res <- brownian_path(DaAnc, phy=phy, sigma=0.1, steps=0.01, plot=TRUE, stochastic = TRUE )

# plot only the expected trajectory => similar to a traitgram
res2 <- brownian_path(DaAnc, phy=phy, sigma=0.1, steps=0.01, plot=TRUE, stochastic = FALSE )



## Multivariate version
mvbrownian_path <- function(data, phy, sigma=1, steps=0.1, stochastic=FALSE, plot=TRUE, ...){
  
  # number of traits
  p <- ncol(data)
  
  # brownian bridge expectation and variance
  bm <- function(s,v0,vt,t) v0 + matrix(((vt - v0)/t), ncol=p)%x%s 
  bm_var <- function(s,t1, t2) ((t2-s)*(s-t1))/(t2-t1)
  
  
  ## discretize time
  bmdiscT<-function(n,dt,age,rootage){
    if(n%%dt==0){
      integ<-n%/%dt
      if(integ!=0){
        disc_ages<-rootage-(cumsum( c(0,rep(dt,integ)) )+age)
      }else{
        disc_ages<-rootage-(c(0,n%%dt)+age)
      }
    }else{
      integ<-(n%/%dt)
      if(integ!=0){
        disc_ages<-rootage-(cumsum(c(rep(dt,integ),n%%dt))+age)
      }else{
        disc_ages<-rootage-(c(0,n%%dt)+age)
      }
    }
    return(disc_ages)
  }
  
  # reorder the tree
  tr<-reorder(phy,"cladewise")
  Ages<-nodeHeights(tr)[,1]
  rootage<-max(nodeHeights(tr))
  paths_list <- times_list <- list()
  sqrtSigma <- t(chol(sigma))
  
  # loop across the branches
  for(i in 1:nrow(tr$edge)){
    bl <- tr$edge.length[i]
    dt <- seq(from=0, to=bl, by=steps)
    # Expected trajectory under the Brownian bridge
    expected_trajectory <- matrix(bm(dt, data[tr$edge[i,1], 1:p], data[tr$edge[i,2], 1:p], bl), ncol=p, byrow = TRUE)
    
    if(stochastic){
      
      expected_variance <- bm_var(Ages[i]+dt,t1=Ages[i], t2=Ages[i]+bl) 
      samples_bl = matrix(rnorm(length(dt)*p, sd=rep(expected_variance^0.5,p)), nrow=p, byrow = TRUE)
      paths_list[[i]] <- expected_trajectory + t(sqrtSigma%*%samples_bl)
    }else{
      paths_list[[i]] <- expected_trajectory 
    }
    
    # save ages
    times_list[[i]] <- bmdiscT(bl, dt=steps, age=Ages[i], rootage)
  }
  
  
  if(plot){
  
     setuprgl <- function()
     {
       clear3d()
       xyz <- matrix(c(-30,30,0, 30, -30, 100), byrow=TRUE, ncol=3)
       bbox3d(color=c("#333377","black"), emission="white",
              specular="#3333FF", shininess=5, alpha=0.8,
              front="line", back="line", lit=FALSE)
       rgl.viewpoint(0,5, zoom=.75, fov=15);
     }
     # Plot simulations
     setuprgl()
     #rgl.viewpoint(0,5, zoom=.75, fov=15);
     points3d(0, data[1:Ntip(phy),1], data[1:Ntip(phy),2], col="black", size=10) # should add the internal values to the plot
     
     # plot bivariate only
     for (i in 1:nrow(tr$edge))
     {
         lines3d(times_list[[i]], paths_list[[i]][,1], paths_list[[i]][,2], col="black", size=1)
       
     }
     rgl.material(alpha=1)
  
  }
  
  return(list(traits=paths_list, times=times_list))
}

## ---------------------------------------- ##
## Worked example                           ##
## ---------------------------------------- ##
library(rgl)
set.seed(1)
# Test the penalized likelihood
n=7  # observations
p=2 # variables

# Simulating covariance matrix
R_true = matrix(c(0.002,0.001,0.001,0.0015),2)
# Simulating data
phy = pbtree(n=n, scale=10)
data <- mvSIM(phy, model="BM1", nsim=1, param=list(sigma=R_true, theta=rep(0,p)))

ancestral <- apply(data, 2, function(x) fastAnc(phy, x)) 
DaAnc <- rbind(data, ancestral)

res <- mvbrownian_path(DaAnc, phy, sigma=R_true, steps=0.1, stochastic=TRUE, plot=TRUE)

# both functions return a list with sampling time along each branches (in postorder, see phy$edge and "reorder.phylo" function)