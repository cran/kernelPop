#
# invasion model in a linear habitat with metapopulation extinction
#

###Take a vector of x,y and return the population numbers
###this function is an internal function and part of the sadism distribution
getpopulations <- function(rland,x,y)
  {
    #  print(x)
    #  print(y)
      popmat <- sapply(1:rland$intparam$habitats,function(i,x,y)
                       {
                         as.logical((floor(rland$demography$epochs[[1]]$leftx[i])<=x)*(ceiling(rland$demography$epochs[[1]]$rightx[i])>=x)*(floor(rland$demography$epochs[[1]]$boty[i])<=y)*(ceiling(rland$demography$epochs[[1]]$topy[i])>=y))
                       },x=x,y=y)
      retval <- unlist(apply(popmat,1,which))[unlist(apply(popmat,1,which))>-1]
      retval[sapply((unlist(apply(popmat,1,which))[unlist(apply(popmat,1,which))>-1]),is.null)] <- 0
      unlist(retval)
    }


seed=as.numeric(format(Sys.time(),"%H%M%S%d"))
set.seed(seed)

cat(date(),file="logfile")
cat("\n",file="logfile",append=T)
cat("seed: ",seed,"\n",file="logfile",append=T)

library(sadism)
source("c-landscape.R")
source("amova-functions.R")
source("fdist.R")

years.per.interval <- 75 #number of years before recording data
intervals <- 2 #multiply years.per.interval to get total length of simulation
phist <- vector("list",intervals)

results <- NULL
reps=20

treat <- expand.grid(erate=c(0,0.1,0.2),mn=c(1500,2000))
typeI <- c(NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL)

###next line for testing comment out for production
#treat <- treat[1:2,]; reps <- 2

for (trt in 1:dim(treat)[1])
  {
    print(treat[trt,]);print(date())
    results <- c(NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL)
    for (r in 1:reps)
      {
        l <- new.testfdist.landscape(erate=treat$erate[trt],dispmean=treat$mn[trt])
        for (i in 1:intervals)
          {
            if (dim(l$individuals)[1]>(25*25))
              {
                l <- landscape.simulate(l,years.per.interval)
                save(file="latest-landscape.rda",l)
                cat(paste("rep ",r,", year ",i*years.per.interval),file="logfile",append=T)
                cat("\n",file="logfile",append=T)
                print(i*years.per.interval)
              }
          }
        if (dim(l$individuals)[1]>(25*25))
          {
            r1 <- landscape.fdist.analysis(l)
            fst <- system("./datacal",intern=T)
            hel <- as.numeric(rowMeans(landscape.exp.het(l),na.rm=F))
            resprop <- sum(table(landscape.populations(l))>24)/l$intparam$habitats
            immprop <- sum(getpopulations(l,l$ind[,4],l$ind[,5])!=getpopulations(l,l$ind[,6],l$ind[,7]))/length(landscape.populations(l))
            results <- rbind(results,cbind(rep(treat$erate[trt],length(r1)),
                                           rep(treat$mn[trt],length(r1)),
                                           rep(r,length(r1)),
                                           r1,
                                           hel,
                                           rep(resprop,length(r1)),
                                           rep(immprop,length(r1)),
                                           rep(fst,length(r1))
                                           )
                             )
          } else {
            cat(paste("rep ",r,"went extinct\n"),file="logfile",append=T)
          }
        cat(date(),file="logfile",append=T)
        cat("\n",file="logfile",append=T)
      }
    typeI <- rbind(typeI,results)
###uncomment next line for production
    save(file="typeI-final3.rda",typeI)
    ##save(file="typeI-debug.rda",typeI)
  }

