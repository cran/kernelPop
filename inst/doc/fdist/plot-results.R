#
# routines to read the results of fdist type I error tests and plot them in 
# some sort of concise and meaningful manner
# also run some hypothesis tests on the effects of various simulation parameters
# on typeI error



library(lattice)

#load  reps 
load("typeI-final1.rda")
typeI.1 <- data.frame(typeI)

load("typeI-final2.rda")
typeI.2 <- data.frame(typeI)

load("typeI-final3.rda")
typeI.3 <- data.frame(typeI)

typeI <- rbind(typeI.1,typeI.2,typeI.3)

names(typeI) <- c("extinct","mnlong","rep","fdist.prob","He","res.rate","prop.immigrants","Fst")
typeI$fdist.prob <- as.numeric(as.character(typeI$fdist.prob))
typeI$He <- as.numeric(as.character(typeI$He))

#calculate the typeI error rates pre replicated experiment
tail2.per.rep=aggregate(typeI[,4],by=list(extinct=typeI[,1],mnlong=typeI[,2],rep=typeI[,3]),function(x){sum(x[x>0]<0.025|x[x>0]>0.975)/length(x[x>0])})

het=aggregate(typeI[,5],by=list(extinct=typeI[,1],mnlong=typeI[,2],rep=typeI[,3]),function(x){sum(x[x>0]<0.025|x[x>0]>0.975)/length(x[x>0])})
het$het <- het$x
het <- het[,-4]

tail2.per.rep$extinct <- as.character(tail2.per.rep$extinct)
tail2.per.rep$mnlong <- as.character(tail2.per.rep$mnlong)
tail2.per.rep$rep <- as.character(tail2.per.rep$rep)


het$extinct <- as.character(het$extinct)
het$mnlong <- as.character(het$mnlong)
het$rep <- as.character(het$rep)

typeI$extinct <- as.character(typeI$extinct)
typeI$mnlong <- as.character(typeI$mnlong)
typeI$rep <- as.character(typeI$rep)

t2 <-merge(het,tail2.per.rep)
tail2.per.rep <- unique(merge(t2,typeI[,c(-4,-5)]))

tail2.per.rep$extinct <- as.numeric(as.character(tail2.per.rep$extinct))
tail2.per.rep$mnlong <- as.numeric(as.character(tail2.per.rep$mnlong))
tail2.per.rep$extinct <- as.numeric(as.character(tail2.per.rep$extinct))
tail2.per.rep$prop.immigrants <- as.numeric(as.character(tail2.per.rep$prop.immigrants))
tail2.per.rep$res.rate <- as.numeric(as.character(tail2.per.rep$res.rate))
#
### qmixed.sc is found in the kernelPop R folder
#
tail2.per.rep$ldd <- qmixed.sc2(0.95,scale=100,mix=0.6,scale2=tail2.per.rep$mnlong)
tail2.per.rep$Fst <- as.numeric(as.character(tail2.per.rep$Fst))

###hack, but it works for my sims.  qmixed.sc has an unfixed bug
tail2.per.rep$ldd[tail2.per.rep$mnlong==500] <- 955

#
# add a tiny bit of jitter to ldd
#
tail2.per.rep$ldd.jit=rnorm(dim(tail2.per.rep)[1],mean=0,sd=4)+tail2.per.rep$ldd



########################################analysis and plot actually used in paper

print(summary(lm(x~ldd*extinct*het,data=tail2.per.rep)))
print(summary(lm(x~ldd,data=tail2.per.rep)))


#
# subset tail2.per.rep to only include the lower values of ldd because I want to look
# at the system as a metapopulation with significant extinction-recolonization dynamics.
# really high ldd makes every deme always occupied.  This effect occurs at about LDD = 2000
#given the distribution of demes in space.  Here I subset to include only ldd<2000
#
print(summary(lm(x~ldd*extinct*het,data=tail2.per.rep,subset=tail2.per.rep$ldd<2000)))
print(summary(lm(x~ldd,data=tail2.per.rep,subset=tail2.per.rep$ldd<2000)))



postscript("figure2.eps",paper="special",horizontal=F,width=6,height=5)
layout(matrix(c(1,0,
                2,0,
                3,4),nrow=3,byrow=T),widths=c(10,1),heights=c(2,3,5))

par(mar=c(0,4,1,0))
with(tail2.per.rep,
     plot((res.rate)~ldd.jit,ylab="Occupied sites",xlab="Long-distance dispersal",cex=0.25,type=c("p"),axes=F,frame.plot=T,pch=16)
     )
axis(2)
medx <- with(tail2.per.rep,aggregate(res.rate,by=list(ldd=ldd),median,na.rm=T))
medx$ldd <- as.numeric(as.character(medx$ldd))
points(medx,type="b",lty=1,pch=17,cex=2)
par(mar=c(4,0.05,0,1))

par(mar=c(0,4,0,0))
with(tail2.per.rep,
     plot(Fst~ldd.jit,ylab="Fst",xlab="Long-distance dispersal",cex=0.25,type=c("p"),axes=F,frame.plot=T,pch=16)
     )
axis(2)
medx <- with(tail2.per.rep,aggregate(Fst,by=list(ldd=ldd),median,na.rm=T))
medx$ldd <- as.numeric(as.character(medx$ldd))
points(medx,type="b",lty=1,pch=17,cex=2)
fst.fit <- lm(Fst~ldd.jit,data=tail2.per.rep)
print(summary(fst.fit))
#abline(coef(fst.fit))
par(mar=c(4,4,0,0.1))
with(tail2.per.rep,
     plot(x~ldd.jit,ylab="Type I error rate",xlab="Long-distance dispersal",cex=0.25,type=c("p"),
          axes=T,pch=16)
     )
#abline(coef(lm(x~ldd,data=tail2.per.rep)))
abline(c(0.05,0),lty=3,lwd=0.75)

medx <- with(tail2.per.rep,aggregate(x,by=list(ldd=ldd),median,na.rm=T))
medx$ldd <- as.numeric(as.character(medx$ldd))
points(medx,type="b",lty=1,pch=17,cex=2)
par(mar=c(4,0.05,0,1))

###yhist <- hist(tail2.per.rep$x,plot=F)
###top <- max(yhist$counts)
###barplot(yhist$counts,horiz=T,axes=FALSE, xlim=c(0, top), space=0)

boxplot(tail2.per.rep$x,axes=F,notched=T)

layout(matrix(1))
dev.off()

