#
#
#routines to generate the locations of habitats in the landscape.  Should be able to specify
#distribution of populations and distribution of sizes approximately and boundaries of the
#landscape exactly
#

landscape.generate.locations <- function(npop=10,
                               xkernel=c(3000,1000),ykernel=xkernel,
                               sizexkernel=c(300,80),sizeykernel=sizexkernel,
                               boundaries=NULL
                               )
  {
    if(is.null(boundaries))
      {
        lft <- abs(rnorm(npop,mean=xkernel[1],sd=xkernel[2]))
        bot <- abs(rnorm(npop,mean=ykernel[1],sd=ykernel[2]))
        rgt <- lft+abs(rnorm(npop,mean=sizexkernel[1],sd=sizexkernel[2]))
        top <- bot+abs(rnorm(npop,mean=sizeykernel[1],sd=sizeykernel[2]))
        overlap=TRUE
        while (overlap)
          {
            overlap=FALSE
            regen=rep(FALSE,npop)
            for (i in 1:npop)
              for(j in 1:npop)
                {
                 
                  if ((lft[i]>lft[j]) & (lft[i]<rgt[j]) &
                      (bot[i]>bot[j]) & (bot[i]<top[j]))
                    {
                      overlap=T
                      regen[i]=T
                      break
                    }
                  if ((rgt[i]>lft[j]) & (rgt[i]<rgt[j]) &
                      (top[i]>bot[j]) & (top[i]<top[j]))
                    {
                      overlap=T
                      regen[i]=T
                      break
                    }

                  if ((lft[i]>lft[j]) & (lft[i]<rgt[j]) &
                      (top[i]>bot[j]) & (top[i]<top[j]))
                    {
                      overlap=T
                      regen[i]=T
                      break
                    }
                  if ((rgt[i]>lft[j]) & (rgt[i]<rgt[j]) &
                      (bot[i]>bot[j]) & (bot[i]<top[j]))
                    {
                      overlap=T
                      regen[i]=T
                      break
                    }
                  
                  if(FALSE)
                    {
                      overlap <- overlap | line.intersect(
                                                          rbind(c(lft[i],bot[i]),c(rgt[i],bot[i])),
                                                          rbind(c(lft[j],bot[j]),c(lft[j],top[j]))
                                                          )
                      
                      overlap <- overlap | line.intersect(
                                                          rbind(c(lft[i],bot[i]),c(rgt[i],bot[i])),
                                                          rbind(c(rgt[j],bot[j]),c(rgt[j],top[j]))
                                                          )
                      
                      overlap <- overlap | line.intersect(
                                                          rbind(c(lft[i],top[i]),c(rgt[i],top[i])),
                                                          rbind(c(lft[j],bot[j]),c(lft[j],top[j]))
                                                          )
                      
                      overlap <- overlap | line.intersect(
                                                          rbind(c(lft[i],top[i]),c(rgt[i],top[i])),
                                                          rbind(c(rgt[j],bot[j]),c(rgt[j],top[j]))
                                                          )
                      if (overlap) {regen[i] <- T}
                    }
                }
            lft[regen] <- abs(rnorm(sum(regen),mean=xkernel[1],sd=xkernel[2]))
            bot[regen] <- abs(rnorm(sum(regen),mean=ykernel[1],sd=ykernel[2]))
            rgt[regen] <- lft[regen]+abs(rnorm(sum(regen),mean=sizexkernel[1],sd=sizexkernel[2]))
            top[regen] <- bot[regen]+abs(rnorm(sum(regen),mean=sizeykernel[1],sd=sizeykernel[2]))
          }
      }
    cbind(lft,bot,rgt,top)
  }

line.intersect <- function(line1,line2)
  {
    ret=F
    if (((max(line1[,1])<=min(line2[,1]))|
         (min(line1[,1])>=max(line2[,1])))&
        ((max(line1[,2])<=min(line2[,2]))|
         (min(line1[,2])>=max(line2[,2]))))
      ret=FALSE
    else
      {
        #line 1 is horizontal
        line1=line1[order(line1[,1]),]
        line2=line2[order(line2[,2]),]
        if (((line1[1,1]<=line2[1,1])&(line1[2,1]>=line2[1,1]))&
            (((line1[1,2]>=line2[1,2])&(line1[2,1]<=line2[2,2]))))
             ret=TRUE
        #line 1 is vertical
        line1=line1[order(line1[,2]),]
        line2=line2[order(line2[,1]),]
        if (((line2[1,1]<=line1[1,1])&(line2[2,1]>=line1[1,1]))&
            (((line2[1,2]>=line1[1,2])&(line2[2,1]<=line1[2,2]))))
             ret=TRUE
      }
    ret
  }
