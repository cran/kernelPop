#
# export the data in fdist format
#
landscape.write.fdist <- function(l,popnum=20,samp=24)
  {
    if (is.landscape(l)) #input error check
      {
        fn="infile"
        
        cat("0\n",file=fn)
        l <- landscape.sample(l,np=popnum,ns=samp)
        cat(paste(length(unique(landscape.populations(l))),"\n"),file=fn,append=T)
        cat(paste(length(l$loci),"\n"),file=fn,append=T)

        for (i in 1:length(l$loci))
          {
            genotypes <- landscape.locus(i,l)[,-1:-landscape.democol()]
            alleles <- rbind(cbind(landscape.populations(l),genotypes[,1]),
                             cbind(landscape.populations(l),genotypes[,1]))
            atbl <- table(alleles[,1],alleles[,2])
            cat(paste(dim(atbl)[2],"\n"),file=fn,append=T)
            tmp <- apply(atbl,1,function(x){cat(paste(x," ",sep=''),file=fn,append=T);cat("\n",file=fn,append=T)})
          }
        ## write a parameter file for fdist
        fn="fdist_params2.dat"
        cat("100\n",file=fn)
        cat(paste(length(unique(landscape.populations(l))),"\n"),file=fn,append=T)
        cat(system("./datacal",intern=T),file=fn,append=T)
        cat("\n",file=fn,append=T)
        cat(paste(2*samp,"\n"),file=fn,append=T)
        cat("1\n",file=fn,append=T)
        cat("20000\n",file=fn,append=T)
      }
  }

landscape.fdist.analysis <- function(l)
  {
    landscape.write.fdist(l)
    tmp <- system("./fdist2",intern=T)
    tmp <- system('echo "data_fst_outfile pvalue.out out.dat" | ./pv',intern=T)
    probs <- as.numeric(read.table("pvalue.out")[,4])
  }
