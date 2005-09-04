 is.landscape <- function (Rland = NULL, verb = TRUE, exact = FALSE) 
{
    ok <- TRUE
    if (!is.list(Rland)) {
        if (verb) {
            print("Landscape not a list")
        }
        ok <- FALSE
    }
    if (is.null(Rland$intparam)) {
        if (verb) {
            print("intparam not found.")
        }
        ok <- FALSE
    }
    if (is.null(Rland$switchparam)) {
        if (verb) {
            print("switchparam not found.")
        }
        ok <- FALSE
    }
    if (is.null(Rland$floatparam)) {
        if (verb) {
            print("floatparam not found.")
        }
        ok <- FALSE
    }
    if (is.null(Rland$demography)) {
        if (verb) {
            print("demography not found.")
        }
        ok <- FALSE
    }
    else {
        for (i in 1:length(Rland$demo$localdem)) {
            if ((min(dim(Rland$demo$localdem[[i]]$LocalS) == 
                c(Rland$intparam$stages, Rland$intparam$stages)) == 
                0) || (min(dim(Rland$demo$localdem[[i]]$LocalR) == 
                c(Rland$intparam$stages, Rland$intparam$stages)) == 
                0) || (min(dim(Rland$demo$localdem[[i]]$LocalM) == 
                c(Rland$intparam$stages, Rland$intparam$stages)) == 
                0)) {
                if (verb) {
                  print("One or more of the local demography matrices is not of the correct dimensions")
                }
                ok <- FALSE
            }
            if (max(apply(Rland$demography$localdem[[i]]$LocalS, 
                2, sum)) > 1) {
                if (verb) {
                  print(paste("Local survival matrix", i, "has a column that sums to a number greater than one"))
                }
                ok <- FALSE
            }
        }
        for (i in 1:length(Rland$demo$epochs)) {
            if ((length(Rland$demo$epochs[[i]]$Extinct) != Rland$intparam$habitats) || 
                (length(Rland$demo$epochs[[i]]$Carry) != Rland$intparam$habitats) || 
                (length(Rland$demo$epochs[[i]]$Localprob) != 
                  Rland$intparam$numdemos) || (min(dim(Rland$demo$epochs[[i]]$S) == 
                c(Rland$intparam$stages * Rland$intparam$habitats, 
                  Rland$intparam$stages * Rland$intparam$habitats)) == 
                0) || (min(dim(Rland$demo$epochs[[i]]$R) == c(Rland$intparam$stages * 
                Rland$intparam$habitats, Rland$intparam$stages * 
                Rland$intparam$habitats)) == 0) || (min(dim(Rland$demo$epochs[[i]]$M) == 
                c(Rland$intparam$stages * Rland$intparam$habitats, 
                  Rland$intparam$stages * Rland$intparam$habitats)) == 
                0)) {
                if (verb) {
                  print(paste("One or more of the epoch paramters is of incorrect dimension in epoch", 
                    i))
                }
                ok <- FALSE
            }
            if (Rland$switchparam$randdemo == 1 || length(Rland$demography$localdem) == 
                1) {
                for (j in 1:length(Rland$demography$localdem)) {
                  strt <- seq(1, dim(Rland$demo$epochs[[i]]$S)[1], 
                    dim(Rland$demography$localdem[[j]]$LocalS)[1])
                  stp <- strt + (dim(Rland$demography$localdem[[j]]$LocalS)[1] - 
                    1)
                  slice <- cbind(strt, stp)
                  tmpS <- Rland$demography$epochs[[i]]$S
                  if (Rland$intparam$habitats != 1) 
                    for (l in 1:Rland$intparam$habitats) {
                      
                      tmpS[slice[l,1]:slice[l,2],slice[l,1]:slice[l,2]] <- Rland$demography$localdem[[j]]$LocalS
                    }
                  if (max(apply(tmpS, 2, sum)) > 1) {
                    if (verb) {
                      print(paste("Columns in the landscape S matrix associated with localdem", 
                        j, "total more than 1"))
                    }
                    ok <- FALSE
                  }
                }
            }
        }
    }
    if (is.null(Rland$loci)) {
        if (verb) {
            print("loci not found.")
        }
        ok <- FALSE
    }
    else {
        if (length(Rland$loci) != Rland$intparam$locusnum) {
            if (verb) {
                print("conflict between size of loci object and size specified in $intparam")
            }
            ok <- FALSE
        }
    }
    if (is.null(Rland$individuals)) {
        if (verb) {
            print("no individuals section found.")
        }
        ok <- FALSE
    }
    else {
        noind <- FALSE
        if (!(dim(Rland$individuals)[1] > 0)) {
            if (verb) {
                print("No individuals in this landscape: spontaneous generation is notallowed")
            }
            ok <- FALSE
            noind <- TRUE
        }
        if (!noind) {
            if (max(Rland$individuals[, 1] > ((Rland$intparam$habitats * 
                Rland$intparam$stages) - 1))) {
                if (verb) {
                  print("There are individuals in stages greater than demography allows")
                }
                ok <- FALSE
            }
            if ((!noind) & (dim(Rland$individuals)[2] != (9 + 
                sum(landscape.ploidy(Rland))))) {
                if (verb) {
                  print("The number of loci do not correspond to the number of columns in $individuals\nThis indicates a problem with locus number and/or ploidy")
                }
                ok <- FALSE
            }
            if ((!noind) & (max(Rland$individuals[, 3]) > Rland$intparam$currentgen)) {
                if (verb) {
                  print("There are individuals born in the future")
                }
                ok <- FALSE
            }
        }
    }
    return(ok)
}
