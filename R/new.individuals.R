"landscape.new.individuals" <-
function(rland, PopulationSizes)
  {
    if (!is.null(rland$intparam)&rland$intparam$habitats*rland$intparam$stages==length(PopulationSizes))
      rland <- .Call("populate_Rland",rland,PopulationSizes,PACKAGE="kernelPop")
    else
      warning("either the Population Size vector does not match other landscape parameters or those parameters have not been declared")
    rland
  }

