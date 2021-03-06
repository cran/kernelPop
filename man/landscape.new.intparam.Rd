\name{landscape.new.intparam}
\alias{landscape.new.intparam}
\title{Create a set of integer parameters}

\description{
  Create a set of integer parameters for a Rmetasim landscape.}
\usage{
  ## must be called AFTER landscape.new.empty()
landscape.new.intparam(rland,h=1,s,cg=0,ce=0,totgen=1000,maxland=2e+05,np=0)}
\arguments{
  \item{rland}{skeletion of landscape object, required}
  \item{h}{habitats (default=1), the number of different subpopulations within the landscape}
  \item{s}{stages (default=1), the number of stages in the life cycle of the organism}
  \item{cg}{currentgen (default=0), the current generation the simulation has reached}
  \item{ce}{currentepoch (default=0), the current epoch the simulation has reached}
  \item{totgen}{totoalgens (default=1000), the total number of generations to simulate}
  \item{maxland}{maxlandsize(default=200000), the maxium number of
    individuals that can exist in the simulation} 
  \item{np}{number of phenotypes (not used)}
}
\examples{
  ## Defaults
  exampleland <- landscape.new.empty()
  exampleland <- landscape.new.intparam(exampleland,s=2)
  exampleland$intparam

  ## 2 habitats, 3 stage lifecycle, 1000000 generations, maximum 1000000 individuals
  exampleland <- landscape.new.empty()
  exampleland <- landscape.new.intparam(exampleland,h=2,s=2,totgen=1000000,maxland=1000000)
  exampleland$intparam

  rm(exampleland)
}
\keyword{misc}
