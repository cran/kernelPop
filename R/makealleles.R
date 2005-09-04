"makealleles" <-
function(type,numalleles,allelesize,frequencies)
{
  retval <- 0

  if(is.null(frequencies))
    {
      frequencies <- rep(1.0/numalleles, numalleles)
    }

  if(length(frequencies) != numalleles)
    {
      stop("Frequency list is not the right size")
    }
  
  if(type == 0 || type == 1)
    {
      statelst <- seq(1,100)
      retval <- vector("list", numalleles)
      for (x in 1:numalleles)
        {
          retval[[x]]$aindex <- x - 1
          retval[[x]]$birth <- 0
          retval[[x]]$prop <- frequencies[x]
          #uses unirange
          val <- sample(1:length(statelst),1)
          retval[[x]]$state <- statelst[val]
          statelst <- statelst[-val]
        }
    }
  else if(type == 2)
    {
      retval <- vector("list", numalleles)
      for (x in 1:numalleles)
        {
          retval[[x]]$aindex <- x -1
          retval[[x]]$birth <- 0
          retval[[x]]$prop <- frequencies[x]
          retval[[x]]$state <- geneseq(allelesize)
        }
    }
  retval          
}

