"landscape.extinct" <-
function(Rland, seed=-1)
  {
    if (is.landscape(Rland))
      {
        if (!(seed<0))
          {
            set.seed(seed)
          }
        Rland=landscape.coerce(Rland)
        .Call("extinct_landscape",Rland,PACKAGE = "kernelPop")
      }
    else
      {
        print("Rland not a landscape object...exiting")
      }
  }

