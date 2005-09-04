new.testfdist.landscape <- function () 
{
    rland <- NULL
    rland <- landscape.new.empty()
    rland <- landscape.new.intparam(rland, h = 200, s = 2, np = 0, 
        totgen = 20000)
    rland <- landscape.new.switchparam(rland, mp = 0)
    rland <- landscape.new.floatparam(rland, s = 0, seedscale = c(10, 
        2000, 10, 2000), seedshape = c(1, sqrt(2000), 1, sqrt(2000)), mix = c(0.75, 
        0.75), pollenscale = 50, asp = 1)
    S <- matrix(c(0.33, 0, 0.0175, 0), byrow = T, nrow = 2)
    R <- matrix(c(0, 400, 0, 0), byrow = T, nrow = 2)
    M <- matrix(c(0, 0, 0, 1), byrow = T, nrow = 2)
    rland <- landscape.new.local.demo(rland, S, R, M)
    S <- matrix(rep(0, (2 * rland$intparam$habitat)^2), nrow = 2 * 
        rland$intparam$habitat)
    R <- matrix(rep(0, (2 * rland$intparam$habitat)^2), nrow = 2 * 
        rland$intparam$habitat)
    M <- matrix(rep(0, (2 * rland$intparam$habitat)^2), nrow = 2 * 
        rland$intparam$habitat)
    locs <- landscape.generate.locations(npop = rland$intparam$habitat, 
        xkernel = c(5000, 4000), ykernel = c(5000, 4000), sizexkernel = c(400, 
            50), sizeykernel = c(400, 50))
    rland <- landscape.new.epoch(rland, S = S, R = R, M = M, carry = (1.5 * 
        (sqrt((locs[, 3] - locs[, 1]) * (locs[, 4] - locs[, 2])))), 
        extinct = rep(0.05, rland$intparam$habitat), leftx = locs[, 
            1], rightx = locs[, 3], boty = locs[, 2], topy = locs[, 
            4], maxland = c(min(locs[1]), min(locs[2]), max(locs[3]), 
            max(locs[4])))

    for (z in 1:100)
      rland <- new.locus(rland, type = 0, ploidy = 2, mutationrate = 0.0005, 
                         transmission = 0, numalleles = 5)

    expmat <- cbind(c(0, 0.3, 0.3, 0.4, 0, 0, 0), c(0, 0.7, 0.25, 
        0.05, 0, 0, 0))
    hsq <- c(0.9, 0.9)
    rland <- landscape.new.expression(rland, expmat = expmat, hsq = hsq)
    rland <- landscape.new.individuals(rland, c(rep(0,100),rep(1000,2),rep(0,99)))
    rland
}
