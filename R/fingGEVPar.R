##' @description In order to get familiar with the GEV family if can
##'     be useful to compare GEV densities having the same mean and
##'     the same quantile for the probability 0.99 because these two
##'     quantities can be quite well estimated from a given sample.
##' 
##' @title Find the GEV Parameters for a given Shape, Mean
##' and Quantile
##' 
##' @param xi Given GEV shape.
##' @param mean Given GEV mean.
##' @param q99 Given quantile with probability 0.99.
##' @return A vector with its named elements being the three GEV
##'     parameters
##' @export
##'
##' @examples
##' library(nieve)
##' theta <- findGEVPar(xi = 0.1)
##' qGEV(p = 0.99, loc = theta[1], scale = theta[2], shape = theta[3])
##' qGEV(p = 0.999, loc = theta[1], scale = theta[2], shape = theta[3])
##'
##' theta1 <- findGEVPar(xi = -0.1)
##' qGEV(p = 0.99, loc = theta1[1], scale = theta1[2], shape = theta1[3])
##' qGEV(p = 0.999, loc = theta1[1], scale = theta1[2], shape = theta1[3])
findGEVPar <- function(xi, mean = 0, q99 = 1.0) {
    if (xi == 0.0) {
        gam <- -digamma(1)
        qStar <- qGEV(p = 0.99)
        sigma <- (q99 - mean) / (qStar - gam) 
        mu <- q99 - sigma * qStar
        return(c(log = mu, scale = sigma, shpe = 0.0))
    }
    g1 <- gamma(1 - xi)
    g1Bis <- (g1 - 1.0) / xi
    qGEV99 <- qGEV(p = 0.99, shape = xi)
    sigma <-  (q99 - mean) / (qGEV99 - g1Bis)
    mu <- mean - sigma * g1Bis
    c(loc = mu, scale = sigma, shape = xi)
}

