##' @title Gumbel Return Level Plot
##' 
##' @param y A numeric vector containing block maxima or a character
##'     defining a distribution function such as \code{"GEV"},
##'     \code{"exp"}, ...
##' 
##' @param a Passed to \code{\link{ppoints}}
##'
##' @param xTicks The type of ticks on the horizontal axis. With the
##'     choice \code{"Gumbel"}, the ticks show the quantiles of the
##'     standard Gumbel distribution.
##'
##' @param add Logical. If \code{TRUE}
##'
##' @param param Used only when \code{y} is character. A vector with
##'     its named elements corresponding to the parameter arguments of
##'     the distribution function. See the \bold{Examples} section.
##' 
##' @param largestText Used only when \code{y} is numeric. Number of
##'     largest observations for which a label will be added on the
##'     plot. The labels are extracted from the names of \code{y}. See
##'     \bold{Examples}
##'
##' @param ... Arguments passed to \code{plot} or \code{points} such
##'     as \code{main}, \code{col}, ...
##'
##' @return Nothing
##'
##' @importFrom graphics abline
##' @importFrom graphics axis
##' @importFrom graphics points
##' @importFrom graphics text
##' @importFrom stats ppoints
##' 
##' @export
##'
##' @examples
##' library(evd)
##' y <- rgumbel(80)
##' gumbplot(y, xTicks = "T", pch = 16, col = "orangered", a = 0)
##' gumbplot(y, pch = 21, col = "SpringGreen3", cex = 1.2, lwd = 2,
##'          add = TRUE)
##' names(y) <- 2024 - rev(seq_along(y))
##' gumbplot(y, xTicks = "T", largestText = 4,
##'          pch = 16, col = "orangered", a = 0)
##' gumbplot("GEV", param = c(loc = 100, "shape" = -0.1),
##'          main = "GEV distribution on a Gumbel plot")
##' gumbplot("GEV", param = c(loc = 100, "shape" = 0.1), col = "red",
##'           add = TRUE)
##' 
gumbplot <- function(y,
                     a = NULL,
                     xTicks = c("T", "pExc", "Gumbel", "p"),
                     add = FALSE,
                     param = NULL,
                     largestText = 0L,
                     ...) {
    
    probAxis <- match.arg(xTicks)

    if (is.character(y)) {
        pFun <- paste0("p", y)
        qFun <- paste0("q", y)
        pFun <- match.fun(pFun)
        qFun <- match.fun(qFun)
        qaL <- c(list(p = c(0.001, 0.999)), param)
        qx <- do.call(qFun, args = qaL)
        qx <- seq(from = qx[1], qx[2], length.out = 100)
        paL <- c(list(q = qx), param)        
        F <- do.call(pFun, args = paL)
        x <- -log(-log(F))
        y <- qx
        ptype <- "l"
    } else {
        y <- sort(y[!is.na(y)])
        n <- length(y)
        if (!is.null(a)) pp <- ppoints(n, a = a)
        else pp <- ppoints(n)
        x <- -log(-log(pp))
        ptype = "p"
    }
        
    if (probAxis == "T") {
        T <- c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000)
        pBar <- 1 / T
        p <- 1 - pBar
        z <- -log(-log(p))
        labels <- format(T)
        xxlab <- "period T"
    } else if (probAxis == "pExc") {
        T <- c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000)
        pBar <- 1 / T
        p <- 1 - pBar
        z <- -log(-log(p))
        labels <- format(pBar)
        xxlab <- "prob. exceed."
    } else if (probAxis == "Gumbel") {
        z <- seq(from = -2, to = 5, by = 1)
        labels = format(z)
        xxlab <- "Gumbel quantile"
    } else if (probAxis == "p") {
        p <- c(0.1, 0.2, 0.5, 0.9, 0.99, 0.999)
        z <- -log(-log(p))
        pBar <- 1 - p
        labels <- format(p)
        xxlab <- "prob."
    }
    
    if (!add) {
        plot(x = x, y = y, xaxt = "n", type = ptype,
             xlab = xxlab, ylab = "Quantile", ...)
    } else {
        points(x = x, y = y, type = ptype, ylab = "Quantile", ...)
    }
    
    if (!add) {
        axis(side = 1, at = z, labels = labels)
        abline(v = z, lty = "dotted", col = "gray")
        abline(h = pretty(y), lty = "dotted", col = "gray")
    }
    
    if (largestText && is.numeric(y) && !is.null(names(y))) {
        ind <- n + 1 - (1:largestText)
        text(x = x[ind], y = y[ind], labels = names(y)[ind],
             pos = 2)
    }

}
