
#' Plot input data for vimes
#'
#' This function makes a series of histograms for data input to vimes.
#'
#' @author Thibaut Jombart \email{thibautjombart@@gmail.com}
#'
#' @export
#' @importFrom graphics hist par
#'
#' @param x a \code{vimes.input} object, as returned by \code{vimes.dist}
#' @param y left for compatibility with the generic 'plot'
#' @param col colors to be used for the histograms
#' @param ... further arguments passed on to \code{hist}
#' @param n the number of histograms displayed per window
#' @param which an integer vector indicating which distances to plot 
#'
#' @examples
#' x1 <- rnorm(20)
#' x2 <- runif(20)
#' names(x1) <- sample(letters, 20)
#' names(x2) <- sample(letters, 20)
#' D1 <- dist(x1)
#' D2 <- dist(x2)
#' out <- vimes.dist(AnkhDist=D1, MorpokDist=D2)
#' out
#' plot(out)
#'
plot.vimes.input <- function(x, y=NULL, col=vimes.pal1(length(x)),
                             n=length(x), which=1:length(x), ...){
    
    ## Some basic processing of the arguments: selecting data to plot,
    ## generating material required for plotting such as color etc.
    
    x <- x[which]
    N <- length(x)
    if(N<1) stop("x is empty")
    if(n<1) stop("n is less than 1")
    col <- rep(col, length=N)

    
    ## Plotting will be a simple series of histograms
    
    par(mfrow=c(n,1))
    for(i in seq_along(x)){
        lab <- ifelse(is.null(names(x)[i]), paste("Distance",i), names(x)[i])
        hist(x[[i]], main=lab, col=col[i], border="white", xlab="Pairwise distance", ...)
    }

    
    ## This kind of thing might upset some. Rich, if you are reading
    ## these lines, here is my attempt at soothing your anger:
    ## https://www.youtube.com/watch?v=3ZsUmoZaivg

    return(invisible(NULL))
}
