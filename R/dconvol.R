#' Convolution of distance distributions
#'
#' This function computes the probability density function of convoolution of various distance distributions, representing distance between cases in time, space or genetic. 
#'
#' @author Anne Cori \email{a.cori@@imperial.ac.uk}
#'
#' @param x vector of quantiles
#' @param type type of distribution to be used, one of "t","g" or "s", see details
#' @param reporting.prob a reporting probability, between 0 and 1
#' @param shape.t Shape of the Gamma distribution used for the serial interval
#' @param scale.t of the Gamma distribution used for the serial interval
#' @param mu mutation rate, NULL by default as not needed for the default analysis of distance in time using only generation time parameters
#' @param sigma.s std of the normal distribution for spatial diffusion in each x/y direction, 
#' NULL by default as not needed for the default analysis of distance in time using only generation time parameters
#' @param tol tolerance; see vignette, the convolution accounting for underreporting uses geometric which will be cut at k.0 so that P(X>=k.0) < tol. 
#' 
#' @details \code{type} can be one of "t","g" or "s". 
#' For \code{type} "t", temporal convolution is performed assuming Gamma distributed serial interval.
#' For \code{type} "g", genetic distance convolution is performed assuming NegBin distribution 
#' (convolution of Gamma distributed serial interval and Poisson distributed number of mutations).
#' For \code{type} "s", spatial Euclidian distance is performed assuming Rayleigh distribution.
#'
#' @export
#'
#' @return the density of the convolution distribution
#'
#' @examples
#'
dconvol <- function(x,
                    type=c("t","g","s"), 
                    reporting.prob = 1, 
                    shape.t, 
                    scale.t, 
                    mu=NULL, 
                    sigma.s=NULL, 
                    tol=0.001 
){
  type <- match.arg(type)
  ### need to add some checks here :-) ###
  
  ### Computing threshold for cutting the sum, based on the tolerance argument ###
  threshold <- stats::qgeom(1-tol, prob=reporting.prob) 
  k <- 0:threshold # number of intermediate cases unobserved over which to integrate
  p2 <- stats::dgeom(k, prob=reporting.prob)
  if(type %in% "t")
  {
    p1 <- stats::dgamma(x,shape=shape.t*(k+1),scale=scale.t)
  }else if(type %in% "g")
  {
    prob <- 1-scale.t*mu/(scale.t*mu+1) # using prob = 1-p intead of p so that our definition correponds to that of rnbinom
    p1 <- stats::dnbinom(x,size=shape.t*(k+1),prob=prob)
    # check that p1 is the same as: choose(shape.t*(k+1)+x-1, x)*prob^(shape.t*(k+1))*(1-prob)^x
  }else if(type %in% "s")
  {
    p1 <- VGAM::drayleigh(x,scale=sigma.s*sqrt(k+1))
  }
  
  return(sum(p1*p2))
}