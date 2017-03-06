#' Expected distributions of distances
#'
#' These functions compute the expected distributions of distances between pairs
#' of cases given a case reporting probability 'pi'. Analytical results are used
#' for some special cases, including:
#'
#' \itemize{
#'
#' \item temporal distances: the serial interval is assumed to be
#' gamma-distributed; the procedure returns a discretized, weighted convolution
#' of gamma distributions.
#'
#' \item genetic distances: assumed ... TBC
#'
#' }
#'
#'
#' @author Anne Cori (a.cori@@imperial.ac.uk) and Thibaut Jombart
#' (thibautjombart@@gmail.com).
#'
#' @export
#'
#' @rdname distributions
#'
#' @param x vector of quantiles.
#' 
#' @param pi The reporting probability, i.e. the proportion of cases of the
#'   outbreak that have been reported.
#'
#' @param alpha The probability threshold to be used to determine the maximum
#'   value of generations between two successive cases to consider; 
#'   this value ('max_kappa') will be the smallest k so that
#'   p(k > max_kappa) < alpha. Defaults to 0.001.
#'   

dpaircase <- function(pi, alpha = 0.001) {
  
}






#' @export
#'
#' @rdname distributions
#'

dtemporal <- function(pi, alpha = 0.001) {
  
}






#' @export
#'
#' @rdname distributions
#' 
#' @param sd standard deviation of the Normal spatial kernel.

dspatial <- function(x, sd, pi, alpha = 0.001) {
  pi <- check_one_proba(pi)
  alpha <- check_one_proba(alpha)
  
  max_kappa <- get_max_kappa(pi, alpha)
  weights <- get_weights(pi, max_kappa)
  distributions <- convolve_spatial(sd=sd, kappa=max_kappa, keep_all=TRUE)(x)
  
  out <- distributions %*% weights
  return(as.vector(out))
}






#' @export
#'
#' @rdname distributions

dgenetic <- function(pi, alpha = 0.001) {
  
}






#' @export
#'
#' @rdname distributions
#'
#' @param p A \code{numeric} vector providing the probability mass function, or
#'   empirical frequencies, of pairwise distances.

dempiric <- function(p, pi, alpha = 0.001) {
  pi <- check_one_proba(pi)
  alpha <- check_one_proba(alpha)
  p <- check_pmf(p)
  
  max_kappa <- get_max_kappa(pi, alpha)
  weights <- get_weights(pi, max_kappa)
  distributions <- convolve_empirical(p, max_kappa, TRUE)
  
  out <- distributions %*% weights
  return(as.vector(out))
}
