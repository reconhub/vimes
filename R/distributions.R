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
#' @param alpha The probability threshold to be used to determine the maximum
#'   value of kappa; this value ('max_kappa') will be the smallest kappa so that
#'   p(kappa > max_kappa) < alpha. Defaults to 0.001.
#'

dpaircase <- function(alpha = 0.001) {

}






#' @export
#'
#' @rdname distributions
#'
dtemporal <- function(alpha = 0.001) {

}






#' @export
#'
#' @rdname distributions
#'
dspatial <- function(alpha = 0.001) {

}






#' @export
#'
#' @rdname distributions
#'
dgenetic <- function(alpha = 0.001) {

}






#' @export
#'
#' @rdname distributions
#'
#' @param x A \code{numeric} vector providing the probability mass function, or
#'   empirical frequencies, of pairwise distances.
#'
dempiric <- function(x, pi, alpha = 0.001) {
  pi <- check_pi(pi)
  x <- check_pmf(x)

  max_kappa <- get_max_kappa(pi, alpha)
  weights <- get_weights(pi, max_kappa)
  distributions <- convolve_empirical(x, max_kappa, TRUE)

  out <- distributions %*% weights
  return(as.vector(out))
}






