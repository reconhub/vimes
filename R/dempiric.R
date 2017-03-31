
#' @export
#'
#' @rdname dpaircase
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
