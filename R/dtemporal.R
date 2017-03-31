
#' @export
#'
#' @rdname dpaircase
#'
#' @param shape shape of the gamma distribution used for the serial interval
#'
#' @param scale scale of the gamma distribution used for the serial interval
#'
#' @param rate an alternative way to specify the scale of the gamma distribution
#'   used for the serial interval
#'

dtemporal <- function(x, shape, rate = 1, scale = 1/rate, pi, alpha = 0.001) {
  pi <- check_one_proba(pi)
  alpha <- check_one_proba(alpha)

  max_kappa <- get_max_kappa(pi, alpha)
  weights <- get_weights(pi, max_kappa)
  distributions <- convolve_gamma(shape, scale = scale,
                                  kappa = max_kappa, keep_all = TRUE)(x)

  out <- distributions %*% weights
  return(as.vector(out))
}
