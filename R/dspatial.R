
#' @export
#'
#' @rdname dpaircase
#'
#' @param sd standard deviation of the Normal spatial kernel.

dspatial <- function(x, sd, pi, alpha = 0.001) {
  pi <- check_one_proba(pi)
  alpha <- check_one_proba(alpha)

  max_kappa <- get_max_kappa(pi, alpha)
  weights <- get_weights(pi, max_kappa)
  distributions <- convolve_spatial(sd = sd,
                                    kappa = max_kappa,
                                    keep_all = TRUE)(x)

  out <- distributions %*% weights
  return(as.vector(out))
}
