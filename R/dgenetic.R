
#' @export
#'
#' @rdname distributions
#'
#' @param gamma_shape,gamma_scale shape and scale of the gamma distribution used for the serial interval
#' @param gamma_rate an alternative way to specify the scale of the gamma distribution used for the serial interval
#' @param poisson_rate rate (i.e. mean) of the poisson distribution used for the per time unit genetic mutation rate

dgenetic <- function(x, gamma_shape, gamma_rate = 1,
                     gamma_scale = 1 / gamma_rate,
                     poisson_rate,
                     pi,
                     alpha = 0.001) {
  pi <- check_one_proba(pi)
  alpha <- check_one_proba(alpha)

  max_kappa <- get_max_kappa(pi, alpha)
  weights <- get_weights(pi, max_kappa)
  distributions <- convolve_gamma_poisson(gamma_shape,
                                          gamma_scale = gamma_scale,
                                          poisson_rate = poisson_rate,
                                          kappa = max_kappa,
                                          keep_all = TRUE)(x)

  out <- distributions %*% weights
  return(as.vector(out))
}
