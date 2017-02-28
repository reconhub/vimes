
## Internal functions used for computing distributions of distances for a given
## reporting probability 'pi'. None of these functions are exported, and they
## should not be used directly.

## Authors: Anne Cori (most of the code) and Thibaut Jombart (fidling with code
## structure).



## In all these functions:

## - pi: reporting probability (i.e. fraction of the cases reported)

## - kappa: integer, number of generations between two cases. '1' reflects a
## direct transmission. Numbers must be finite and strictly greater than 0.






## Functions which check provided arguments

check_pi <- function(pi) {
  if (!is.numeric(pi)) {
    stop("pi must be numeric")
  }

  if (length(pi) != 1L) {
    stop("pi must have a length of 1")
  }

  if (!is.finite(pi)) {
    stop("non-finite values in pi")
  }

  if(pi < 0 || pi > 1) {
    stop("pi must be between 0 and 1")
  }

  return(pi)
}






check_kappa <- function(kappa, only_one = FALSE) {
  if (!is.numeric(kappa)) {
    stop("kappa must be numeric.")
  }

  if (any(!is.finite(kappa))) {
    stop("Non-finite values in kappa.")
  }

  if(any(kappa < 1)) {
    stop("kappa must be strictly positive.")
  }

  kappa <- as.integer(round(kappa))

  if (only_one) {
    if (length(kappa) != 1L) {
      stop("Only one value of kappa expected.")
    }
  }

  return(kappa)
}






## 'pmf' stands for probability mass function; basically we check that all
## values are positive, finite numbers and we standardise it so that it sums to
## one.

check_pmf <- function(x) {
  if (!is.numeric(x)) {
    stop("x must be numeric.")
  }

  if (any(!is.finite(x))) {
    stop("Non-finite values in x.")
  }

  if(any(x < 0)) {
    stop("x must be positive.")
  }

  x <- x / sum(x)

  return(x)
}






## This function computes weights for various values of kappa. Weights are given
## by the mass function geometric distribution. As it is truncated, we
## re-standardise values to have actual weights summing to 1.

get_weights <- function(pi, max_kappa = 20) {
  pi <- check_pi(pi)
  max_kappa <- check_kappa(max_kappa)

  x_val <- seq_len(max_kappa)

  out <- stats::dgeom(x_val-1, pi)
  out <- out / sum(out)
  return(out)
}







## Convolution of Gammas; we use the following analytic result: ... TBC by Anne

## Note that kappa here is a single value.

convolve_gamma <- function(shape, rate, kappa) {
  kappa <- check_kappa(kappa, only_one = TRUE)

  ## ...
}






convolve_gamma_poisson <- function(gamma_shape, gamma_rate, poisson_rate, kappa) {
  kappa <- check_kappa(kappa, only_one = TRUE)

  ## ...
}





convolve_spatial <- function(sd, kappa) {
  kappa <- check_kappa(kappa, only_one = TRUE)

  ## ...
}






## This convolution supposes that the user has specified a probability mass
## function. Checks and standardisation are achieved by check_pmf.

convolve_empirical <- function(x, kappa) {
  x <- check_pmf(x)
  kappa <- check_kappa(kappa, only_one = TRUE)


  ## Computations should be checked by Anne

  out <- x
  if (kappa > 1) {
    for (k in 2:kappa) {
      out <- stats::convolve(out,
                             rev(x),
                             type="open")

    }
  }

  return(out)
}

