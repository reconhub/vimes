
## Internal functions used for computing distributions of distances for a given
## reporting probability 'pi'. None of these functions are exported, and they
## should not be used directly.

## Authors: Anne Cori (most of the code) and Thibaut Jombart (fidling with code
## structure).



## In all these functions:

## - pi: reporting probability (i.e. fraction of the cases reported)

## - kappa: integer, number of generations between two cases. '1' reflects a
## direct transmission. Numbers must be finite and strictly greater than 0.







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






## This function identifies the maximum value of kappa to be considered for a
## given value of 'pi' and threshold probability 'alpha'. It is defined as the (1 - alpha)
## quantile of the corresponding geometric distribution.

get_max_kappa <- function(pi, alpha = 0.001) {
  out <- stats::qgeom(1 - alpha, pi) + 1L
  return(out)
}






## This function computes weights for various values of kappa. Weights are given
## by the mass function geometric distribution. As it is truncated, we
## re-standardise values to have actual weights summing to 1. 'alpha' is the
## threshold probability used to determine the maximum value of 'kappa'.

get_weights <- function(pi, max_kappa) {
  x_val <- seq_len(max_kappa)

  out <- stats::dgeom(x_val - 1, pi)
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
## function. Prior checks and standardisation are achieved by check_pmf. The argument
## 'keep_all' triggers different behaviours and outputs:

## - keep_all = FALSE: returns the 'kappa' convolution of the pmf

## - keep_all = TRUE: returns all convolutions of the pmf from 1 to kappa; if
## - so, it makes sure that all vectors have the same length.

convolve_empirical <- function(x, kappa, keep_all = FALSE) {
  kappa <- check_kappa(kappa, only_one = TRUE)

  if (kappa == 1) {
    if (keep_all) {
      x <- matrix(x)
      colnames(x) <- "1"
    }
    return(x)
  }


  ## Computations should be checked by Anne

  if (keep_all) {
    out <- list()
    out[[1]] <- x

    for (k in 2:kappa) {
      out[[k]] <- stats::convolve(out[[k-1]],
                                  rev(x),
                                  type="open")
    }


    ## Here we shape the output as a data.frame where each column is a different
    ## value of kappa.

    L <- length(out[[kappa]])
    out <- lapply(out, fill_with, 0, L)
    out <- as.matrix(data.frame(out))
    colnames(out) <- seq_len(ncol(out))
  } else {
    out <- x

    for (k in 2:kappa) {
      out <- stats::convolve(out,
                             rev(x),
                             type="open")
    }
  }
  return(out)
}
