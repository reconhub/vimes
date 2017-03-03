
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
    if (!is.numeric(pi)) {
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







## Convolution of Gammas; we use the following analytic result: 
## the sum of k independant Gamma distributed random variables with shape a and scale b
## is Gamma distributed with shape k*a and scale b. 

## Note that kappa here is a single value.

convolve_gamma <- function(shape, rate = 1, scale = 1/rate, kappa) { 
  ## at the moment not vectorized in x, need to work on that
  ## could potentially be done using 
  ## f <- function(x) sapply(x, function(e) stats::dgamma(e, kappa*shape, rate))
  ## but need to work on column/row names to clarify what's x and what's the gamma parameters
    kappa <- check_kappa(kappa, only_one = FALSE)

    f <- function(x) stats::dgamma(x, kappa*shape, rate) 
    return(f) 
}
## the convolution weighted by the geometric weights should eventually look like something like that:
## weighted_convolve_gamma <- function(x, shape, rate = 1, scale = 1/rate, pi, max_kappa = 20) 
## {
##  sum(get_weights(pi, max_kappa)*convolve_gamma(shape, rate=rate, kappa=1:max_kappa)(x))
## }

convolve_gamma_poisson <- function(gamma_shape, gamma_rate = 1, gamma_scale = 1/gamma_rate, poisson_rate, kappa) {
    kappa <- check_kappa(kappa, only_one = TRUE)

    ## ...
}





convolve_spatial <- function(sd, kappa) {
    kappa <- check_kappa(kappa, only_one = TRUE)

    ## ...
}






convolve_empirical <- function(x, kappa) {
    kappa <- check_kappa(kappa, only_one = TRUE)

    ## ...
}

