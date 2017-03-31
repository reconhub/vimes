
## The function get_quanticles is a wrapper around 2 non exported functions
## which approximate quantiles for a given probability density / mass
## function. The main function is get_quantiles_pdf, on which the 'pmf'
## equivalent relies. It creates a 1D regular grid iteratively, which it uses to
## compute the cummulative density/mass, until it reaches the highest quantile
## requested. This grid has a mesh resolution of 'precision' and a size
## 'n_steps'.

## If the function 'f' giving the density/mass is an instance of 'fpaircase',
## then get_quantiles determines if 'f' is continuous automatically.






## Main function to compute the quantiles

get_quantiles_pdf <- function(f, p, precision = 0.01, n_steps = 1000, maxit = 1000, ...) {
  p <- check_proba(p)


  ## we approximate the empirical cdf by stepsize precision, over first n_steps
  ## steps

  x <- seq(from = 0, by = precision, length = n_steps)
  csm <- cumsum(f(x, ...)) * precision
  iter <- 0


  ## if we didn't go far enough to catch the quantile p (or at least one of
  ## them) we reiterate the process as long as needed, by n_steps at a time

  while ( (max(p) > max(csm)) && (iter < maxit) ) {
    iter <- iter + 1
    new_x <- seq(from = max(x)+precision, by = precision, length = n_steps)
    x <- c(x, new_x)
    new_cms <- cumsum(f(new_x, ...))*precision + max(csm)
    csm <- c(csm, new_cms)
  }

  if (iter == maxit) {
    warning("maxit reached, quantile estimation may be unreliable")
  }

  ## now we have the approximate cdf up to far enough, we find where the
  ## quantile(s) p lie in the recorded vector of cdf quantile defined as the
  ## smallest recorded cdf which is > p

  find_one_quantile <- function(e) { # e is a probability
    ## corner cases

    if (e == 0 ) {
      return(0.0)
    }

    if (e == 1) {
      return(Inf)
    }


    ## normal cases

    temp <- csm > e
    if (any(temp)) {
      out_idx <- min(which(temp))
      out <- x[out_idx]
      return(out)
    } else {
      return(NA_real_)
    }
  }

  out <- vapply(p, find_one_quantile, double(1))

  return(out)
}






## Specific function for discrete distributions

get_quantiles_pmf <- function(f, p, n_steps = 1000, maxit = 1000, ...) {
  get_quantiles_pdf(f, p, precision = 1, n_steps = n_steps, maxit = maxit, ...)

}






#' Get quantiles for a given probability distribution
#'
#' These functions approximate quantiles for a probability density function or
#' mass function given by a function \code{f}.
#'
#' @author Anne Cori (a.cori@@imperial.ac.uk) and Thibaut Jombart.
#'
#'
#' @aliases get_quantiles
#'
#' @export
#'
#' @param f A function returning the probability density function (for
#'   \code{get_quantiles_pdf}) or the probability mass function (for
#'   \code{get_quantiles_pmf}.
#'
#' @param p A vector of probabilities.
#'
#' @param continuous A logical indicating if the distribution computed by
#'   \code{f} is continuous (i.e., a probability density function) or discrete
#'   (i.e. a probability mass function). Automatically detected if the function
#'   provided is an instance of \code{\link{fpaircase}}.
#'
#' @param precision The size of the step used to discretise the continuous
#'   distribution. Defaults to 0.01.
#'
#' @param n_steps The number steps used to discretise the continuous
#'   distribution. Defaults to 1000. Note that if \code{n_steps} intervals are
#'   not enough to attain the largest value in \code{p}, batches of
#'   \code{n_steps} are iteratively added.
#'
#' @param maxit The maximum number of batches of \code{n_steps} considered.
#'   Defaults to 1000. Avoids infinite looping when \code{p} is close to 1.
#'
#' @param ... Further arguments passed to the function \code{f}
#'
#' @examples
#'
#' ## reference: exponential of rate 1
#' qexp(c(.4, .5, .95))
#'
#' ## approximation
#' get_quantiles(f = dexp, c(.4,.5,.95), TRUE)
#'
#' ## better precision
#' get_quantiles(f = dexp, c(.4,.5,.95), precision = 0.001, TRUE)
#'
#'
#' ## example with fpaircase
#' f <- fpaircase("spatial", sd_spatial=10)
#' plot(f)
#' plot(f, xlim = c(0, 100))
#' plot(f, xlim = c(0, 100), pi = 0.4)
#'
#' q <- get_quantiles(f, c(.9, .95, .99))
#' q
#' abline(v = q)
#'
#'

get_quantiles <- function(f, p, continuous = NULL, precision = 0.01,
                          n_steps = 1000, maxit = 1000, ...) {
  if (inherits(f, "fpaircase")) {
    continuous <- attr(f, "continuous")
  }

  if (is.null(continuous)) {
    msg <- "continuous is NULL: TRUE or FALSE is needed to determine quantiles"
    stop(msg)
  }

  if (continuous) {
    get_quantiles_pdf(f, p, precision, n_steps, maxit, ...)
  } else {
    get_quantiles_pmf(f, p, n_steps, maxit, ...)
  }

}






