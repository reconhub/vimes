





## get quantiles of a probability density function
## f HAS to be a pdf

#' Get quantiles for a given probability distribution
#'
#' These functions approximate quantiles for a probability density function or
#' mass function given by a function \code{f}.
#'
#' @author Anne Cori (a.cori@@imperial.ac.uk) and Thibaut Jombart.
#'
#'
#' @aliases get_quantiles
#' @aliases get_quantiles_pdf
#'@aliases get_quantiles_pmf
#'
#' @rdname get_quantiles
#'
#' @export
#'
#' @param f A function returning the probability density function (for
#'   \code{get_quantiles_pdf}) or the probability mass function (for
#'   \code{get_quantiles_pmf}.
#'
#' @param p A vector of probabilities.
#'
#' @param precision The size of the step used to discretise the continuous
#'   distribution. Defaults to 0.01.
#'
#' @param n_steps The number steps used to discretise the continuous
#'   distribution. Defaults to 1000. Note that if \code{n_steps} intervals are
#'   not enough to attain the largest value in \code{p}, batches of
#'   \code{n_steps} are iteratively added.
#'
#' @examples
#'
#' ## reference: exponential of rate 1
#' qexp(c(.4, .5, .95))
#'
#' ## approximation
#' get_quantiles_pdf(f = dexp, c(.4,.5,.95))
#'
#' ## better precision
#' get_quantiles_pdf(f = dexp, c(.4,.5,.95), precision = 0.001)
#'
get_quantiles_pdf <- function(f, p, precision = 0.01, n_steps = 1000, ...) {
  p <- check_proba(p)

  ## we approximate the empirical cdf by stepsize precision, over first n_steps
  ## steps

  x <- seq(from = 0, by = precision, length = n_steps)
  csm <- cumsum(f(x, ...)) * precision
  iter <- 0


  ## if we didn't go far enough to catch the quantile p (or at least one of
  ## them) we reiterate the process as long as needed, by n_steps at a time

  while (max(p) > max(csm)) {
    iter <- iter + 1
    new_x <- seq(from = max(x)+precision, by = precision, length = n_steps)
    x <- c(x, new_x)
    new_cms <- cumsum(f(new_x, ...))*precision + max(csm)
    csm <- c(csm, new_cms)
  }


  ## now we have the approximate cdf up to far enough, we find where the
  ## quantile(s) p lie in the recorded vector of cdf quantile defined as the
  ## smallest recorded cdf which is > p

  idx_q <- vapply(p, function(e) min(which(csm > e)), 0L)
  out <- x[idx_q]
  ## check <- csm[idx_q] ## compared to p

  return(out)

}






#' @rdname get_quantiles
#' @export
get_quantiles_pmf <- function(f, p, n_steps = 1000, ...) {
  get_quantiles_pdf(f, p, precision = 1, n_steps = n_steps, ...)

}
