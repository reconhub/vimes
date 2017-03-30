## Check that p is non null, numeric and with values betwen 0 and 1

check_quantiles <- function(p)
{
  if(is.null(p)){
    stop("Argument p is missing")
  }
  
  if(!is.numeric(p)){
    stop("p must be numeric")
  }
  
  if(any(p<0 || p>1)){
    stop("all values of p must between 0 and 1")
  }
}






## get quantiles of a probability density function
## f HAS to be a pdf

get_quantiles_pdf <- function(f, p, precision = 0.01, n_steps = 1000)
{
  check_quantiles(p)
  
  ## we approximate the empirical cdf by stepsize precision, over first n_steps steps
  x <- seq(from = 0, by = precision, length = n_steps)
  csm <- cumsum(f(x)) * precision
  iter <- 0
  
  ## if we didn't go far enough to catch the quantile p (or at least one of them)
  ## we reiterate the process as long as needed, by n_steps at a time
  while(max(p) > max(csm))
  {
    iter <- iter + 1
    new_x <- seq(from = max(x)+precision, by = precision, length = n_steps)
    x <- c(x, new_x)
    new_cms <- cumsum(f(new_x))*precision + max(csm)
    csm <- c(csm, new_cms)
  }
  
  ## now we have the approximate cdf up to far enough, 
  ## we find where the quantile(s) p lie in the recorded vector of cdf
  ## quantile defined as the smallest recorded cdf which is > p
  idx_q <- vapply(p, function(e) min(which(csm > e)), 0L)
  out <- x[idx_q]
  ## check <- csm[idx_q] ## compared to p
  
  return(out)
  
}






## get quantiles of a probability mass function
## f HAS to be a pmf

get_quantiles_pmf <- function(f, p, n_steps = 1000)
{
  out <- get_quantiles_pdf(f, p, precision = 1)
  
  return(out)
  
}