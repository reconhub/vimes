## Check that q is non null, numeric and with values betwen 0 and 1

check_quantiles <- function(q)
{
  if(is.null(q)){
    stop("Argument q is missing")
  }
  
  if(!is.numeric(q)){
    stop("q must be numeric")
  }
  
  if(any(q<0 || q>1)){
    stop("all values of q must between 0 and 1")
  }
}

## get quantiles of a function
## f has to be a pdf

get_quantiles <- function(f, q, precision = 0.01, n_steps = 1000)
{
  check_quantiles(q)
  
  ## we approximate the empirical cdf by stepsize precision, over first n_steps steps
  x <- seq(from = 0, by = precision, length = n_steps)
  csm <- cumsum(f(x)) * precision
  iter <- 0
  
  ## if we didn't go far enough to catch the quantile q (or at least one of them)
  ## we reiterate the process as long as needed, by n_steps at a time
  while(max(q) > max(csm))
  {
    iter <- iter + 1
    new_x <- seq(from = max(x)+precision, by = precision, length = n_steps)
    x <- c(x, new_x)
    new_cms <- cumsum(f(new_x))*precision + max(csm)
    csm <- c(csm, new_cms)
  }
  
  ## now we have the approximate cdf up to far enough, 
  ## we find where the quantile(s) q lie in the recorded vector of cdf
  ## quantile defined as the smallest recorded cdf which is > q
  idx_q <- vapply(q, function(e) min(which(csm > e)), 0L)
  out <- x[idx_q]
  ## check <- csm[idx_q] ## compared to q
  
  return(out)
  
}