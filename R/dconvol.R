dconvol <- function(x,type=c("t","g","s"), # the type of distribution to be used 
                    # if "t", temporal convolution is performed assuming Gamma distributed serial interval
                    # if "g", genetic distance convolution is performed assuming NegBin distribution (convolution of Gamma distributed serial interval and Poisson distributed number of mutations) 
                    # if "s", spatial Euclidian distance is performed assuming Rayleigh distribution 
                    reporting_prob = 1, # a reporting probability, between 0 and 1
                    shape_t, # Shape of serial interval
                    scale_t, # Scale of serial interval
                    mu=NULL, # mutation rate # NULL by default as not needed for the deafult temporal analysis
                    sigma_s=NULL, # std of the normal distribution for spatial diffusion in each x/y direction
                    tol=0.001 # tolerance; the geometric distribution will be cut at k_0 so that P(X>=k_0) < tol
){
  type <- match.arg(type)
  ### need to add some checks here :-) ###
  
  ### Computing threshold for cutting the sum, based on the tolerance argument ###
  threshold <- qgeom(1-tol, prob=reporting_prob) 
  k <- 0:threshold # number of intermediate cases unobserved over which to integrate
  p2 <- dgeom(k, prob=reporting_prob)
  if(type %in% "t")
  {
    p1 <- dgamma(x,shape=shape_t*(k+1),scale=scale_t)
  }else if(type %in% "g")
  {
    prob <- 1-scale_t*mu/(scale_t*mu+1) # using prob = 1-p intead of p so that our definition correponds to that of rnbinom
    p1 <- dnbinom(x,size=shape_t*(k+1),prob=prob)
    # check that p1 is the same as: choose(shape_t*(k+1)+x-1, x)*prob^(shape_t*(k+1))*(1-prob)^x
  }else if(type %in% "s")
  {
    p1 <- drayleigh(x,scale=sigma_s*sqrt(k+1))
  }
  
  return(sum(p1*p2))
}