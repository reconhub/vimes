#' Expected distributions of distances
#'
#' These functions compute the expected distributions of distances between pairs
#' of cases given a case reporting probability 'pi'. Analytical results are used
#' for some special cases, including:
#'
#' \itemize{
#'
#' \item temporal distances: the serial interval is assumed to be
#' gamma-distributed; the procedure returns a discretized, weighted convolution
#' of gamma distributions.
#'
#' \item genetic distances: assumed ... TBC
#'
#' }
#'
#'
#' @author Anne Cori (a.cori@@imperial.ac.uk) and Thibaut Jombart
#' (thibautjombart@@gmail.com).
#'
#' @export
#'
#' @rdname distributions
#'
#' @param x vector of quantiles.
#' 
#' @param pi The reporting probability, i.e. the proportion of cases of the
#'   outbreak that have been reported.
#'
#' @param alpha The probability threshold to be used to determine the maximum
#'   value of generations between two successive cases to consider; 
#'   this value ('max_kappa') will be the smallest k so that
#'   p(k > max_kappa) < alpha. Defaults to 0.001.
#'   
#' @examples
#'  #### compare dempiric and dtemporal ####
#'  
#'  ## compute empirical distribution correponding to exponential(mean 50)
#'  mean_exp <- 50
#'  x <- 0:300
#'  reporting_rate <- 0.5
#'  p <- dgamma(x, shape=mean_exp, rate=1) 
#'        # computes pdf of a gamma distr with shape mean_exp and scale=rate=1 
#'        # (i.e. an exponential distr with mean mean_exp)
#'
#'  ## use this as an empirical distribution to feed into dempiric
#'  empiric_exp_distr_with_underreporting <- dempiric(p, pi=reporting_rate)
#'  temporal_distr_with_underreporting <- dtemporal(x, 
#'      shape=mean_exp, rate=1, pi=reporting_rate)
#'  
#'  ## compare the two
#'  correlation <- cor(empiric_exp_distr_with_underreporting[1:length(x)],
#'      temporal_distr_with_underreporting)
#'  
#'  ## graphical comparison
#'  plot(x, empiric_exp_distr_with_underreporting[1:length(x)], xlab="Time", ylab="pdf", 
#'        main="Pdf of time between a case and its closest ancestry in dataset
#'              when SI is exponentially distributed with mean 50,
#'              and reporting probability is 0.5", cex.main=1, pch=3)
#'  lines(x, temporal_distr_with_underreporting, col="red")
#'  legend("topright",c("using dempiric","using dtemporal"), 
#'      pch=c(3, -1), lwd=c(-1, 1), col=c("black","red"), bty="n")
#'      
#'      
#'  #### compare dempiric and dgenetic ####
#'  
#'  ## compute empirical distribution correponding to a Exponential(mean 50)-Poisson(mean 0.6) mixture
#'  mean_exp <- 50
#'  mutation_rate <- 0.6
#'  x <- 0:300
#'  reporting_rate <- 0.5
#'  prob <- 1-mutation_rate/(mutation_rate+1)
#'  p <- dnbinom(x, size=mean_exp,prob=prob) 
#'        # computes pmf of a negative binomial distr with parameters size and prob
#'
#'  ## use this as an empirical distribution to feed into dempiric
#'  empiric_exp_distr_with_underreporting <- dempiric(p, pi=reporting_rate)
#'  genetic_distr_with_underreporting <- dgenetic(x, 
#'      gamma_shape=mean_exp, gamma_rate=1, poisson_rate=mutation_rate, pi=reporting_rate)
#'  
#'  ## compare the two
#'  correlation <- cor(empiric_exp_distr_with_underreporting[1:length(x)],
#'      genetic_distr_with_underreporting)
#'  
#'  ## graphical comparison
#'  plot(x, empiric_exp_distr_with_underreporting[1:length(x)], xlab="Number of mutations", ylab="pmf", 
#'        main="Pmf of number of mutations between a case and its closest ancestry in dataset
#'              when SI is exponentially distributed with mean 50,
#'              mutation rate per time unit is 0.6, and reporting probability is 0.5"
#'              , cex.main=1, pch=3)
#'  lines(x, genetic_distr_with_underreporting, col="red")
#'  legend("topright",c("using dempiric","using dgenetic"), 
#'      pch=c(3, -1), lwd=c(-1, 1), col=c("black","red"), bty="n")



dpaircase <- function(pi, alpha = 0.001) {
  
}






#' @export
#'
#' @rdname distributions
#'
#' @param shape,scale shape and scale of the gamma distribution used for the serial interval
#' @param rate an alternative way to specify the scale of the gamma distribution used for the serial interval

dtemporal <- function(x, shape, rate = 1, scale = 1/rate, pi, alpha = 0.001) {
  pi <- check_one_proba(pi)
  alpha <- check_one_proba(alpha)
  
  max_kappa <- get_max_kappa(pi, alpha)
  weights <- get_weights(pi, max_kappa)
  distributions <- convolve_gamma(shape, rate=rate, kappa=max_kappa, keep_all=TRUE)(x)
  
  out <- distributions %*% weights
  return(as.vector(out))
}






#' @export
#'
#' @rdname distributions
#' 
#' @param sd standard deviation of the Normal spatial kernel.

dspatial <- function(x, sd, pi, alpha = 0.001) {
  pi <- check_one_proba(pi)
  alpha <- check_one_proba(alpha)
  
  max_kappa <- get_max_kappa(pi, alpha)
  weights <- get_weights(pi, max_kappa)
  distributions <- convolve_spatial(sd=sd, kappa=max_kappa, keep_all=TRUE)(x)
  
  out <- distributions %*% weights
  return(as.vector(out))
}






#' @export
#'
#' @rdname distributions
#' 
#' @param gamma_shape,gamma_scale shape and scale of the gamma distribution used for the serial interval
#' @param gamma_rate an alternative way to specify the scale of the gamma distribution used for the serial interval
#' @param poisson_rate rate (i.e. mean) of the poisson distribution used for the per time unit genetic mutation rate

dgenetic <- function(x, gamma_shape, gamma_rate = 1, gamma_scale = 1/gamma_rate, poisson_rate, pi, alpha = 0.001) {
  pi <- check_one_proba(pi)
  alpha <- check_one_proba(alpha)
  
  max_kappa <- get_max_kappa(pi, alpha)
  weights <- get_weights(pi, max_kappa)
  distributions <- convolve_gamma_poisson(gamma_shape, gamma_rate=gamma_rate, gamma_scale=gamma_scale, poisson_rate=poisson_rate, kappa=max_kappa, keep_all=TRUE)(x)
  
  out <- distributions %*% weights
  return(as.vector(out))
}






#' @export
#'
#' @rdname distributions
#'
#' @param p A \code{numeric} vector providing the probability mass function, or
#'   empirical frequencies, of pairwise distances.

dempiric <- function(p, pi, alpha = 0.001) {
  pi <- check_one_proba(pi)
  alpha <- check_one_proba(alpha)
  p <- check_pmf(p)
  
  max_kappa <- get_max_kappa(pi, alpha)
  weights <- get_weights(pi, max_kappa)
  distributions <- convolve_empirical(p, max_kappa, TRUE)
  
  out <- distributions %*% weights
  return(as.vector(out))
}
