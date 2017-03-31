#' Expected distributions of distances (functions)
#'
#' The function \code{fpaircase} returns functions which compute the expected
#' distributions of distances between pairs of cases given a case reporting
#' probability 'pi'. See \code{\link{dpaircase}} for details on different types
#' of distances between cases for which distributions can be computed.
#'
#'
#' @author Thibaut Jombart \email{thibautjombart@@gmail.com}.
#'
#' @export
#'
#' @inheritParams dpaircase
#'
#' @examples

fpaircase <- function(type = c("temporal","genetic","spatial", "empiric"),
                      gamma_shape, gamma_rate = 1, gamma_scale = 1/gamma_rate,
                      poisson_rate,
                      sd_spatial,
                      p,
                      alpha = 0.001) {

  type <- match.arg(type)

  out <- function(x, pi = 1) {
    dpaircase(x, pi = pi, type = type,
              gamma_shape = gamma_shape,
              gamma_rate = gamma_rate,
              gamma_scale = gamma_scale,
              poisson_rate = poisson_rate,
              sd_spatial = sd_spatial,
              p = p,
              alpha = alpha)
  }

  class(out) <- c("fpaircase", "function")
  attr(out, "type") <- type
  attr(out, "continuous") <- type %in% c("temporal", "spatial")
  attr(out, "call") <- match.call()

  return(out)
}






#' @export
#'
#' @rdname fpaircase
#'
#' @param ... further arguments passed to methods (print, plot, etc.)
#'
print.fpaircase <- function(x, ...) {
  cat("/// distribution of distances")

  cat(sprintf("\n  class: %s",
              paste(class(x), collapse = ", ")))

  cat(sprintf("\n  type: %s (%s) distribution",
              attr(x, "type"),
              ifelse(attr(x, "continuous"), "continuous", "discrete")))

  cat("\n  original call:", deparse(attr(x, "call")))
  cat("\n")
}






#' @export
#'
#' @rdname fpaircase
#'
plot.fpaircase <- function(x, y = NULL, xlim = c(0, 10), ...) {
  continuous <- attr(x, "continuous")

  titles <- c("Delays",
              "Dispersal distance",
              "Genetic signature",
              "Empirical")
  titles <- paste(titles, "distribution")

  xlabs <- c("Time between cases",
             "Geographic distance",
             "Number mutations",
             "Distance between cases")

  names(titles) <- c("temporal", "spatial", "genetic", "empirical")
  names(xlabs) <- names(titles)

  type <- attr(x, "type")

  if (continuous) {
    plot.function(x, xlim = xlim,
                  main = titles[type],
                  xlab = xlabs[type], ...)
  } else {
    x_val <- as.integer(seq(from = min(xlim), to = max(xlim), by = 1L))
    plot(x_val, x(x_val), type = "h", lend = 1, lwd = 10,
         main = titles[type],
         xlab = xlabs[type], ...)
  }
}
