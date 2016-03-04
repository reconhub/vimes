#' Builds a graph from pairwise distances, optimizing cutoff using ML
#'
#' This function builds a graph using a \code{dist} object and a cut-off distance beyond which individuals are no longer connected.
#' The cutoff distance is chosen using Maximum-Likelihood estimation. The likelihood function has to be provided.
#'
#'
#' @author Thibaut Jombart \email{thibautjombart@@gmail.com}
#'
#' @export
#'
#' @param x pairwise distances stored as a \code{dist} object.
#' @param f a function giving the log-likelihood of a given distance between two epidemiologically conected cases.
#' @param graph.opt a list of graphical options for the graphs, as returned by \code{\link{vimes.graph.opt}}.
#'
#' @return a list containing:
#' \describe{
#' \item{graph}{a graph connecting individuals, weighted with the input distances, with \code{igraph} class (from the \code{igraph} package).}
#' \item{clusters}{a list providing cluster information: group membership, cluster sizes, and the number of clusters (K)}
#' }
#'
#' @details
#' The cutoff is inclusive: only cases strictly further away than the cutoff distance will be disconnected on the graph. Note that this differs from the original \code{gengraph} implementation in the package \code{adegenet}.
#'
#' @seealso the function \code{gengraph} in the package \code{adegenet}, which was an initial implementation of the same idea in a genetics  context.
#'
vimes.prune.ml <- function(x, f, graph.opt=vimes.graph.opt()){
    ## CHECKS ##
    if(is.null(x)){
        stop("input data is NULL")
    }

    ## FIND THRESHOLD
    cutoff <- cutoff.ml(x, f)

    ## GET GRAPH ##
    out <- vimes.prune(x, cutoff=cutoff, graph.opt=graph.opt)
    return(out)
} # end vimes.prune.ml





## Function to find ML cutoff estimate
##
## [non-exported function]
## Input:
## d: dist object,
## f: log-density function
## cutoff.ini: initial cutoff value
## Output: ML cutoff estimate
##
cutoff.ml <- function(d,f){
    ## define log-like function
    LL <- function(cutoff){
        mean(f(d[d<=cutoff]))
    }

    ## use optimize
    out <- optimise(LL, range(d), maximum=TRUE)$maximum

    ## return output
    return(out)
} # end cutoff.ml
