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
#' @param cutoff.ini a set of initial values for cutoff distances beyond which individuals will not be connected in the graph.
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
vimes.prune.ml <- function(x, cutoff.ini=NULL){
    ## CHECKS ##
    if(is.null(x)){
        stop("input data is NULL")
    }

    ## INTERACTIVE MODE FOR CHOOSING CUTOFF ##
    if(is.null(cutoff)){
        return(cutoff.choice(x=x, graph.opt=graph.opt))
    }


    ## GET GRAPH ##
    ## build graph ##
    x[x>cutoff] <- 0
    g <- graph.adjacency(as.matrix(x), mode="undirected", weighted=TRUE, diag=FALSE)

    ## find clusters ##
    groups <- clusters(g)
    names(groups) <- c("membership", "size", "K")

    ## add cluster colors
    groups$color <- graph.opt$col.pal(groups$K)
    names(groups$color) <- 1:groups$K

    ## setup graphical options for graph ##
    g <- set.graph.opt(g, graph.opt)


    ## RETURN OUTPUT ##
    out <- list(graph=g, clusters=groups, cutoff=cutoff)
} # end vimes.prune





## Function to find ML cutoff estimate
##
## [non-exported function]
## Input:
## d: dist object,
## f: log-density function
## cutoff.ini: initial cutoff value
## Output: ML cutoff estimate
##
cutoff.ml <- function(d,f, cutoff.ini){
    ## define log-like function
    LL <- function(cutoff){
        sum(f(d[d<=cutoff]))
    }

    ## use optimize
    out <- optimise(LL, range(d), maximum=TRUE)$maximum

    ## return output
    return(out)
} # end cutoff.ml
