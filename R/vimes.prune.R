#' Builds a graph from pairwise distances
#'
#' This function builds a graph using a \code{dist} object and a cut-off distance beyond which individuals are no longer connected. It also identifies clusters of connected individuals from the graph. If no cut-off is provided, a histogram is displayed for interactive selection.
#'
#'
#' @author Thibaut Jombart \email{thibautjombart@@gmail.com}
#'
#' @export
#'
#' @importFrom igraph "graph.adjacency" "clusters" "V" "V<-" "layout_nicely" "plot.igraph"
#' @importFrom graphics "abline" "plot"
#'
#' @param x pairwise distances stored as a \code{dist} object.
#' @param cutoff a cutoff distance beyond which individuals will not be connected in the graph.
#' @param graph.opt a list of graphical options for the graphs, as returned by \code{\link{vimes.graph.opt}}.
#' @param ... further arguments to be passed to \code{hist}.
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
vimes.prune <- function(x, cutoff=NULL, graph.opt=vimes.graph.opt(), ...){
    ## CHECKS ##
    if(is.null(x)){
        stop("input data is NULL")
    }

    ## INTERACTIVE MODE FOR CHOOSING CUTOFF ##
    if(is.null(cutoff)){
        return(cutoff.choice.interactive(x=x, graph.opt=graph.opt))
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
    g <- set.igraph.opt(g, graph.opt)


    ## RETURN OUTPUT ##
    out <- list(graph=g, clusters=groups, cutoff=cutoff)
} # end vimes.prune





