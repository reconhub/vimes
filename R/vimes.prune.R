#' Builds a graph from pairwise distances
#'
#' This function builds a graph using a \code{dist} object and a cut-off distance beyond which individuals are no longer connected. It also identifies clusters of connected individuals from the graph. If no cut-off is provided, a histogram is displayed for interactive selection.
#'
#'
#' @author Thibaut Jombart \email{thibautjombart@@gmail.com}
#'
#' @export
#'
#' @inheritParams vimes
#' @param x pairwise distances stored as a \code{dist} object.
#' @param cutoff a cutoff distance beyond which individuals will not be connected in the graph.
#' @param ... further arguments to be passed to \code{hist}.
#'
#' @return a list containing:
#' \describe{
#' 
#' \item{graph}{a graph connecting individuals, weighted with the
#' input distances, with \code{igraph} class (from the \code{igraph}
#' package).}
#' 
#' \item{clusters}{a list providing cluster information: group
#' membership, cluster sizes, and the number of clusters (K)}
#' 
#' }
#'
#' @details
#'
#' The cutoff is inclusive: only cases strictly further away
#' than the cutoff distance will be disconnected on the graph. Note
#' that this differs from the original \code{gengraph} implementation
#' in the package \code{adegenet}.
#'
#' @seealso
#'
#' the function \code{gengraph} in the package \code{adegenet}, which
#' was an initial implementation of the same idea in a genetics
#' context.
#'

## The purpose of this function is to prune a fully connected graph
## using a given cutoff distance value. If 'cutoff' is not provided,
## we used by default interactive cutoff selection.

vimes_prune <- function(x, cutoff = NULL,
                        graph_opt = vimes_graph_opt(), ...){
    ## CHECKS ##
    if (is.null(x)) {
        stop("input data is NULL")
    }

    ## INTERACTIVE MODE FOR CHOOSING CUTOFF ##
    if (is.null(cutoff)) {
        return(cutoff.choice.interactive(x = x,
                                         graph_opt = graph_opt))
    }


    ## BUILD GRAPH ##

    ## In the following we create a pruned graph, derive corresponding
    ## clusters, create new graphical attributes for the graph (mostly
    ## coloring clusters).
    
    x[x>cutoff] <- 0
    g <- igraph::graph.adjacency(as.matrix(x), mode = "undirected",
                                 weighted = TRUE, diag = FALSE)

    ## find clusters ##
    groups <- igraph::clusters(g)
    names(groups) <- c("membership", "size", "K")

    ## add cluster colors
    groups$color <- graph_opt$col_pal(groups$K)
    names(groups$color) <- 1:groups$K

    ## Here we add new graphical properties to the graph that will
    ## ultimately be returned.
    
    g <- set_igraph_opt(g, graph_opt)

    ## The returned output should be a self-sufficient list containing
    ## the pruned graph, cluster definition, and cutoff values used.
    
    out <- list(graph = g, clusters = groups, cutoff = cutoff)
} 
