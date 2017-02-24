#' VIsualisation and Monitoring of EpidemicS
#'
#' The function \code{vimes} is used to identify clusters of related cases based
#' on multiple data.
#'
#' !!! This package is still under development. Do not use it without contacting
#' the author. !!!
#'
#'
#' @author Thibaut Jombart \email{thibautjombart@@gmail.com}
#'
#' @export
#' 
#' @param x a list of the class 'vimes_data' as returned by
#' \code{vimes_data}.
#' 
#' @param cutoff a vector with the same length as 'x' indicating cutoff
#' distances beyond which individuals will not be connected in the separate
#' graphs; recycled if needed. If NULL, interactive mode will be triggered to
#' ask the user for cutoff distances.
#' 
#' @param graph_opt a list of graphical options for the graphs, as
#' returned by \code{\link{vimes_graph_opt}}.
#' 
#' @param method a character string indicating the pruning method to be used;
#' see details.
#' 
#' @param log_dens a list of log-density functions to be used for ML
#' estimation; one function is needed for each type of data.
#' 
#' @param ... further arguments to be passed to \code{hist}.
#'
#' @seealso
#' \describe{
#'
#' \item{\code{\link{vimes_data}}}{to prepare the input data.}
#' 
#' \item{\code{\link{vimes_prune}}}{for getting individual pruned graphs.}
#'
#' }
#'
#' @details
#' Different methods can be used for graph pruning:
#' \describe{
#' 
#'  \item{basic}{pre-defined cutoffs are used if provided as \code{cutoff}; if
#' missing, they are chosen interactively by the user by examining the
#' distribution of distances}
#'
#' }
#' 
#' @examples
#'
#'  ## generate data
#'  set.seed(2)
#'  dat1 <- rnorm(30, c(0,1,6))
#'  dat2 <- rnorm(30, c(0,0,1))
#'  dat3 <- rnorm(30, c(8,1,2))
#'  x <- lapply(list(dat1, dat2, dat3), dist)
#'  x <- vimes_data(x)
#'  plot(x)
#'
#'  ## analyse data
#'  res <- vimes(x, cutoff=c(2,4,2))
#'  res
#'  plot(res$graph)
#'

## This is the main function of the package. It implements the
## following workflow:
##
## 1) For each distance matrice provided, find cutoff values past which cases
## are disconnected.
##
## 2) For each distance matrice provided, create pruned graphs
##
## 3) Build a consensus graph by intersection of all the pruned graphs
##
## Note that output graphs normally contain their own grapĥical options and
## customisations.
## 

vimes <- function(x, method = c("basic"),
                  cutoff = NULL,
                  log_dens = NULL,
                  graph_opt = vimes_graph_opt(), ...){
    ## CHECKS ##
    ## basic checks
    if(is.null(x)) stop("x is NULL")
    if(!is.list(x)) stop("x is not a list")
    if(!inherits(x, "vimes_data")) stop("x is not a vimes_data object")
    K <- length(x)
    if(!is.null(cutoff)) cutoff <- rep(cutoff, length=K)
    x.labels <- names(x)

    ## check method used
    method <- match.arg(method)
    

    ## MAKE SEPARATE GRAPHS ##
    all_graphs <- list()
    for (i in seq_along(x)) {
        ## call the prune method
        if (method=="basic") {
            all_graphs[[i]] <- vimes_prune(x[[i]],
                                           cutoff = cutoff[i],
                                           graph_opt = graph_opt, ...)
        }
    }


    ## GET MAIN GRAPH ##
    ## intersect graphs ##
    g <- do.call(igraph::intersection,
                 lapply(all_graphs, function(e) e$graph))

    ## set graphical options ##
    ## disable weights
    graph_opt$edge.label <- FALSE
    g <- set_igraph_opt(g, graph_opt)

   ## find clusters ##
    groups <- igraph::clusters(g)
    names(groups) <- c("membership", "size", "K")

    ## add cluster colors ##
    groups$color <- graph_opt$col_pal(groups$K)
    names(groups$color) <- 1:groups$K

    ## ADJUST SEPARATE GRAPHS FEATURES ##
    for(i in seq_along(x)){
        ## vertex color ##
        igraph::V(all_graphs[[i]]$graph)$color <- igraph::V(g)$color

        ## layout ##
        all_graphs[[i]]$graph$layout <- g$layout
    }
    names(all_graphs) <- x.labels
    

    ## The output will contain the main graph, cluster definitions,
    ## the cutoff values used, and then similar information for each
    ## individual graph (one per original distance matrix).
    
    out <- list(graph = g,
                clusters = groups,
                cutoff = cutoff,
                separate_graphs = all_graphs)

    return(out)
}
