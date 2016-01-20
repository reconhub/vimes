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
#' \item{graph}{a graph connecting individuals, weighted with the input distances, with \code{igraph} class (from the \code{igraph} package.}
#' \item{clusters}{a list providing cluster information: group membership, cluster sizes, and the number of clusters (K)}
#' }
#'
#' @details
#' The cutoff is inclusive: only cases strictly further away than the cutoff distance will be disconnected on the graph. Note that this differs from the original \code{gengraph} implementation in the package \code{adegenet}.
#'
#' @seealso the function \code{gengraph} in the package \code{adegenet}, which was an initial implementation of the same idea in a genetics  context.
#'
vimes.graph <- function(x, cutoff=NULL, graph.opt=vimes.graph.opt(), ...){
    ## INTERACTIVE MODE FOR CHOOSING CUTOFF ##
    if(is.null(cutoff)){
        chooseAgain <- TRUE
        while (chooseAgain) {
            ## plot histogram ##
            hist(x, xlab="Pairwise distances", ylab="Frequency", main="Choose a cutoff distance",
                 border="white", col="#8585ad", ...)

            ## get input from user ##
            cat("\nEnter a cutoff distance:  ")
            cutoff <- NA
            while(is.null(cutoff) || is.na(cutoff)) suppressWarnings(cutoff <- as.numeric(readLines(n = 1)))

            ## add cutoff to the plot ##
            abline(v=cutoff,col="red",lty=2, lwd=2)

            ## get corresponding output ##
            out <- vimes.graph(x, cutoff=cutoff)

            ## show output ##
            cat(paste("\nNumber of clusters found:  ", out$clusters$K, sep=""))
            plot(out$graph)

            ## ask if cutoff should be kept ##
            ans <- ""
            while(!ans %in% c("y","n")){
                cat("\nAre you satisfied with this solution? (yes:y / no:n): ")
                ans <- tolower(readLines(n = 1))
            }
            if(ans=="y") chooseAgain <- FALSE
        }
        return(out)
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


    ## RETURN OUTPU ##
    out <- list(graph=g, clusters=groups, cutoff=cutoff)
} # end vimes.graph
