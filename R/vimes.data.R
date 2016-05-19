#' Process input data for vimes
#'
#' This function processes input data for vimes, computing for each data a matrix of pairwise distances.
#' Data can have the following pre-defined types:
#' \itemize{
#' \item{dates}{dates associated to cases, typically dates of onset of symptoms}
#' \item{spatial}{matrix or data.frame with two columns for geographic coordinates}
#' \item{DNA sequences}{aligned DNA sequences of the pathogen}
#' }
#'
#' Data without a pre-defined type will be fed directly to the function \code{dist}.
#' Objects which are already pairwise distances with the class \code{dist} are left as is.
#'
#'
#' @author Thibaut Jombart \email{thibautjombart@@gmail.com}
#'
#' @seealso \code{\link{vimes.dist}} for further processing of the resulting pairwise distance
#' matrices; internally called by \code{vimes.data}
#'
#'
#' @param dates a vector of dates for each cases; typically dates of onset are used; the vector
#' should be named if elements do not match perfectly the other data types
#'
#' @param xy a 2-columns matrix or data.frame containing locations of the cases; if \code{lonlat} is
#' \code{TRUE}, coordinates are taken as longitudes and latitudes and the great circle distance will
#' be used; rows should be named if elements do not match perfectly the other data types
#'
#' @param dna a matrix with class \code{DNAbin} (from the package 'ape'); rows should be named if
#' elements do not match perfectly the other data types
#'
#' @param ... other data to be used foir computing pairwise distances using \code{dist}; entries
#' should be named if elements do not match perfectly the other data types; \code{dist} objects are
#' left as is.
#'
#' @export
#'
#' @return a list of named dist objects with matching entries, with attributes: 'labels' (labels of
#' the cases) and 'N' (number of cases); distances based on dates, xy coordinates, and genetic data
#' will be named 'temporal', 'spatial' and 'genetic', respectively
#'
#'
#' @examples
#' x <- vimes.data(dates=sim1$dates, xy=sim1$xy, dna=sim1$dna)
#' x
#' plot(x)
#'
vimes.data <- function(dates=NULL, xy=NULL, dna=NULL, lonlat=FALSE, ...){
    ## We process each type of data at a time, computing the appropriate distances whenever the type
    ## of data is fairly standard. Three types of data will be the most common and are therefore
    ## explicitely named and specifically handled: dates, geographic coordinates, and DNA
    ## sequences. Note that matching of entries across data types is handled at a later stage by
    ## vimes.dist, to which we pass a list of 'dist' objects (out).

    out <- list()


    ## PROCESS DATES ##

    ## dates are assumed to have the following classes: numeric, Date, POSIXct; we use
    ## 'dist' for numeric types, and difftime for the last two.
    if (!is.null(dates)) {
        if (!inherits(dates, what=c("numeric","Date","POSIXct"))) {
            warning("provided dates are not 'numeric', 'Date', or 'POSIXct'; using 'dist' to compute distances and hoping for the best")
        }
        out$temporal <- dist(dates)
    }


    ## PROCESS GEOGRAPHIC COORDINATES ##

    ## xy coordinates are 2-columns matrices or data.frames; they are assumed to be either simple
    ## numeric values, or longitudes (col 1) and latitudes(col 2)

    if (!is.null(xy)) {
        if (!is.numeric(xy)) {
            warning("provided xy coordinates is not 'numeric'; using 'dist' to compute distances")
            out$spatial <- dist(xy)
        } else {
            ## we use the great circle distances for lon / lat data
            if (lonlat) {
                out$spatial <- fields::rdist.earth(xy)
            } else {
                out$spatial <- dist(xy)
            }
        }
    }


    ## PROCESS DNA SEQUENCES ##

    ## dna sequences are stored is DNAbin matrices; see the ape package documentation for more info
    ## about this class; the distances between sequences will be the basic number of mutations
    ## between sequences; we always use pairwise deletions. This distance is a rough measure of
    ## genetic differentiation but permits to derive the distribution of 'genetic signatures'
    ## (expected nb of mutations between 2 linked cases) fairly easily; more work would be needed
    ## for fancier distances.

    if (!is.null(dna)) {
        if (!inherits(dna, what="DNAbin")) {
            stop("dna must be a DNAbin object (or remain NULL)")
        } else {
            out$genetic <- ape::dist.dna(dna, model="N", pairwise.deletion=TRUE)
        }
    }


    ## PROCESS OTHER DATA IN ... ##

    ## Whatever other data is passed to '...' is stored in a list 'othe' and each item of the list
    ## is fed to 'dist', unless it is already a 'dist' object.

    ## extract data from list ##
    other <- list(...)

    ## if first item is a list, use it as input
    if (length(other) > 0L) {
        if(is.list(other[[1]])) other <- other[[1]]

        ## add new distances to output
        other.names <- names(other)
        for (i in seq_along(other)) {
            if (!inherits(other[[i]], what="dist")) {
                out[[length(out)+1]] <- dist(other[[i]])
            } else {
                out[[length(out)+1]] <- other[[i]]
            }
            names(out)[length(out)] <- other.names[i]
        }
    }

    ## PROCESS PAIRWISE DISTANCES AND MATCH ENTRIES ##
    out <- vimes.dist(out)

    return(out)
}
