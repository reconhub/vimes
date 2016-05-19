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
#' @return a list of dist objects with matching entries, with attributes: 'labels' (labels of the cases) and 'N' (number of cases)
#'
#' @examples
#' x1 <- c(0,1,3)
#' x2 <- c(2,5)
#' names(x1) <- letters[1:3]
#' names(x2) <- c('a', 'r')
#' D1 <- dist(x1)
#' D2 <- dist(x2)
#' out <- vimes.data(D1, D2)
#' out
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
        if (is.numeric(dates)) {
            out$dates <- dist(dates)
        } else if (inherits(dates, what="Date") ||
                   inherits(dates, what="POSIXct")) {
            out$dates <- difftime(dates)
        } else {
            warning("provided dates are not 'numeric', 'Date', or 'POSIXct'; using 'dist' to compute distances")
            out$dates <- dist(dates)
        }
    }


    ## PROCESS GEOGRAPHIC COORDINATES ##
    if (!is.null(xy)) {
        if (!is.numeric(xy)) {
            warning("provided xy coordinates is not 'numeric'; using 'dist' to compute distances")
            out$xy <- dist(xy)
        } else {
            ## we use the great circle distances for lon / lat data
            if (lonlat) {
                out$xy <- fields::rdist.earth(xy)
            } else {
                out$xy <- dist(xy)
            }
        }
    }

    ## xy coordinates are 2-columns matrices or data.frames; they are assumed to be either simple
    ## numeric values, or longitudes (col 1) and latitudes(col 2)



    ## extract data from list ##
    data <- list(...)
    data.names <- names(data)
    if(length(data)==0L) stop("no data to process")

    ## escape if data has been processed already
    if(inherits(data[[1]],"vimes.input")) return(data[[1]])

    ## if first item is a list, use it as input
    if(is.list(data[[1]])) data <- data[[1]]


    ## ENSURE MATRICES AND LABELLING ##
    ## convert all data to matrices
    data <- lapply(data, as.matrix)
    K <- length(data)

    ## assign labels if missing
    for(i in seq_along(data)){
        if(is.null(rownames(data[[i]]))) {
            rownames(data[[i]]) <- colnames(data[[i]]) <- 1:nrow(data[[i]])
        }
    }


    ## HANDLE NAS AND SORTING ##

    ## The policy will be to remove cases with NAs, as cases with NA
    ## distances tend to link all other cases. This may change in
    ## later version of the method.

    ## get labels to keep
    ## (i.e. present without NA everywhere)
    lab.to.keep <- Reduce(intersect,
                          lapply(data, function(e) rownames(e)[!apply(is.na(e),1,all)])
                          )
    N <- length(lab.to.keep)
    if(N<2){
        warning("Data contain less than 2 cases without missing data.")
        return(NULL)
    }

    ## remove NAs, order, store result
    out <- vector(K, mode="list")
    for(i in seq_along(data)){
        out[[i]] <- stats::as.dist(data[[i]][lab.to.keep, lab.to.keep])
    }


    ## RETURN OUTPUT ##

    ## Output will be a list of 'dist' objects; labels and the number
    ## of cases will be stored as attributes; the output will be a
    ## list with the additional class vimes.input
    names(out) <- data.names
    attr(out, "labels") <- lab.to.keep
    attr(out, "N") <- N

    class(out) <- c("list", "vimes.input")
    return(out)
}
