#' Check input data
#'
#' @author Thibaut Jombart \email{thibautjombart@@gmail.com}
#'
#' @param ... a set of matrices or dist objects serving as input
#'
#' @export
#'
vimes.data <- function(...){
    ## EXTRACT DATA FROM LIST ##
    data <- list(...)

    ## CONVERT DATA TO MATRICES ##
    data <- lapply(data, as.dist)

    ## ADD LABELS IF NEEDED ##
    for(i in seq_along(data)){
        if(is.null(labels(data[[i]]))) attr(data[[i]], "Labels") <- 1:attr(data[[i]], "Size")
    }

    ## MATCH MATRICES ##
    ## get list of all labels
    all.labels <- unique(unlist(lapply(data, attr, "Labels")))

    ## reorder dist objects
    out <- list()
    for(i in seq_along(data)){
        out[[i]] <- as.dist(as.matrix(data[[i]])[all.labels, all.labels])
    }


    ## RETURN OUTPUT ##
    attr(out, "labels") <- all.labels
    attr(out, "N") <- length(all.labels)

    return(out)
} # end vimes.data
