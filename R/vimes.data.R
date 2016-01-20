#' Check input data
#'
#' @author Thibaut Jombart \email{thibautjombart@@gmail.com}
#'
#' @param ... a set of matrices or dist objects serving as input
#'
#' @export
#' @importFrom stats as.dist
#'
vimes.data <- function(...){
    ## PROCESS INPUT ##
    ## extract data from list ##
    data <- list(...)

    ## convert data to matrices ##
    data <- lapply(data, as.dist)

    ## add labels if needed ##
    for(i in seq_along(data)){
        if(is.null(labels(data[[i]]))) attr(data[[i]], "Labels") <- 1:attr(data[[i]], "Size")
    }

    ## MATCH ORDERING/FILL GAPS ##
    ## get list of all labels ##
    all.labels <- unique(unlist(lapply(data, attr, "Labels")))
    N <- length(all.labels)

    ## create model matrix ##
    out <- list()
    empty.mat <- matrix(0, ncol=N, nrow=N)
    rownames(empty.mat) <- colnames(empty.mat) <- all.labels

    ## fit data into model matrix
    for(i in seq_along(data)){
        temp <- as.matrix(data[[i]])
        temp.lab <- rownames(temp)
        out[[i]] <- empty.matrix
        out[[i]][temp.lab, temp.lab] <- temp
        out[[i]] <- as.dist(out[[i]])
    }

    ## RETURN OUTPUT ##
    attr(out, "labels") <- all.labels
    attr(out, "N") <- length(all.labels)

    return(out)
} # end vimes.data
