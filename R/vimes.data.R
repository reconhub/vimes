#' Process input data for vimes
#'
#' This function takes a series of 'dist' objects (or objects which can be converted to 'dist', and ensures that entries match in all objects, adding NAs where necessary. The total number of cases and labels are returned as attributes.
#'
#' @author Thibaut Jombart \email{thibautjombart@@gmail.com}
#'
#' @param ... a set of matrices or dist objects serving as input
#'
#' @export
#' @importFrom stats as.dist
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
vimes.data <- function(...){
    ## PROCESS INPUT ##
    ## extract data from list ##
    data <- list(...)
    data.names <- names(data)

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
    empty.mat <- matrix(NA_real_, ncol=N, nrow=N)
    rownames(empty.mat) <- colnames(empty.mat) <- all.labels

    ## fit data into model matrix
    for(i in seq_along(data)){
        temp <- as.matrix(data[[i]])
        temp.lab <- rownames(temp)
        out[[i]] <- empty.mat
        out[[i]][temp.lab, temp.lab] <- temp
        out[[i]] <- as.dist(out[[i]])
    }

    ## RETURN OUTPUT ##
    names(out) <- data.names
    attr(out, "labels") <- all.labels
    attr(out, "N") <- length(all.labels)

    class(out) <- c("list", "vimes.input")
    return(out)
} # end vimes.data
