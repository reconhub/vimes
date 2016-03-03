#' Process input data for vimes
#'
#' This function takes a series of 'dist' objects (or objects which can be converted to 'dist', and ensures that entries match in all objects, adding NAs where necessary. The total number of cases and labels are returned as attributes.
#'
#' @author Thibaut Jombart \email{thibautjombart@@gmail.com}
#'
#' @param ... a series (alternatively, a list) of matrices or dist objects serving as input.
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
    ## PROCESS TYPES OF INPUT  ##
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
        out[[i]] <- as.dist(data[[i]][lab.to.keep, lab.to.keep])
    }

    
    ## RETURN OUTPUT ##
    names(out) <- data.names
    attr(out, "labels") <- lab.to.keep
    attr(out, "N") <- N

    class(out) <- c("list", "vimes.input")
    return(out)
} # end vimes.data
