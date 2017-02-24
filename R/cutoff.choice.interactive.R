#' Interactive choice of cutoff points.
#'
#' This function displays histograms of pairwise distances and asks the user for a cut-off point to choose. It will prune each corresponding graph for any new value requested.
#'
#' @author Thibaut Jombart \email{thibautjombart@@gmail.com}
#'
#' @inheritParams vimes 
#' @param ... further arguments passed to 'hist'
#' 
cutoff_choice_interactive <- function(x, graph_opt = vimes_graph_opt(), ...){
      chooseAgain <- TRUE
        while (chooseAgain) {
            ## plot histogram ##
            
            graphics::hist(x, xlab="Pairwise distances", ylab="Frequency",
                           main="Choose a cutoff distance",
                           border="white", col="#8585ad", ...)

            ## get input from user ##
            cat("\nEnter a cutoff distance:  ")
            cutoff <- NA
            while(is.null(cutoff) || is.na(cutoff)) {
                suppressWarnings(cutoff <- as.numeric(readLines(n = 1)))
            }

            ## add cutoff to the plot ##
            graphics::abline(v = cutoff, col = "red", lty = 2, lwd = 2)

            ## get corresponding output ##
            out <- vimes_prune(x, cutoff = cutoff, graph_opt = graph_opt)

            ## show output ##
            cat(paste("\nNumber of clusters found:  ", out$clusters$K, sep=""))
            graphics::plot(out$graph)

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
