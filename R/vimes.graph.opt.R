#' Set up graphical options for graphs
#'
#' This function sets up graphical options for \code{igraph} objects.
#' Existing options include:
#' \describe{
#' 
#' \item{col_pal}{a color palette used for the groups; defaults to vimes.pal1}
#' 
#' \item{layout}{a layout function used for plotting the graph; see
#' \code{?layout_nicely} for more information.}
#' 
#' \item{seed}{a random seed to be used for plotting the graph}
#' 
#' \item{vertex_size}{the size of the vertices; defaults to 10}
#' 
#' \item{label_family}{a font family for labels; defaults to "sans"}
#' 
#' \item{label_color}{the color of the labels; defaults to "black"}
#' 
#' \item{edge_label}{a logical indicating if weights should be used to annotate
#' the edges; defaults to FALSE}
#' 
#'}
#'
#' @param ... a list of named graphical options; see Description.
#'
#' @author Thibaut Jombart \email{thibautjombart@@gmail.com}
#'
#' @export
#' @importFrom utils modifyList
#' @importFrom igraph "E" "E<-"
#'
#' @examples
#' vimes_graph_opt()
#'
vimes_graph_opt <- function(...){
    
    ## The purpose of this function is to handle all graphical options
    ## for plotting in vimes. If there is a need for storing different
    ## options for different types of plots, and a risk for ambiguous
    ## names, options should be named as [type of
    ## plot].[option]. Default values should be provided for all
    ## options. Note that unknown options should issue an error (they
    ## will through modify.defaults).

    config <- list(...)

    ## SET DEFAULTS ##
    defaults <- list(col_pal = vimes_pal2,
                     layout = igraph::layout_nicely,
                     seed = 1,
                     vertex_size = 10,
                     label_family = "sans",
                     label_color = "black",
                     edge_label = FALSE)

    ## MODIFY CONFIG WITH ARGUMENTS ##
    config <- modify_defaults(defaults, config)

    return(config)
}




