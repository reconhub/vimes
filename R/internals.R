
## This file contains functions which are not exported. The API may
## change, so use ::: at your own risk.


## This function will get a set of graphical settings defined in a
## named list (typically returned by vimes.graph_opt) and use them to
## store new graphical options in a igraph object.

set_igraph_opt <- function(g, opt){
    ## check class
    if(!inherits(g, "igraph")) stop("g is not a igraph object")

    ## find clusters ##
    groups <- igraph::clusters(g)

    ## layout ##
    set.seed(opt$seed)
    g$layout <- opt$layout(g)

    ## vertices ##
    groups$color <- rep("lightgrey", groups$no) # groups of size 1 are grey
    groups$color[groups$csize>1] <- opt$col_pal(sum(groups$csize>1))
    igraph::V(g)$color <- groups$color[groups$membership]

    ## size
    igraph::V(g)$size <- opt$vertex_size

    ## font
    igraph::V(g)$label.family <- opt$label_family

    ## font color
    igraph::V(g)$label.color <- opt$label_color

    ##  edges ##
    ## labels
    if(length(igraph::E(g))>0 && opt$edge_label) {
        igraph::E(g)$label <- igraph::E(g)$weight
    }

    ## color
    igraph::E(g)$label.color <-  opt$label_color

    ## font
    ##    E(g)$label.family <- "sans" # bugs for some reason

    return(g)
}




## This has been written by Rich Fitzjohn. This function will modify a
## list with default values using a list of new values. Trying to add
## items not defined as default will trigger an error.

modify_defaults <- function(defaults, x){
    extra <- setdiff(names(x), names(defaults))
    if (length(extra) > 0L){
        stop("Additional invalid options: ", paste(extra, collapse=", "))
    }
    modifyList(defaults, x)
}
