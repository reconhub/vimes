
## This file contains functions which are not exported. The API may
## change, so use ::: at your own risk.


## This function will get a set of graphical settings defined in a
## named list (typically returned by vimes_graph_opt) and use them to
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






## This simple function takes a vector 'x' and hads a tail of elements 'filling'
## so that it reaches a length 'L'.

fill_with <- function(x, filling, L = length(x)) {
  if (L <= length(x)) {
    return(x)
  }
  out <- rep(filling, L)
  out[seq_along(x)] <- x
  return(out)
}






## 'pmf' stands for probability mass function; basically we check that all
## values are positive, finite numbers and we standardise it so that it sums to
## one.

check_pmf <- function(x) {
  if (!is.numeric(x)) {
    stop("x must be numeric.")
  }

  if (any(!is.finite(x))) {
    stop("Non-finite values in x.")
  }

  if(any(x < 0)) {
    stop("x must be positive.")
  }

  x <- x / sum(x)

  return(x)
}






## This function checks that its argument is a single probability / proportion.

check_one_proba <- function(x) {
  if (!is.numeric(x)) {
    stop("x must be numeric")
  }

  if (length(x) != 1L) {
    stop("x must have a length of 1")
  }

  if (!is.finite(x)) {
    stop("non-finite values in x")
  }

  if(x < 0 || x > 1) {
    stop("x must be between 0 and 1")
  }

  return(x)
}
