#' Color palettes used in vimes
#'
#' These functions are color palettes used in vimes.
#'
#' @author Thibaut Jombart \email{thibautjombart@@gmail.com}
#'
#' @param n a number of colors
#'
#' @export
#' @importFrom grDevices colorRampPalette
#'
#' @examples
#'
#' plot(1:8, cex=8, pch=20, col=vimes.pal1(8), main="vimes.pal1")
#' plot(1:4, cex=8, pch=20, col=vimes.pal1(4), main="vimes.pal1")
#' plot(1:100, col=vimes.pal1(100), pch=20, cex=6)
vimes.pal1 <- function(n){
    colors <- c("#ff6666", "#666699","#79d2a6",
                "#a4a4c1","#ffcc00","#85e085",
                "#df9fbf","#b2b266")
    if(n<8) {
        return(colors[seq_len(n)])
    } else {
        return(colorRampPalette(colors)(n))
    }
}
