#' Color palettes used in vimes
#'
#' These functions are color palettes used in vimes.
#'
#' @author Thibaut Jombart \email{thibautjombart@@gmail.com}
#'
#' @param n a number of colors
#'
#' @export
#'
#' @examples
#'
#' plot(1:8, cex=8, pch=20, col=vimes.pal1(8), main="vimes.pal1")
#'
vimes.pal1 <- colorRampPalette(
    c("#ff6666", "#666699","#79d2a6",
      "#a4a4c1","#ffcc00","#85e085",
      "#df9fbf","#b2b266"))
