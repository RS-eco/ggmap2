#' Clean theme for maps
#'
#' A clean theme that is good for displaying maps.
#'
#' @inheritParams ggplot2::theme_grey
#' @examples
#' library(ggplot2)
#' ggplot(faithfuld, aes(waiting, eruptions)) + geom_raster(aes(fill = density)) + 
#'        theme_map()
#' @export
theme_map <- function(base_size = 9, base_family = "") {
  requireNamespace("ggplot2")
  ggplot2::theme_bw(base_size = base_size, base_family = base_family) %+replace%
    ggplot2::theme(axis.line = ggplot2::element_blank(),
          axis.text = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          axis.title = ggplot2::element_blank(),
          panel.background = ggplot2::element_blank(),
          panel.border = ggplot2::element_blank(),
          panel.grid = ggplot2::element_blank(),
          panel.spacing = unit(0, "lines"),
          plot.background = ggplot2::element_blank())
}