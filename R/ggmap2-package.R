#' R package to easily create nice maps
#' 
#' @name ggmap2 package
#' @aliases ggmap2package
#' @docType package
#' @title ggmap2 - R package to easily create nice maps
#' @author RS-eco
#' @import grDevices gtable dplyr graphics ggplot2
#' @importFrom stats lm sd var rnorm
#' @importFrom methods as
#' @importFrom utils data find download.file read.csv
#' @keywords package
#'
NULL
#'
#' @docType data
#' @name continents
#' @title Outline of the world's continents
#' @description Outline of the world split into the different continents
#' @usage data(continents)
#' @details GADM outline of the world split into the different continents
#' @format A sf-data.frame with 8 observations.
NULL
#'
#' @docType data
#' @name largeislands
#' @title Outline of the world's islands with a size larger than 5 km2
#' @description GADM of the world's islands with a size larger than 5 km2
#' @usage data(largeislands)
#' @format A sf-data.frame with 8365 observations and 20 variables.
#' @details This shapefile contains the outline of the world's islands.
NULL
#'
#' @docType data
#' @name smallislands
#' @title Outline of the world's islands with a size smaller than 5 km2
#' @description GADM of the world's islands with a size smaller than 5 km2
#' @usage data(smallislands)
#' @format A sf-data.frame with 76773 observations and 20 variables.
#' @details This shapefile contains the outline of the world's islands.
NULL
#'
#' @docType data
#' @name outline
#' @title Outline of the world
#' @description Shapefile of the world's land outline
#' @usage data(outline)
#' @format \code{sfc_Multipolygon}.
#' @details This shapefile contains information about the world's land outline
NULL
