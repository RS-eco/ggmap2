#' Calculate distance to coast
#'
#' @description Create a raster layer with the distance to coast for the sea or 
#' land (if inverse=TRUE).
#' 
#' @usage 
#' distanceToCoast(ncol, nrow, extent='', inverse=FALSE, ...)
#' 
#' @param ncol Number of columns used for creating distanceToCoast raster
#' @param nrow Number of rows used for creating distanceToCoast raster
#' @param extent Extent used for creating distanceToCoast raster
#' @param inverse Logical. If TRUE the distance to land will be calculated
#' @param ... Additional required (!) parameters. 
#' These are data set specific. See Details
#'
#' @details distanceToCoast internally loads the outline data, 
#' which is also included in this package. It then creates a raster of the outline shapefile 
#' using the rasterize function according to the specified resolution and extent provided 
#' and then calculates the distance using the distance function from the raster package.
#'  
#' @return A Raster* layer
#'  
#' @examples
#' distSea <- distanceToCoast(ncol=36, nrow=18)
#' distLand <- distanceToCoast(ncol=36, nrow=18, inverse=TRUE)
#' 
#' @references 
#' \url{http://www.gadm.org}
#' 
#' @keywords spatial
#' 
#' @export
distanceToCoast <- function(ncol=36, nrow=18, extent='', inverse=FALSE, ...){
  # Create raster
  r <- raster::raster(ncol=ncol,nrow=nrow, vals=1)

  # Save file
  #raster::writeRaster(r, "data/dist_land.tif", format="GTiff")

  # Rasterize country outline
  r_land <- raster::rasterize(as(sf::st_as_sf(ggmap2::outline), "Spatial"), r)
  #r_land <- gdalUtils::gdal_rasterize(b=1, burn=1, l="outline", src_datasource="outline.shp",
  #               dst_filename="data/dist_land.tif", output_Raster=TRUE)

  # Turn ocean to land if inverse = T
  if(inverse==TRUE){r_sea <- raster::mask(r, r_land, inverse=TRUE)}
  
  # Calculate distance
  raster::distance(r_land)
}  
