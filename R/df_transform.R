#' Transform projection of data.frame
#'
#' @description Change the projection of x and y values (spatial coordinates) 
#'              of a data.frame, make sure your data only includes x,y and z 
#'              and no additional categorical variables
#' 
#' @usage 
#' df_transform(data, coords, crs.in, crs.out)
#' 
#' @param data data.frame object
#' @param coords vector of the two coordinate column names
#' @param crs.in original projection of data.frame, standard is WGS1984.
#' @param crs.out output projection of data.frame, which is returned.
#' @details df_transform internally converts the data.frame into a spatial object, 
#' than transforms the projection of the data and 
#' converts the final spatial object back into a data.frme.
#' @return A data.frame
#'  
#' @examples
#' data(meuse, package="sp")
#' meuse <- df_transform(meuse, 
#'                       crs.in=sp::CRS("+init=epsg:28992"), 
#'                       crs.out=sp::CRS("+init=epsg:4326"))
#'
#' @export
df_transform <- function(data, coords=c("x","y"), 
                         crs.in=sp::CRS("+init=epsg:4326"), 
                         crs.out=NA){
  if(class(data) == "data.frame"){
    data <- raster::rasterFromXYZ(data)
    raster::projection(data) <- crs.in
    data <- raster::projectRaster(data, crs=crs.out)
    data <- as.data.frame(raster::rasterToPoints(data))
  } else{
    warning("Data has to be of class data.frame.")
  }
  return(data)
}