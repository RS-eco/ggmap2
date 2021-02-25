#' Add north arrow to plot
#'
#' Function to add a north arrow to plot
#' 
#' @param position position of arrow
#' @param size size of arrow
#' @param bearing direction of arrow
#' @return nice north arrow
#' @examples
#' \dontrun{
#' library(ggplot2)
#' data(meuse, package="sp")
#' plot(meuse$x, meuse$y)
#' ggplot(aes(x = x, y = y, color = zinc), data = meuse) + geom_point() + geom_northarrow(position=c(0.1,0.8),size=0.5)
#' }
#' @export
geom_northarrow <- function(position, bearing=0, size, colour, ...) { 
  #From Tanimura, Kuroiwa, Mizota 2007. J. Statistical Software. V 19 
  # checking arguments 
  if(missing(position)) stop("position is missing")
  if(missing(size)) stop("size is missing") 
  # default colors are white and black 
  if(missing(colour)) colour <- rep(c("white","black"),8) 
  # calculating coordinates of polygons 
  radii <- rep(size/c(1,4,2,4),4) 
  x <- radii[(0:15)+1]*cos((0:15)*pi/8+bearing)+position[1]
  y <- radii[(0:15)+1]*sin((0:15)*pi/8+bearing)+position[2] 
  # drawing polygons 
  for (i in 1:15) { 
    x1 <- c(x[i],x[i+1],position[1]) 
    y1 <- c(y[i],y[i+1],position[2]) 
    polygon(x1,y1,col=colour[i])
  } 
  # drawing the last polygon
  ggplot2::geom_polygon(c(x[16],x[1],position[1]),c(y[16],y[1],position[2]),colour=colour[16]) 
  # drawing letters 
  b <- c("E","N","W","S") 
  for (i in 0:3) text((size+par("cxy")[1])*cos(bearing+i*pi/2)+position[1], 
                      (size+par("cxy")[2])*sin(bearing+i*pi/2)+position[2],b[i+1],cex=1)
}
