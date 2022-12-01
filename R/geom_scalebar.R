#' Add Scale Bar to ggplot Figure
#'
#' Function to add scale bar to ggplot
#' 
#' @param noBins
#' @param xname
#' @param yname
#' @param unit
#' @param position
#' @param sbLengthPct
#' @param sbHeightsvsWidth
#' @return ggplot object with scale bar
#' @examples
#' \dontrun{
#' library(ggplot2)
#' data(meuse, package="sp")
#' ggplot(data=meuse, aes(x = x, y = y, color = zinc)) + geom_point() + 
#' geom_scalebar(noBins = 3, position="bottomleft")
#' }
#' @export
geom_scalebar <- function(noBins = 5, xname = "x", yname = "y", unit = "m", position = "bottomright", 
                          sbLengthPct = 0.3, sbHeightvsWidth = 1/14){
  range_x = max(.data[[xname]]) - min(.data[[xname]])
  range_y = max(.data[[yname]]) -  min(.data[[yname]])
  lengthScalebar <- sbLengthPct*range_x
  
  # Rounding provided by code from Maarten Plieger
  makeNiceNumber <- function(num, num.pretty = 1) {
    return((round(num/10^(round(log10(num))-1))*(10^(round(log10(num))-1))))
  }
  widthBin = makeNiceNumber(lengthScalebar / noBins)
  heightBin = lengthScalebar * sbHeightvsWidth
  if(position == "bottomleft"){
    lowerLeftCornerScaleBar = c(x = min(.data[[xname]]), y = min(.data[[yname]]))
  } else if(position == "bottomright"){
    lowerLeftCornerScaleBar = c(x = max(.data[[xname]]), y = min(.data[[yname]]))
  }
  createBoxPolygon <- function(llcorner, width, height) {
    relativeCoords = data.frame(c(0, 0, width, width, 0), c(0, height, height, 0, 0))
    names(relativeCoords) = names(llcorner)
    return(t(apply(relativeCoords, 1, function(x) llcorner + x)))
  }
  scaleBarPolygon = do.call("rbind", lapply(0:(noBins - 1), function(n) {
    dum = data.frame(createBoxPolygon(lowerLeftCornerScaleBar + c((n * widthBin), 0), widthBin, heightBin))
    if(!(n + 1) %% 2 == 0) dum$cat = "odd" else dum$cat = "even"
    return(dum)
  }))
  #scaleBarPolygon[[attribute]] = min(.data[[attribute]])
  if(unit == "m"){
    textScaleBar = data.frame(x = lowerLeftCornerScaleBar[["x"]] + (c(0:(noBins)) * widthBin), y = lowerLeftCornerScaleBar[["y"]],
                              label = c(as.character(0:(noBins-1) * widthBin), paste(as.character(noBins * widthBin), "m", sep=" "))) 
    #textScaleBar[[attribute]] = min(.data[[attribute]])    
  } else if(unit == "km"){
    textScaleBar = data.frame(x = lowerLeftCornerScaleBar[["x"]] + (c(0:(noBins)) * widthBin), y = lowerLeftCornerScaleBar[["y"]],
                              label = c(as.character(0:(noBins-1) * widthBin / 1000), paste(as.character(noBins * widthBin / 1000), "km", sep=" "))) 
    #textScaleBar[[attribute]] = min(.data[[attribute]])
  }
  return(ggplot2::geom_polygon(data = subset(scaleBarPolygon, cat == "odd"), aes(x=x, y=y), fill = "black", color = "black") +
           ggplot2::geom_polygon(data = subset(scaleBarPolygon, cat == "even"), aes(x=x, y=y), fill = "white", color = "black") +
           ggplot2::geom_text(data = textScaleBar, aes(x=x, y=y, label = label), color = "black", size = 6, hjust = 0.5, vjust = 1.2))
}
