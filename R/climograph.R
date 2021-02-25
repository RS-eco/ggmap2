#' Create Climograph
#'
#' @description Function to plot climograph of a given location in the world.
#'
#' @param name character. Name of dataset, default is worldclim.
#' @param res numeric. One of 0.5, 1, 5, 10. Resolution of worldclim data to use, default is 10.
#' @param lon Longitude of location you want to create climograph for.
#' @param lat Latitude of location you want to create climograph for.
#' @param path Character. Path name indicating where to store the data. Default is the current working directory.
#' @param download Logical. If TRUE data will be downloaded if not locally available
#' @param style character. normal, plotly or ggplot. Defaults to normal.
#' @param save True/False. If Plot should be saved to file.
#' @param filename the name of the output file.
#' @param height the height of the device in inches.
#' @param width the width of the device in inches.
#' @param bg the initial background colour, default is transparent.
#' @return Returns a climograph plot  with coordinates (x,y) and corresponding raster values.
#' @name climograph
#' @examples
#' \dontrun{
#' climograph(lon=39.5, lat=40.5)
#' }
#' @export
climograph <- function(name="worldclim", res=10, lon=39.5, lat=40.5, 
                       path='', download=TRUE, style="normal", save=FALSE,
                       filename="Rplots.pdf", height=7, width=7, bg="transparent"){
  requireNamespace("dplyr")
  
  prec <- raster::getData(name=name, var="prec",lon=lon, lat=lat, res=res, path=path, download=download)
  tmean <- raster::getData(name=name, var="tmean", lon=lon, lat=lat, res=res, path=path, download=download)
    
  # Mask and crop precipitation data by location
  location <- data.frame(cbind(lon, lat))
  sp::coordinates(location) <- ~lon+lat
  raster::projection(location) <- sp::CRS("+init=epsg:4326")
  prec <- raster::extract(prec, location)
  tmean <- raster::extract(tmean, location)
  
  # Standardise temperature to degree Celsius
  tmean <- tmean/10
  
  # Combine precipitation & temperature data
  clim_sp_avg <- data.frame(cbind(t(data.frame(prec)), t(data.frame(tmean))))
  colnames(clim_sp_avg) <- c("prec", "tmean")
  
  # Transform temperature values
  clim_sp_avg$prec <- clim_sp_avg$prec + (2*min(clim_sp_avg$tmean))
  
  # Plot average precipitation and mean temperature data
  if(style == "normal"){
    if(save == TRUE){
      grDevices::cairo_pdf(filename=filename, height=height, width=width, bg=bg)
    }
    graphics::par(mar = c(4.5,4.5,1.5,4.5)+.1, bg="white", cex.lab=1.5, cex.axis=1.2)
    graphics::barplot(clim_sp_avg$prec, space=0.5, 
            ylim=c(min(floor(clim_sp_avg$tmean/10)*20),max(ceiling(clim_sp_avg$prec/20)*20)), 
            pch=19, axes=FALSE, 
            col="blue", xlab="Month of the Year", ylab="")
    graphics::axis(side=4, at=seq(0, max(ceiling(clim_sp_avg$prec/20)*20), by=20), col.axis="blue", las=1)
    graphics::axis(side=1, at=c(1,2.5,4,5.5,7,8.5,10,11.5,13,14.5,16,17.5), 
         labels=c("J","F","M","A","M","J","J","A","S","O","N","D"))
    min(floor(clim_sp_avg$tmean/10)*20)
    lines((clim_sp_avg$tmean*2)~c(1,2.5,4,5.5,7,8.5, 10,11.5,13,14.5,16,17.5), col="red", lwd=2.5)
    graphics::axis(side=2, at=seq(min(floor(clim_sp_avg$tmean/10)*20), max(ceiling(clim_sp_avg$tmean/10)*20), by=20), 
         labels=seq(min(floor(clim_sp_avg$tmean/10)*10), max(ceiling(clim_sp_avg$tmean/10)*10), by=10), 
         pos= -0.2, col.axis="red", las=1)
    mtext(side = 4, line=3, "Precipitation (mm)", col="blue", cex=1.5)
    #mtext(side = 2, line=3, expression(paste("Temperature (",degree,"C)")), cex=1.5, col="red")
    mtext(side = 3, line=-2, paste("degree C:", round(mean(clim_sp_avg$tmean),1), "   ", "mm:", round(sum(clim_sp_avg$prec),0), sep=" "))
    abline(h = 0, v = NA, col = "black")
    box()
    if(save==TRUE){
      dev.off()
    }
  } else if(style == "plotly"){
    clim_sp_avg$month <- c(1:12)
    clim_sp_avg$month <- factor(clim_sp_avg$month, labels = c("Jan","Feb","Mar","Apr","May", 
                                                              "Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
    plotly::plot_ly(data=clim_sp_avg, x = ~month, alpha=0.5) %>%
      plotly::add_bars(data=clim_sp_avg, x= ~month, y= ~prec, yaxis="y2") %>%
      plotly::add_lines(y= ~tmean) %>%
      plotly::layout(yaxis=list(side="left", 
                                title=expression(paste("Temperature (",degree,"C)"))), 
                     yaxis2=list(overlaying="y", side="right", 
                                 title="Precipitation (mm)"))
  } else if(style == "ggplot"){
    clim_sp_avg$month <- c(1:12)
    ggplot2::ggplot(data=clim_sp_avg) + 
      ggplot2::geom_bar(ggplot2::aes(x=.data$month, y=prec/2), stat="identity",fill="blue") + 
      ggplot2::geom_line(ggplot2::aes(x=.data$month, y=tmean), colour="red") + ggplot2::theme_bw() + 
      ggplot2::scale_y_continuous(sec.axis = ggplot2::sec_axis(~ .*2, name="Precipitation (mm)")) + 
      ggplot2::scale_x_continuous(breaks=c(1:12),
                                  labels=c("J","F","M","A","M", "J","J","A","S","O","N","D")) + 
      ggplot2::labs(x="Month of the Year", y=expression(paste("Temperature (",degree,"C)")))
    if(save==TRUE){
      ggplot2::ggsave(filename=filename, height=height, width=width, bg=bg)
    }
  }
}
