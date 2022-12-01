#' Create map(s) of one raster layer or a raster stack
#'
#' Read a global raster layer or raster stack and plot on a map
#' 
#' @param data raster layer, raster stack or dataframe
#' @param name character, name of legend
#' @param split logical, should multiple plots have individual legends or not
#' @param trans transform data
#' @param facet_grid logical, default to FALSE, if TRUE facet_grid is used for plotting
#' @param crop logical, default to TRUE, not implemented right now
#' @param grid logical, default to FALSE, if TRUE a grid is added to the plot
#' @param colours specify colourPalette used for ggplot2::scale_fill_gradientn()
#' @param varnames character vector with names of data
#' @param subnames character vector with subnames of data
#' @param outline default is NA
#' @param extent defaults to NA
#' @param ncol specify number of columns for plotting with facet_wrap or ..., 
#' not needed if facet_grid is TRUE.
#' @param long defaults to FALSE. Specify if data is in long format or not.
#' @param country defaults to TRUE. Add country outline to plot.
#' @param save defaults to FALSE. Specify if plot should be written to file.
#' @param filename If save=TRUE you can specify the filename of your plot here
#' @return ggplot2 object
#' @examples
#' # Load raster library
#' library(raster)
#' # Create data
#' r <- raster(nrow=90, ncol=180)
#' r <- setValues(r, rnorm(ncell(r), 0, 1))
#' #Create plots
#' ggmap2(r, name="Single", split=FALSE, ncol=1)
#' ggmap2(r, name="Single", split=FALSE, ncol=1, extent=c(-20, 70, -50, 80))
#' @export
ggmap2 <- function(data, name="", split=FALSE, trans="identity", facet_grid=FALSE, crop=TRUE, grid=FALSE, 
                   colours=colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))(255),
                   varnames=NA, subnames="", outline=NA, extent=NA, ncol=2, long=FALSE, country=TRUE, save=FALSE, filename="Rplots.pdf"){
  requireNamespace("dplyr")
  
  # Set plotting theme
  if(grid == TRUE){
    ggplot2::theme_set(ggplot2::theme_bw() + ggplot2::theme(
      axis.title.x = ggplot2::element_text(size=16),
      axis.title.y = ggplot2::element_text(size=16, angle=90),
      axis.text.x = ggplot2::element_text(size=14),
      axis.text.y = ggplot2::element_text(size=14),
      legend.text = ggplot2::element_text(size=14, hjust=1),
      legend.title = ggplot2::element_text(size=16),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "right"))
  } else{
    # Without labels
    ggplot2::theme_set(ggplot2::theme_classic() + ggplot2::theme(
      axis.title = ggplot2::element_blank(), axis.line = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(), axis.text = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size=14, hjust=1),
      panel.grid = ggplot2::element_blank(), 
      legend.position = "right"))
  }
  
  # Convert data to dataframe
  if(any(class(data) %in% c("RasterLayer", "RasterStack", "RasterBrick"))){
    data <- data.frame(raster::rasterToPoints(data))
  } else if(any(class(data) == "data.frame")){
    data <- as.data.frame(data) 
  } else{
    warning("Data is not a raster object nor a dataframe.")
  }
  
  if(anyNA(outline)){
    #Load world data for mapping
    data(outline, envir=environment())
  }
  
  if(anyNA(extent)){
    extent <- raster::extent(c(min(data$x, na.rm=TRUE), max(data$x, na.rm=TRUE), 
                               min(data$y, na.rm=TRUE), max(data$y, na.rm=TRUE)))
  } else{
    extent <- raster::extent(extent)
    data <- dplyr::filter(data, .data$x >= raster::xmin(extent) & .data$x <= raster::xmax(extent))
    data <- dplyr::filter(data, .data$y >= raster::ymin(extent) & .data$y <= raster::ymax(extent))
    if(country==TRUE){
      outline <- sf::st_crop(outline, extent, snap="in")
    }
  }
  
  # Calculate required breaks
  breaks_x <- round(seq(raster::xmin(extent), raster::xmax(extent), length=7), digits=0)
  breaks_y <- round(seq(raster::ymin(extent), raster::ymax(extent), length=7), digits=0)
  
  # Calculate plotting ratio
  #ratio <- 1/round((raster::xmax(extent)-raster::xmin(extent))/
  #                   (raster::ymax(extent)-raster::ymin(extent)))
  
  # Set up a default plot
  ppp <- ggplot2::ggplot()     #+ 
  #scale_x_continuous(name=expression(paste("Longitude (",degree,")")), 
  #                       expand=c(0.01,0.01), breaks=breaks_x) + 
  #scale_y_continuous(name=expression(paste("Latitude (",degree,")")), 
  #                   expand=c(0,5), breaks=breaks_y) + 
  #ggplot2::coord_quickmap(xlim=c(raster::xmin(extent), raster::xmax(extent)), 
  #                ylim=c(raster::ymin(extent), raster::ymax(extent)))
  
  if(ncol(data) == 3){
    colnames(data) <- c("x","y","z")
    # Create plot of raster layer with country outline
      p <- ppp + ggplot2::geom_tile(data=data, ggplot2::aes(x=.data$x,y=.data$y,fill=.data$z)) + 
      ggplot2::scale_fill_gradientn(name=name, colours=colours, 
                           na.value="transparent", trans=trans,
                           limits=c(floor(min(data$z, na.rm=T)), ceiling(max(data$z, na.rm=TRUE))))
      if(country == TRUE){
        p <- p + ggplot2::geom_sf(data=outline, fill="transparent", color="black") + ggplot2::coord_sf()
      }
      if(save==TRUE){
      ggplot2::ggsave(filename=filename)
    }
    p
  } else if(split == FALSE){
    if(long == FALSE){
      #if(length(varnames == 2)){
      # Format data for facet_grid
      #colnames(data)[1:2] <- c("x", "y")
      #if(length(varnames[[1]]) == (ncol(data)-2)){
      #  colnames(data)[3:ncol(data)] <- sapply(1:length(varnames[[1]]), FUN=function(x){
      #    paste0(varnames[[1]][x], varnames[[2]][x])
      #  })
      #}
      #data <- tidyr::gather(data, "var", "value", -c(x, y))
      #data$var <- sapply(data$var, function(x) strsplit(x))
      #} else{
      # Create facet plot of raster stack with country outline
      colnames(data)[1:2] <- c("x", "y")
      colnames(data)[3:ncol(data)] <- letters[seq(1,ncol(data)-2,1)]
      # n <- ceiling((ncol(data)-2)/2) # Needed for width and height adjustment
      data <- tidyr::gather(data, key="var", value="value", -c("x","y"))
      data[,"value"] <- as.numeric(data[,"value"])
      #}
    } else if(ncol(data) == 4){
      colnames(data)[3:4] <- c("var", "value")
    }
    if(unique(!is.na(subnames))){data$var <- factor(data$var, labels=subnames)}
    if(facet_grid == TRUE){
      # see: https://stackoverflow.com/questions/20552226/make-one-panel-blank-in-ggplot2
      p <- ppp + ggplot2::geom_tile(data=data, ggplot2::aes(x=.data$x,y=.data$y,fill=.data$value)) + 
        ggplot2::scale_fill_gradientn(name=name, colours=colours, 
                             na.value="transparent", trans=trans,
                             limits=c(floor(min(data$value, na.rm=T)), ceiling(max(data$value, na.rm=TRUE)))) + 
        ggplot2::facet_grid(var ~ var2) + 
        ggplot2::theme(strip.background= ggplot2::element_blank(), 
              strip.text.x = ggplot2::element_text(size=16),
              panel.spacing.x=ggplot2::unit(1.5, "lines"),
              panel.spacing.y=ggplot2::unit(1, "lines"))
    } else{
      p <- ppp + ggplot2::geom_tile(data=data, ggplot2::aes(x=.data$x,y=.data$y,fill=.data$value)) + 
        ggplot2::scale_fill_gradientn(name=name, colours=colours, 
                             na.value="transparent", trans=trans,
                             limits=c(floor(min(data$value, na.rm=T)), ceiling(max(data$value, na.rm=TRUE)))) + 
        ggplot2::facet_wrap(~ var, ncol=ncol) + 
        ggplot2::theme(strip.background= ggplot2::element_blank(), 
              strip.text.x = ggplot2::element_text(size=16),
              panel.spacing.x=ggplot2::unit(1.5, "lines"),
              panel.spacing.y=ggplot2::unit(1, "lines"))
    }
    if(country == TRUE){
      p <- p + ggplot2::geom_sf(data=outline, fill="transparent", color="black") + ggplot2::coord_sf()
    }
    if(save==TRUE){
      ggplot2::ggsave(filename=filename)
    }
    return(p)
  } else if(split == TRUE){
    #Create separate multiple plots
    n <- ncol(data)-2
    colnames(data)[1:2] <- c("x", "y")
    z <- paste0("z", seq(1,n,1))
    colnames(data)[3:ncol(data)] <- z
    labels <- letters[seq(1,n,1)]
    if(length(name) != n){rep(name, length=n)}
    
    # Create plot for each variable and turn into grob element
    p <- lapply(1:n, FUN=function(a){
      p <- ppp + ggplot2::geom_tile(data=data, ggplot2::aes_string("x","y",fill=z[a])) + 
        ggplot2::scale_fill_gradientn(name=name[a], colours=colours, 
                             na.value="transparent", trans=trans,
                             limits=c(floor(min(data[,z[a]], na.rm=TRUE)), ceiling(max(data[,z[a]], na.rm=TRUE)))) + 
        #ggtitle(paste0(labels[a], ")")) +
        ggplot2::theme(legend.key = ggplot2::element_blank(),
              plot.title = ggplot2::element_text(size=18, hjust=-0.14))
      if(country == TRUE){
        p <- p + ggplot2::geom_sf(data=outline, fill="transparent", color="black") + ggplot2::coord_sf()
      }
      return(p)
    })
    
    # Adjust plot number to account for uneven number of plots for grid
    if(ceiling(n/ncol) == n/ncol){
      # Get number of empty plots
      #n_new <- ceiling(n/ncol)*ncol-n
      
      # Create additional ggplot
      #p_blank <- lapply(1:n_new, FUN=function(x){
      #  p[[1]] + theme(legend.position="none")
      #})
      #p <- append(p, p_blank)
      
      #g_blank <- ggplot2::ggplotGrob(ppp + ggplot2::geom_raster(data=data, ggplot2::aes_string("x","y"), fill="white") + 
      #  theme(legend.key = element_blank(),
      #        plot.title=element_text(size=18, hjust=-0.14),
      #        text = element_text(colour="white"),
      #        line = element_blank(),
      #        rect = element_blank(),
      #        axis.text.x = element_blank(),
      #        axis.text.y = element_blank()))
      
      # Turn plots into grob elements
      g <- lapply(p, ggplot2::ggplotGrob)
      
      # Adjust grobs of additional plots with blank_grob
      #g[6][[1]]$grobs[c(1:14,16:18)] <- g_blank$grobs
      #g[6][[1]]$grobs[g[6][[1]]$name=="guide-box"] <- NULL
      
      # Add empty plot to grob
      # see: https://stackoverflow.com/questions/22450765/how-to-use-empty-space-produced-by-facet-wrap
      #pg <- gtable_add_grob(pg, qg, t=max(pl$t), l=max(pl$l))
      
      # Adjust maximum width of plot legends for each column
      # see: https://stackoverflow.com/questions/13294952/left-align-two-graph-edges-ggplot?rq=1
      if(ncol != 1 & !is.na(ncol)){
        cols <- lapply(1:ncol, FUN=function(x){seq(x, length(g), by=ncol)})
        requireNamespace("gtable")
        g <- lapply(cols, FUN=function(x){do.call("rbind", c(g[x], size="last"))})
        g <- do.call("cbind", c(g, size = "first"))
      } else {
        g <- do.call("rbind", c(g, size="first"))
      }
      # Issues with Legends require Grob workaround for Multiplot 
      # following: http://stackoverflow.com/questions/17462504/align-edges-of-ggplot-chloropleth-legend-title-varies?rq=1
      for (i in which(g$layout$name == "guide-box")) {
        g$grobs[[i]] <- g$grobs[[i]]$grobs[[1]]
      }
      #Save to file and plot
      if(save==TRUE){
        grid::grid.newpage()
        bitmap(file=filename)
        grid::grid.draw(g)
        dev.off(); rm(g)
      } else{
        grid::grid.newpage()
        grid::grid.draw(g); rm(g)
      }
    } else{
      #Save to file and plot
      if(save==TRUE){
        grid::grid.newpage()
        bitmap(file=filename)
        nrow <- ceiling(length(p)/ncol)
        do.call(gridExtra::grid.arrange, c(p, nrow=nrow, ncol = ncol))
        dev.off()
      } else{
        grid::grid.newpage()
        nrow <- ceiling(length(p)/ncol)
        do.call(gridExtra::grid.arrange, c(p, nrow=nrow, ncol = ncol))
      }
    }
  }
}
