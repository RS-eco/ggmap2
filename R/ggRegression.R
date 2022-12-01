#' Get GIMMS3g NDVI time series data
#'
#' @param data rasterStack with date information
#' @param vars currently only takes all variables
#' @param save specify if you want to save the plot to file or not, default = FALSE
#' @param filename define filename if plot should be saved to file
#' @param colour specify colour palette of ggplot2::scale_fill_gradientn()
#' @return ggplot2 object
#' @examples
#' \dontrun{
#' library(ggmap2)
#' dat_v0 <- ndvi3g(extent = c(10, 30, 30, 50), version="v0", startyear=1981, 
#'                  endyear=1985, path="/media/matt/Data/Documents/Wissenschaft/Data/GIMMS")
#' ggRegression(data=dat_v0)
#' }
#' @export
ggRegression <- function(data, vars=c("slope", "R2", "adjR2", "pValue"), save=FALSE, filename=NULL, 
                         colour=colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", 
                                                   "yellow", "#FF7F00", "red", "#7F0000"))(255)){
  # Get Dates
  date <- raster::getZ(data)
  
  # Turn into data.frame
  data <- t(data.frame(raster::rasterToPoints(data)))
  
  # Create empty output data.frame
  lin_reg_dat <- data.frame(x = data[1,], y = data[2,])
  for(j in 1:ncol(data)){
    # Run individual model
    m <- lm(data[c(-1,-2),j] ~ date)
    
    # Extract R2 value and save to dataframe
    lin_reg_dat$slope[j] <- m$coefficients[2]
    lin_reg_dat$R2[j] <- summary(m)$r.squared
    lin_reg_dat$adjR2[j] <- summary(m)$adj.r.squared
    lin_reg_dat$pValue[j] <- summary(m)$coefficients[2,4]
  }
  
  # Plot with faceting
  #lin_reg_dat <- tidyr::gather(lin_reg_dat, key="variable", value="value", -c(x,y))
  #lin_reg_dat <- dplyr::filter(lin_reg_dat, .data$variable %in% vars)
  #ggplot2::ggplot(data = lin_reg_dat, ggplot2::aes(x = x, y = y)) +
  #  ggplot2::geom_tile(ggplot2::aes(fill=value)) + ggplot2::facet_grid(.~variable) +
  #  ggplot2::scale_fill_gradientn(colors = colour,
  #                                na.value="transparent") +
  #  ggplot2::labs(x="Latitude", y="Longitude") + ggplot2::theme_bw() + 
  #  ggplot2::theme(legend.background = ggplot2::element_rect(fill="transparent", colour=NA),
  #                 axis.text.y = ggplot2::element_text(angle=90)) +
  #  ggplot2::guides(color = ggplot2::guide_legend(order=1), shape = ggplot2::guide_legend(order=2)) +
  #  ggplot2::coord_sf(expand=FALSE)
  
  lim1 <- c(signif(min(lin_reg_dat$slope, na.rm=T), digits=2),signif(max(lin_reg_dat$slope, na.rm=T), digits=2))
  lim2 <- c(signif(min(lin_reg_dat$R2, na.rm=T), digits=2),signif(max(lin_reg_dat$R2, na.rm=T), digits=2))
  lim3 <- c(signif(min(lin_reg_dat$adjR2, na.rm=T), digits=2),signif(max(lin_reg_dat$adjR2, na.rm=T), digits=2))
  lim4 <- c(signif(min(lin_reg_dat$pValue, na.rm=T), digits=2),round(max(lin_reg_dat$pValue, na.rm=T), digits=2))
  
  col_val1 <- scales::rescale(stats::quantile(lin_reg_dat$slope, probs=seq(0,1,0.2)))
  col_val2 <- scales::rescale(stats::quantile(lin_reg_dat$R2, probs=seq(0,1,0.12)))
  col_val3 <- scales::rescale(stats::quantile(lin_reg_dat$adjR2, probs=seq(0,1,0.12)))
  col_val4 <- scales::rescale(stats::quantile(lin_reg_dat$pValue, probs=seq(0,1,0.12)))
  
  #Plot map of slope, R2, adj. R2 and p-value
  p1 <- ggplot2::ggplot(data = lin_reg_dat, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_raster(ggplot2::aes(fill=.data$slope)) + 
    ggplot2::scale_fill_gradientn(name="Slope", colors = colour, na.value="transparent",
                                  limits=lim1, values=col_val1) +
    ggplot2::labs(x="Latitude", y="Longitude") + ggplot2::theme_bw() + 
    ggplot2::theme(legend.background = ggplot2::element_rect(fill="transparent", colour=NA),
          axis.text.y = ggplot2::element_text(angle=90)) +
    ggplot2::guides(color = ggplot2::guide_legend(order=1), shape = ggplot2::guide_legend(order=2)) +
    ggplot2::coord_sf(expand=FALSE)
  p2 <- ggplot2::ggplot(data = lin_reg_dat, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_raster(ggplot2::aes(fill=.data$R2)) + 
    ggplot2::scale_fill_gradientn(name="R2", colors = colour, na.value="transparent",
                                  limits=lim2, values=col_val2) +
    ggplot2::labs(x="Latitude", y="Longitude") + ggplot2::theme_bw() + 
    ggplot2::theme(legend.background = ggplot2::element_rect(fill="transparent", colour=NA),
                   axis.text.y = ggplot2::element_text(angle=90)) +
    ggplot2::guides(color = ggplot2::guide_legend(order=1), shape = ggplot2::guide_legend(order=2)) +
    ggplot2::coord_sf(expand=FALSE)
  p3 <- ggplot2::ggplot(data = lin_reg_dat, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_raster(ggplot2::aes(fill=.data$adjR2)) + 
    ggplot2::scale_fill_gradientn(name="Adjusted R2", colors = colour, na.value="transparent",
                                  limits=lim3, values=col_val3) +
    ggplot2::labs(x="Latitude", y="Longitude") + ggplot2::theme_bw() + 
    ggplot2::theme(legend.background = ggplot2::element_rect(fill="transparent", colour=NA),
                   axis.text.y = ggplot2::element_text(angle=90)) +
    ggplot2::guides(color = ggplot2::guide_legend(order=1), shape = ggplot2::guide_legend(order=2)) +
    ggplot2::coord_sf(expand=FALSE)
  p4 <- ggplot2::ggplot(data = lin_reg_dat, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_raster(ggplot2::aes(fill=.data$pValue)) + 
    ggplot2::scale_fill_gradientn(name="p-value", colors = colour, na.value="transparent",
                                  limits=lim4, values=col_val4) +
    ggplot2::labs(x="Latitude", y="Longitude") + ggplot2::theme_bw() + 
    ggplot2::theme(legend.background = ggplot2::element_rect(fill="transparent", colour=NA),
                   axis.text.y = ggplot2::element_text(angle=90)) +
    ggplot2::guides(color = ggplot2::guide_legend(order=1), shape = ggplot2::guide_legend(order=2)) +
    ggplot2::coord_sf(expand=FALSE)
  requireNamespace("patchwork")
  p <- p1 + p2 + p3 + p4 + patchwork::plot_layout(ncol=2)
  if(save==TRUE){
    if(is.null(filename)){filename="Rplots.pdf"}
    ggplot2::ggsave(filename=filename, plot=p)
  }
  return(p)
}
