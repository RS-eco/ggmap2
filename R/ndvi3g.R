#' Load ndvi3g data
#'
#' Function to dextract AVHRR GIMMS ndvi3g data for a specific 
#' timeperiod and area of your choice. To download the data see the downloadNDVI() function.
#' Data is available globally and for the timeperiod 1981 - 2015.
#'
#' @param path Path to file directory where data is stored, should be same as used in downloadNDVI().
#' @param extent An extent object.
#' @param startyear A year between 1981 and 2015.
#' @param endyear A year between 1981 and 2015, should be larger than startyear.
#' @param snap see raster::crop
#' @param version One of "v0" or "v1", otherwise function automatically uses "v1".
#' @param output specify the output variable, default is "ndvi". v0 provides two options: flag and ndvi. v1 provides two options: ndvi and percentile. 
#' @return raster stack with the extent of \code{extent} and layers for each year from \code{startyear} till \code{endyear}.
#' @examples
#' \dontrun{
#' dat_v0 <- ndvi3g(extent = c(10, 30, 30, 50), version="v0", 
#'                  startyear=1981, endyear=1982)
#' flag_dat_v0 <- ndvi3g(extent = c(10, 30, 30, 50), version="v0", startyear=1981, 
#'                       endyear=1982, output="flag")
#' dat_v1 <- ndvi3g(extent = c(10, 30, 30, 50), version="v1", startyear=1981, endyear=1982)
#' perc_dat_v1 <- ndvi3g(extent = c(10, 30, 30, 50), version="v1", startyear=1981, 
#'                       endyear=1982, output="percentile")
#' }
#' @export
ndvi3g <- function(path, extent=c(-180,180,-90,90), startyear=1981, endyear=2015, snap="near", version="v1",
                   output="ndvi"){
  if(startyear < 1981 | startyear > 2015){startyear <- 1981}
  if(endyear < startyear | endyear > 2015){endyear <- 2015}
  if(endyear > 2013){version == "v1"}
  year <- startyear:endyear
  
  # read file list
  # file_list <- read.csv(paste0(path, "/file_list.txt"))
  
  # set to loop through years and months within each year
  if(version == "v0"){
    # Define extent
    extent <- raster::extent(extent)
    
    # specify temporal parameters
    year.name <- c("82","83","84","85","86","87","88","89","90","91","92","93","94","95",
                   "96","97","98","99","00","01","02","03","04","05","06","07","08","09","10","11","12","13")
    year.name <- year.name[year.name %in% substr(year, 3, 4)]
    month.name <- c("jan", "feb", "mar", "apr", "may", "jun","jul","aug", "sep","oct","nov","dec")
    
    # Merge year.name, month combinations
    year.month <- expand.grid(month=month.name, year=year.name)
    if(startyear == 1981){
      month.name <- c("jul","aug", "sep","oct","nov","dec")
      year.name <- "81"
      year.month <- rbind(expand.grid(month=month.name, year=year.name), year.month)
    }
    
    ndvi_15day  <- lapply(1:nrow(year.month), function(j){
      
      # set name format 
      file.path.a <- list.files(path, pattern=paste0("geo", year.month$year[j], year.month$month[j], 15, "a"), full.names=TRUE, recursive=T)
      file.path.b <- list.files(path, pattern=paste0("geo", year.month$year[j], year.month$month[j], 15, "b"), full.names=TRUE, recursive=T)
      
      # Read binary file
      binread.a <- base::readBin(con=file.path.a[[1]], what="integer", n=9331200, size=2, signed=T, endian="big")
      binread.b <- base::readBin(con=file.path.b[[1]], what="integer", n=9331200, size=2, signed=T, endian="big")
      
      # covert binary object into a matrix using specifications from the metadata
      mtrx.a <- matrix(binread.a, nrow=2160, ncol=4320, byrow=F)
      mtrx.b <- matrix(binread.b, nrow=2160, ncol=4320, byrow=F)
      
      # convert from matrix to a raster object
      rstr <- raster::stack(raster::raster(mtrx.a), raster::raster(mtrx.b))
      
      # specify extent and projection
      raster::extent(rstr) <- raster::extent(c(-180, 180, -90, 90))
      raster::projection(rstr) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
      
      #Cut raster to extent of studyarea
      rstr <- raster::crop(rstr, extent, snap=snap)
      
      # set NA values
      rstr <- raster::subs(rstr, data.frame(id=-5000, v=NA), subsWithNA=FALSE)
      
      # set water values also to NA
      rstr <- raster::subs(rstr, data.frame(id=-10000, v=NA), subsWithNA=FALSE)
      
      #Scaling to actual value
      if(output=="flag"){
        rstr <- rstr-floor(rstr/10)*10 + 1
        #FLAG = 7 (missing data)
        #FLAG = 6 (NDVI retrieved from average seasonal profile, possibly snow)
        #FLAG = 5 (NDVI retrieved from average seasonal profile)
        #FLAG = 4 (NDVI retrieved from spline interpolation, possibly snow)
        #FLAG = 3 (NDVI retrieved from spline interpolation)
        #FLAG = 2 (Good value)
        #FLAG = 1 (Good value)
      } else{
        rstr <- floor(rstr/10)/1000
      }
      return(rstr); rm(binread.a, binread.b, rstr); gc()
    })
    #Save 15day NDVI in raster stack
    ndvi_15day <- raster::stack(ndvi_15day)
    
    #Set time of raster values 
    if(startyear == 1981){
      ndvi_15day <- raster::setZ(ndvi_15day, c(seq(from = as.Date(paste(startyear, "07-15", sep="-")), 
                                                         to = as.Date(paste(startyear, "12-30", sep="-")), 
                                                         length=12),
                                                     seq(from = as.Date(paste(startyear+1, "01-15", sep="-")), 
                                                         to = as.Date(paste(endyear, "12-30", sep="-")), 
                                                         length=24*(endyear-startyear))))
      names(ndvi_15day) <- format(c(seq(from = as.Date(paste(startyear, "07-15", sep="-")), 
                                           to = as.Date(paste(startyear, "12-30", sep="-")), 
                                           length=12),
                                       seq(from = as.Date(paste(startyear+1, "01-15", sep="-")), 
                                           to = as.Date(paste(endyear, "12-30", sep="-")), 
                                           length=24*(endyear-startyear))), '%Y%m%d')
    } else{
    ndvi_15day <- raster::setZ(ndvi_15day, seq(from = as.Date(paste(startyear, "01-15", sep="-")), 
                                                     to = as.Date(paste(endyear, "12-30", sep="-")), 
                                                     length=24*(endyear-startyear+1)))
    names(ndvi_15day) <- format(seq(from = as.Date(paste(startyear, "01-15", sep="-")), 
                                       to = as.Date(paste(endyear, "12-30", sep="-")), 
                                       length=24*(endyear-startyear+1)), '%Y%m%d')
    }
  } else{
    ndvi_15day  <- lapply(1:length(year), function(j){
      # set name format 
      file.paths <- list.files(path, pattern=paste0("geo", "_", version, "_", year[j]), full.names=TRUE, recursive=T)
      
      # convert from matrix to a raster object
      rstr <- lapply(file.paths, function(x) raster::stack(x, varname=output))
      rstr <- raster::stack(rstr)
  
      #Cut raster to extent of studyarea
      rstr <- raster::crop(rstr, extent, snap=snap)
      
      # set NA values
      if(output=="percentile"){
        rstr <- raster::subs(rstr, data.frame(id=0, v=NA), subsWithNA=FALSE)
      } else{
        rstr <- raster::subs(rstr, data.frame(id=-32768, v=NA), subsWithNA=FALSE)
      }
      
      #Scaling to actual value
      rstr <- floor(rstr/10)/1000
      return(rstr); rm(rstr); gc()
    })
    #Save 15day NDVI in raster stack
    ndvi_15day <- raster::stack(ndvi_15day)
    
    #Set time of raster values
    if(startyear == 1981){
      ndvi_15day <- raster::setZ(ndvi_15day, c(seq(from = as.Date(paste(startyear, "07-01", sep="-")), 
                                                         to = as.Date(paste(startyear, "12-31", sep="-")), 
                                                         length=12),
                                    seq(from = as.Date(paste(startyear+1, "01-01", sep="-")), 
                                                       to = as.Date(paste(endyear, "12-30", sep="-")), 
                                                       length=24*(endyear-startyear))))
      names(ndvi_15day) <- format(c(seq(from = as.Date(paste(startyear, "07-01", sep="-")), 
                                           to = as.Date(paste(startyear, "12-31", sep="-")), 
                                           length=12),
                                       seq(from = as.Date(paste(startyear+1, "01-01", sep="-")), 
                                           to = as.Date(paste(endyear, "12-30", sep="-")), 
                                           length=24*(endyear-startyear))), '%Y%m%d')
    } else{
      ndvi_15day <- raster::setZ(ndvi_15day, seq(from = as.Date(paste(startyear, "01-01", sep="-")), 
                                                       to = as.Date(paste(endyear, "12-15", sep="-")), 
                                                       length=24*(endyear-startyear+1)))
      names(ndvi_15day) <- format(seq(from = as.Date(paste(startyear, "01-01", sep="-")), 
                                         to = as.Date(paste(endyear, "12-15", sep="-")), 
                                         length=24*(endyear-startyear+1)), '%Y%m%d')
    }
  }
  return(ndvi_15day)
}
