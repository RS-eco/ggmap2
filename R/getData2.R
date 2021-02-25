#' Get geographic data
#'
#' @description Get geographic data for anywhere in the world. Data are read from files that are first downloaded if necessary. 
#' Function ccodes returns country names and the ISO codes
#' 
#' This function extends the getData function from the raster package to download and
#' access various other geographic data (BioOracle, GEBCO, MarSpec, OceanColor, WOA, WDPA, gimms3g).
#'
#' @usage 
#' getData2(name, download=TRUE, extent='', path='', ...)
#' ccodes()
#'
#' @param name  Data set name, currently supported are 'GADM', 'countries', 'SRTM', 'alt', 'CMIP5', 'WDPA' and 'worldclim'. 
#' See Details for more info. GADM', 'countries', 'SRTM', 'alt', 'CMIP5' and 'worldclim' are further supported by the getData() function of the raster package.
#' @param download Logical. If TRUE data will be downloaded if not locally available
#' @param path Character. Path name indicating where to store the data. Default is the current working directory
#' @param extent extent object. Spatial object extent delimiting the area you are interested in.
#' @param ... Additional required (!) parameters. These are data set specific. See Details
#'
#' @details 'alt' stands for altitude (elevation); the data were aggregated from SRTM 90 m resolution data between -60 and 60 latitude. 'GADM' is a database of global administrative boundaries. 'worldclim' is a database of global interpolated climate data. 'SRTM' refers to the hole-filled CGIAR-SRTM (90 m resolution). 
#' 
#' 'countries' has polygons for all countries at a higher resolution than the 'wrld_simpl' data in the maptools package . 
#' 
#' If \code{name} is 'alt' or 'GADM' you must provide a 'country=' argument. Countries are specified by their 3 letter ISO codes. Use getData('ISO3') to see these codes. In the case of GADM you must also provide the level of administrative subdivision (0=country, 1=first level subdivision). In the case of alt you can set 'mask' to FALSE. If it is TRUE values for neighbouring countries are set to NA. For example:
#' \code{getData('GADM', country='FRA', level=1)}
#' \code{getData('alt', country='FRA', mask=TRUE)}
#' If \code{name} is 'SRTM' you must provide 'lon' and 'lat' arguments (longitude and latitude). These should be single numbers somewhere within the SRTM tile that you want.
#' \code{getData('SRTM', lon=5, lat=45)}
#' If \code{name='worldclim'} you must also provide arguments \code{var}, and a resolution \code{res}. Valid variables names are 'tmin', 'tmax', 'prec' and 'bio'. Valid resolutions are 0.5, 2.5, 5, 10, 30 and 60 (minutes of a degree). In the case of \code{res=0.5}, you must also provide a \code{lon} and \code{lat} argument for a tile or an extent argument; for the lower resolutions global data will be downloaded. In all cases there are 12 (monthly) files for each variable except for 'bio' which contains 19 files.
#' \code{getData('worldclim', var='tmin', res=0.5, lon=5, lat=45)}
#' \code{getData('worldclim', var='bio', res=10)}
#' To get (projected) future climate data (CMIP5), you must provide arguments \code{var} and \code{res} as above. Only resolutions 2.5, 5, and 10 are currently available. 
#' In addition, you need to provide \code{model}, \code{rcp} and \code{year}. For example,
#' \code{getData('CMIP5', var='tmin', res=10, rcp=85, model='AC', year=70)}
#' function (var, model, rcp, year, res, lon, lat, path, download = TRUE) 
#' 'model' should be one of "AC", "BC", "CC", "CE", "CN", "GF", "GD", "GS", "HD", "HG", "HE", "IN", "IP", "MI", "MR", "MC", "MP", "MG", or  "NO".
#' 'rcp' should be one of 26, 45, 60, or 85.
#' 'year' should be 50 or 70
#' Not all combinations are available. See www.worldclim.org for details.
#' 
#' If \code{name='WDPA'} you can provide a 'country=' argument to download the current protected areas of the specific country.
#' \code{getData('WDPA', country='LIE')}
#' @return A spatial object (Raster* or sf`*)
#'  
#' @examples
#' 
#' \dontrun{
#' getData2(name = "worldclim", res=30, var="tmin")
#' getData2(name="worldclim", res=10, var="tmin", extent=c(8, 14, 47, 51))
#' getData2(name="worldclim", res=0.5, var="tmin", extent=c(0, 20, 20, 50))
#' getData2(name="CMIP5", var="tmin", res=30, rcp=85, model="AC", year=70)
#' getData2(name="CMIP5", var="tmin", res=10, rcp=85, model="AC", year=70, extent=c(0, 20, 20, 50))
#' getData2(name = "WDPA", country="LIE")
#' }
#' 
#' @references 
#' \url{http://www.worldclim.org}
#' \url{http://www.gadm.org}
#' \url{http://srtm.csi.cgiar.org/}
#' \url{http://diva-gis.org/gdata}
#' \url{http://protectedplanet.net}
#'  
#' @keywords spatial
#' 
#' @export
getData2 <- function(name='GADM', download=TRUE, extent='', path='', ...) {
  path <- .getDataPath(path)
  if (name=='GADM') {
    .GADM(..., download=download, path=path, version=2.8)
  } else if (name=='SRTM') {
    .SRTM(..., download=download, path=path)
  } else if (name=='alt') {
    .raster(..., name=name, download=download, path=path)
  } else if (name=='worldclim') {
    .worldclim(..., download=download, extent=extent, path=path)
  } else if (name=='CMIP5') {
    .cmip5(..., download=download, extent=extent, path=path)
  } else if (name=='ISO3') {
    ccodes()[,c(2,1)]
  } else if (name=='countries') {
    .countries(download=download, path=path, ...)
    #} else if (name == "Bio-Oracle"){
    #  .biooracle(download=download, path=path,...)
    #} else if (name == "GEBCO") {
    #  .gebco(download=download, path=path, ...)
    #} else if (name == "MarSpec") {
    #  .marspec(download=download, path=path, ...)
  } else if(name == "WDPA"){
    .wdpa(download=download, path=path, ...)
    #} else if(name == "gimms3g"){
    #  .ndvi3g(download=download, path=path, ...)
    #} else if(name == "emis"){
    #  .emis(download=download, path=path, ...)
  } else {
    stop(name, ' not recognized as a valid name.')
  }
}

.download <- function(aurl, filename) {
  fn <- paste(tempfile(), '.download', sep='')
  res <- utils::download.file(url=aurl, destfile=fn, method="auto", quiet = FALSE, mode = "wb", cacheOK = TRUE)
  if (res == 0) {
    w <- getOption('warn')
    on.exit(options('warn' = w))
    options('warn'=-1) 
    if (! file.rename(fn, filename) ) { 
      # rename failed, perhaps because fn and filename refer to different devices
      file.copy(fn, filename)
      file.remove(fn)
    }
  } else {
    stop('could not download the file' )
  }
}

.ISO <- function() {
  ccodes()
}

ccodes <- function() {
  path <- system.file(package="raster")
  #d <- utils::read.csv(paste(path, "/external/countries.csv", sep=""), stringsAsFactors=FALSE, encoding="UTF-8")
  readRDS(file.path(path, "external/countries.rds"))
}

.getCountry <- function(country='') {
  country <- toupper(raster::trim(country[1]))
  
  cs <- ccodes()
  cs <- sapply(cs, toupper)
  cs <- data.frame(cs, stringsAsFactors=FALSE)
  nc <- nchar(country)
  
  if (nc == 3) {
    if (country %in% cs$ISO3) {
      return(country)
    } else {
      stop('unknown country')
    }
  } else if (nc == 2) {
    if (country %in% cs$ISO2) {
      i <- which(country==cs$ISO2)
      return( cs$ISO3[i] )
    } else {
      stop('unknown country')
    }
  } else if (country %in% cs[,1]) {
    i <- which(country==cs[,1])
    return( cs$ISO3[i] )
  } else if (country %in% cs[,4]) {
    i <- which(country==cs[,4])
    return( cs$ISO3[i] )
  } else if (country %in% cs[,5]) {
    i <- which(country==cs[,5])
    return( cs$ISO3[i] )
  } else {
    stop('provide a valid name name or 3 letter ISO country code; you can get a list with "ccodes()"')
  }
}

.getDataPath <- function(path) {
  path <- raster::trim(path)
  if (path=='') {
    path <- getwd()
  } else {
    if (substr(path, nchar(path)-1, nchar(path)) == '//' ) {
      p <- substr(path, 1, nchar(path)-2)		
    } else if (substr(path, nchar(path), nchar(path)) == '/'  | substr(path, nchar(path), nchar(path)) == '\\') {
      p <- substr(path, 1, nchar(path)-1)
    } else {
      p <- path
    }
    if (!file.exists(p) & !file.exists(path)) {
      stop('path does not exist: ', path)
    }
  }
  if (substr(path, nchar(path), nchar(path)) != '/' & substr(path, nchar(path), nchar(path)) != '\\') {
    path <- paste(path, "/", sep="")
  }
  return(path)
}

.GADM <- function(country, level, download, path, version) {
  #	if (!file.exists(path)) {  dir.create(path, recursive=T)  }
  
  country <- .getCountry(country)
  if (missing(level)) {
    stop('provide a "level=" argument; levels can be 0, 1, or 2 for most countries, and higher for some')
  }
  
  filename <- paste(path, 'GADM_', version, '_', country, '_adm', level, ".rds", sep="")
  if (!file.exists(filename)) {
    if (download) {
      
      baseurl <- paste0("http://biogeo.ucdavis.edu/data/gadm", version)
      if (version == 2) {
        theurl <- paste(baseurl, '/R/', country, '_adm', level, ".RData", sep="")
      } else {
        theurl <- paste(baseurl, '/rds/', country, '_adm', level, ".rds", sep="")			
      }
      
      .download(theurl, filename)
      if (!file.exists(filename))	{ 
        message("\nCould not download file -- perhaps it does not exist") 
      }
    } else {
      message("File not available locally. Use 'download = TRUE'")
    }
  }	
  if (file.exists(filename)) {
    if (version == 2) {
      thisenvir <- new.env()
      data <- get(load(filename, thisenvir), thisenvir)
    } else {
      data <- readRDS(filename)
    }
    return(data)
  } else {
    return(NULL)
  }
}

.countries <- function(download, path, type='sp', ...) {
  if (type == 'sf') {
    f <- "countries_gadm36_sf.rds"
  } else {
    f <- "countries_gadm36_sp.rds"	
  }
  filename <- file.path(path, f)
  
  if (!file.exists(filename)) {
    if (download) {
      theurl <- paste0("https://biogeo.ucdavis.edu/data/gadm3.6/", f)
      .download(theurl, filename)
      if (!file.exists(filename)) {
        message("\nCould not download file -- perhaps it does not exist") 
      }
    } else {
      message("File not available locally. Use 'download = TRUE'")
    }
  }	
  if (file.exists(filename)) {
    #thisenvir = new.env()
    #data <- get(load(filename, thisenvir), thisenvir)
    data <- readRDS(filename)
    return(data)
  } 
}

.cmip5 <- function(var, model, rcp, year, res, lon, lat, path, extent, download=TRUE) {
  if (!res %in% c(0.5, 2.5, 5, 10, 30, 60)) {
    stop('resolution should be one of: 0.5, 2.5, 5, 10, 30, 60')
  }
  if(res > 10){fact=res/10-0.000001; res <- 10}
  if (res==2.5) { 
    res <- '2_5m' 
  } else if (res == 0.5) {
    res <- "30s"
  } else {
    res <- paste(res, 'm', sep='')
  }
  
  var <- tolower(var[1])
  vars <- c('tmin', 'tmax', 'prec', 'bio')
  stopifnot(var %in% vars)
  var <- c('tn', 'tx', 'pr', 'bi')[match(var, vars)]
  
  model <- toupper(model)
  models <- c('AC', 'BC', 'CC', 'CE', 'CN', 'GF', 'GD', 'GS', 'HD', 'HG', 'HE', 'IN', 'IP', 'MI', 'MR', 'MC', 'MP', 'MG', 'NO')
  stopifnot(model %in% models)
  
  rcps <- c(26, 45, 60, 85)
  stopifnot(rcp %in% rcps)
  stopifnot(year %in% c(50, 70))
  
  #m <- matrix(c(0,1,1,0,1,1,1,1,1,1,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,0,0,1,0,1,1,1,0,0,1,1,1,1,0,1,1,1,1,1,0,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1), ncol=4)
  m <- matrix(c(0,1,1,0,1,1,1,1,1,0,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,0,0,0,1,1,1,0,1,0,1,1,1,1,0,1,1,1,1,1,0,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1), ncol=4)
  
  i <- m[which(model==models), which(rcp==rcps)]
  if (!i) {
    warning('this combination of rcp and model is not available')
    return(invisible(NULL))
  }
  
  path <- paste(path, '/cmip5/', res, '/', sep='')
  dir.create(path, recursive=TRUE, showWarnings=FALSE)
  
  zip <- tolower(paste(model, rcp, var, year, '.zip', sep=''))
  theurl <- paste('http://biogeo.ucdavis.edu/data/climate/cmip5/', res, '/', zip, sep='')
  
  zipfile <- paste(path, zip, sep='')
  if (var == 'bi') {
    n <- 19
  } else {
    n <- 12
  }
  tifs <- paste(raster::extension(zip, ''), 1:n, '.tif', sep='')
  files <- paste(path, tifs, sep='')
  fc <- sum(file.exists(files))
  if (fc < n) {
    if (!file.exists(zipfile)) {
      if (download) {
        .download(theurl, zipfile)
        if (!file.exists(zipfile))	{ 
          message("\n Could not download file -- perhaps it does not exist") 
        }
      } else {
        message("File not available locally. Use 'download = TRUE'")
      }
    }	
    utils::unzip(zipfile, exdir=dirname(zipfile))
  }
  st<- raster::stack(paste(path, tifs, sep=''))
  # Aggregate data resolution larger than 10 (Increases the outline of the world???)
  if(exists("fact")){
    st <- raster::aggregate(x=st, fact=fact, fun=mean, expand=F, na.rm=T)
  }
  if(extent!=''){
    st <- raster::crop(x=st, y=raster::extent(extent))
  }
  return(st)
}

.worldclim <- function(var, res, lon, lat, path, extent, download=TRUE) {
  if (!res %in% c(0.5, 2.5, 5, 10, 30, 60)) {
    stop('resolution should be one of: 0.5, 2.5, 5, 10, 30, 60')
  }
  if(res > 10){fact=res/10-0.000001; res <- 10}
  if (res==2.5) { res <- '2-5' }
  stopifnot(var %in% c('tmean', 'tmin', 'tmax', 'prec', 'bio', 'alt'))
  path <- paste(path, 'wc', res, '/', sep='')
  dir.create(path, showWarnings=FALSE)
  if (res==0.5) {
    if(exists("extent")){
      ex_lon <- c(extent[1], extent[2])
      ex_lat <- c(extent[3], extent[4])
      lon <- expand.grid(lon=ex_lon,lat=ex_lat)$lon
      lat <- expand.grid(lon=ex_lon,lat=ex_lat)$lat
    }
    lon <- sapply(lon, function(x) min(180, max(-180, x)))
    lat <- sapply(lat, function(x) min(90, max(-60, x)))
    rs <- raster::raster(nrows=5, ncols=12, xmn=-180, xmx=180, ymn=-60, ymx=90 )
    row <- sapply(lat, function(x) raster::rowFromY(rs, x) - 1)
    col <- sapply(lon, function(x) raster::colFromX(rs, x) - 1)
    rc <- unique(paste(row, col, sep=''))
    zip <- paste(var, '_', rc, '.zip', sep='')
    zipfile <- paste(path, zip, sep='')
    if (var  == 'alt') {
      bilfiles <- paste(var, '_', rep(rc, each=12), '.bil', sep='')
      hdrfiles <- paste(var, '_', rep(rc, each=12), '.hdr', sep='')			
    } else if (var  != 'bio') {
      bilfiles <- paste(var, 1:12, '_', rep(rc, each=12), '.bil', sep='')
      hdrfiles <- paste(var, 1:12, '_', rep(rc, each=12), '.hdr', sep='')
    } else {
      bilfiles <- paste(var, 1:19, '_', rep(rc, each=12), '.bil', sep='')
      hdrfiles <- paste(var, 1:19, '_', rep(rc, each=12), '.hdr', sep='')		
    }
    theurl <- paste('http://biogeo.ucdavis.edu/data/climate/worldclim/1_4/tiles/cur/', zip, sep='')
  } else {
    zip <- paste(var, '_', res, 'm_bil.zip', sep='')
    zipfile <- paste(path, zip, sep='')
    if (var  == 'alt') {
      bilfiles <- paste(var, '.bil', sep='')
      hdrfiles <- paste(var, '.hdr', sep='')			
    } else if (var  != 'bio') {
      bilfiles <- paste(var, 1:12, '.bil', sep='')
      hdrfiles <- paste(var, 1:12, '.hdr', sep='')
    } else {
      bilfiles <- paste(var, 1:19, '.bil', sep='')
      hdrfiles <- paste(var, 1:19, '.hdr', sep='')	
    }
    theurl <- paste('http://biogeo.ucdavis.edu/data/climate/worldclim/1_4/grid/cur/', zip, sep='')
  }
  files <- c(paste(path, bilfiles, sep=''), paste(path, hdrfiles, sep=''))
  fc <- sum(file.exists(files))
  if ( fc < length(files) ) {
    if (any(!file.exists(zipfile))) {
      if (download) {
        if(length(theurl) > 1){
          sapply(1:length(theurl), function(x){
            if (!file.exists(zipfile[x])){
              .download(theurl[x], zipfile[x])
            }  
          })
        } else{
          .download(theurl, zipfile) 
        }
        if (any(!file.exists(zipfile)))	{ 
          message("\n Could not download file -- perhaps it does not exist") 
        }
      } else {
        message("File not available locally. Use 'download = TRUE'")
      }
    }
    if(length(zipfile) > 1){
      lapply(1:length(zipfile),function(z){
        utils::unzip(zipfile[z], exdir=dirname(zipfile[z]))
      })
    } else{
      utils::unzip(zipfile, exdir=dirname(zipfile))
    }
    for (h in paste(path, hdrfiles, sep='')) {
      x <- readLines(h)
      x <- c(x[1:14], 'PIXELTYPE     SIGNEDINT', x[15:length(x)])
      writeLines(x, h)
    }
  }
  if (var  == 'alt') {
    st <- raster::raster(paste(path, bilfiles, sep=''))
    if(extent!=''){
      st <- raster::crop(x=st, y=raster::extent(extent))
    }
  } else {
    if(res == 0.5){
      if(length(rc) > 1){
        st <- lapply(rc, function(z){
          st <- raster::stack(paste(path, bilfiles[grep(bilfiles, pattern=z)], sep=''))
          if(extent!=''){
            st <- raster::crop(x=st, y=raster::extent(extent))
          }
        })
        st <- do.call(raster::merge, st)
        if(extent!=''){
          st <- raster::crop(x=st, y=raster::extent(extent))
        }
      } else{
        st <- raster::stack(paste(path, bilfiles, sep=''))
        if(extent!=''){
          st <- raster::crop(x=st, y=raster::extent(extent))
        }
      }
    } else{
      st <- raster::stack(paste(path, bilfiles, sep=''))
      if(extent!=''){
        st <- raster::crop(x=st, y=raster::extent(extent))
      }
    }
  }
  if(exists("fact")){
    # Aggregate data resolution, if res > 10 (Increases the outline of the world!)
    st <- raster::aggregate(x=st, fact=fact, fun=mean, expand=F, na.rm=T)
  }
  raster::projection(st) <- "+proj=longlat +datum=WGS84"
  return(st)
}

.raster <- function(country, name, mask=TRUE, path, download, keepzip=FALSE, ...) {
  country <- .getCountry(country)
  path <- .getDataPath(path)
  if (mask) {
    mskname <- '_msk_'
    mskpath <- 'msk_'
  } else {
    mskname<-'_'
    mskpath <- ''		
  }
  filename <- paste(path, country, mskname, name, ".grd", sep="")
  if (!file.exists(filename)) {
    zipfilename <- filename
    raster::extension(zipfilename) <- '.zip'
    if (!file.exists(zipfilename)) {
      if (download) {
        theurl <- paste("http://biogeo.ucdavis.edu/data/diva/", mskpath, name, "/", country, mskname, name, ".zip", sep="")
        .download(theurl, zipfilename)
        if (!file.exists(zipfilename))	{ 
          message("\nCould not download file -- perhaps it does not exist") 
        }
      } else {
        message("File not available locally. Use 'download = TRUE'")
      }
    }
    ff <- utils::unzip(zipfilename, exdir=dirname(zipfilename))
    if (!keepzip) {
      file.remove(zipfilename)
    }
  }	
  if (file.exists(filename)) { 
    rs <- raster::raster(filename)
  } else {
    #patrn <- paste(country, '.', mskname, name, ".grd", sep="")
    #f <- list.files(path, pattern=patrn)
    f <- ff[substr(ff, nchar(ff)-3, nchar(ff)) == '.grd']
    if (length(f)==0) {
      warning('something went wrong')
      return(NULL)
    } else if (length(f)==1) {
      rs <- raster::raster(f)
    } else {
      rs <- sapply(f, raster::raster)
      message('returning a list of RasterLayer objects')
      return(rs)
    }
  }
  raster::projection(rs) <- "+proj=longlat +datum=WGS84"
  return(rs)	
}

.SRTM <- function(lon, lat, download, path) {
  stopifnot(lon >= -180 & lon <= 180)
  stopifnot(lat >= -60 & lat <= 60)
  
  rs <- raster::raster(nrows=24, ncols=72, xmn=-180, xmx=180, ymn=-60, ymx=60 )
  rowTile <- raster::rowFromY(rs, lat)
  colTile <- raster::colFromX(rs, lon)
  if (rowTile < 10) { rowTile <- paste('0', rowTile, sep='') }
  if (colTile < 10) { colTile <- paste('0', colTile, sep='') }
  
  f <- paste('srtm_', colTile, '_', rowTile, sep="")
  zipfilename <- paste(path, "/", f, ".ZIP", sep="")
  tiffilename <- paste(path, "/", f, ".TIF", sep="")
  
  if (!file.exists(tiffilename)) {
    if (!file.exists(zipfilename)) {
      if (download) { 
        theurl <- paste("ftp://xftp.jrc.it/pub/srtmV4/tiff/", f, ".zip", sep="")
        test <- try (.download(theurl, zipfilename) , silent=TRUE)
        if (class(test) == 'try-error') {
          theurl <- paste("http://hypersphere.telascience.org/elevation/cgiar_srtm_v4/tiff/zip/", f, ".ZIP", sep="")
          test <- try (.download(theurl, zipfilename) , silent=TRUE)
          if (class(test) == 'try-error') {
            theurl <- paste("http://srtm.csi.cgiar.org/SRT-ZIP/SRTM_V41/SRTM_Data_GeoTiff/", f, ".ZIP", sep="")
            .download(theurl, zipfilename)
          }
        }
      } else {message('file not available locally, use download=TRUE') }	
    }
    if (file.exists(zipfilename)) { 
      utils::unzip(zipfilename, exdir=dirname(zipfilename))
      file.remove(zipfilename)
    }	
  }
  if (file.exists(tiffilename)) { 
    rs <- raster::raster(tiffilename)
    raster::projection(rs) <- "+proj=longlat +datum=WGS84"
    return(rs)
  } else {
    stop('file not found')
  }
}

#.biooracle <- function(path, download){
#}

#.gebco <- function(path, download){
#}

#.marspec <- function(path, download){
#}

#.ndvi3g <- function(path, download){
# see downloadNDVI.R
#}

#.emis <- function(path, download){
# Download EMIS data
# http://emis.jrc.ec.europa.eu/emis/satellite/2km/EMIS_T_SST_07_2002.nc
#}

.wdpa <- function(country='', month='', year='', path, format="shapefile", download, keepzip=FALSE, ...){
  if(month == ''){month <- lubridate::month(Sys.Date(), label=T, locale=Sys.setlocale("LC_TIME", "C"))}
  if(year == ''){year <- lubridate::year(Sys.Date())}
  path <- .getDataPath(path)
  if(country != ''){
    country <- .getCountry(country)
    filename <- paste(path, "WDPA_", month, year, "_", country, "-", format, ".zip", sep="") 
  } else{
    filename <- paste(path, "WDPA_", month, year, "-", format, ".zip", sep="") 
  }
  if (!file.exists(sub(".zip", "-polygons.shp", filename))) {
    if (!file.exists(filename)) {
      zipfilename <- filename
      raster::extension(zipfilename) <- '.zip'
      if (!file.exists(zipfilename)) {
        if (download) {
          if(country != ''){
            theurl <- paste("http://d1gam3xoknrgr2.cloudfront.net/current/WDPA_", month, year, 
                            "_", country, "-", format, ".zip", sep="")  
          } else{
            theurl <- paste("http://d1gam3xoknrgr2.cloudfront.net/current/WDPA_", month, year, 
                            "-", format, ".zip", sep="")
            # theurl <- paste0("https://pp-import-production.s3.amazonaws.com/WDPA_", month, year, "_Public", ".zip")
            # Link for downloading geodatabase
          }
          .download(theurl, zipfilename)
          if (!file.exists(zipfilename))	{ 
            message("\nCould not download file -- perhaps it does not exist") 
          }
        } else {
          message("File not available locally. Use 'download = TRUE'")
        }
      }
      ff <- utils::unzip(zipfilename, exdir=dirname(zipfilename))
      if (!keepzip) {
        file.remove(zipfilename)
      }
    }	
  } else{
    ff <- sub(".zip", "-polygons.shp", filename)
  }
  #patrn <- paste(country, '.', mskname, name, ".grd", sep="")
  #f <- list.files(path, pattern=patrn)
  f <- ff[substr(ff, nchar(ff)-3, nchar(ff)) == '.shp']
  if (length(f)==0) {
    warning('something went wrong')
    return(NULL)
  } else if (length(f)==1) {
    rs <- sf::st_read(f)
  } else {
    rs <- sapply(f, sf::st_read)
    message('returning a list of sf objects')
    return(rs)
  }
  return(rs)	
}