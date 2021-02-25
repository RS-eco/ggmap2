#' Download ndvi3g data
#'
#' Function to download AVHRR GIMMS ndvi3g data for a specific timeperiod of your choice.
#' Data is available globally and for the timeperiod 1981 - 2015.
#'
#' @param startyear A year between 1981 and 2015.
#' @param endyear A year between 1981 and 2015, should be larger than startyear.
#' @param version One of "v0" or "v1", otherwise function automatically uses "v1".
#' @param path Path where data is downloaded to. If not available a subfolder with the version name will be automatically created.
#' @return The function just downloads the files to the specified path.
#' @examples
#' \dontrun{
#' downloadNDVI(startyear=1981, endyear=2013, version="v0")
#' downloadNDVI(startyear=1981, endyear=2015, version="v1")
#' }
#' @export
downloadNDVI <- function(startyear=1981, endyear=2013, version="v0", path=getwd()){
  '%!in%' <- function(x,y)!('%in%'(x,y))
  if(startyear < 1981 | startyear > 2015){startyear <- 1981}
  if(endyear < startyear | endyear > 2015){endyear <- 2015}
  if(endyear > 2013){version == "v1"}
  if(version %!in% c("v0", "v1")){version == "v1"}
  if(!dir.exists(paste0(path, "/3g.", version))){dir.create(paste0(path, "/3g.", version))}
  #try(file_list <- read.csv(paste0("http://ecocast.arc.nasa.gov/data/pub/gimms/3g.", version, "/00FILE-LIST.txt"), header=F))
  file_list <- read.csv(list.files(paste0(path, "/3g.", version, "/"), pattern="00FILE-LIST.txt", recursive=T, full.names=T), header=F)
  file_list$years <- sapply(file_list$V1, function(x){strsplit(basename(as.character(x)), split="_")[[1]][4]})
  file_list <- file_list[file_list$years %in% seq(startyear, endyear),]
  lapply(file_list$V1, function(x){
    if(!file.exists(paste0(path, "/3g.", version, "/", basename(as.character(x))))){
      download.file(url=as.character(x), destfile=paste0(path, "/3g.", version, "/", basename(as.character(x))))
    }
  })
  return(NULL)
}
