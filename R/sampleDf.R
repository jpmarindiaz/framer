#' Creates a new Dataframe from a dataframe
#' @name sampleData
#' @description Creates a new datapackage from json, data frame, list of data frames, list of data framees (see Dataframe reference class)
#' @param d might be a json string, data frame or list of data frames.
#' @return dp
#' @export
#' @examples \dontrun{
#' frame <- newDataframeFromDataframe(mtcars)
#' }
sampleData <- function(ftype, asFrame=FALSE){
  dir <- system.file("sampledata",package="framer", mustWork=TRUE)
  if(!ftype %in% availableSampleData()){
    stop("No data for this ftype")
    }
  else{
    filename <- paste0("data",ftype,".csv")
    out <- read.csv(file.path(dir,filename), stringsAsFactors=FALSE)
  }
  if(asFrame){out <- frame(out)}
  out
}


#' @export
#'
availableSampleData <- function(){
  dir <- system.file("sampledata",package="framer", mustWork=TRUE)
  files <- list.files(dir)
  files <- files[grepl("^data.*csv$",files)]
  x <- gsub("data","",files)
  x <- gsub(".csv","",x)
  x
}


