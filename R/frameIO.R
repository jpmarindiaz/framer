#' @export
writeFrame <- function(f,file = NULL, path = NULL){
  path <- path <- "."
  if(!isFrame(f))
    stop("not a frame")
  csv <- f$writeCSV()
  yaml <- f$writeYAML()
  paste0(csv,yaml)
}

#' @export
readFrame <- function(file = NULL, path = NULL){
  file <- file_path_sans_ext(file)
  yamlFile <- paste0(file,".yaml")
  csvFile <- paste0(file,".csv")
  d <- read.csv(csvFile,stringsAsFactors = FALSE)
  l <- list()
  if(file.exists(yamlFile))
    l <- yaml.load_file(yamlFile)
  frame(data = d,
        name = l$name,
        description = l$description,
        ctypes = l$ctypes,
        cformats = l$cformats,
        cdescriptions = l$cdescriptions)
}


