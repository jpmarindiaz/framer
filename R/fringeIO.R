#' @export
writeFringe <- function(f,file = NULL, path = NULL){
  path <- path %||% "."
  if(!isFringe(f))
    stop("not a fringe")
  csv <- f$writeCSV(file, path)
  yaml <- f$writeYAML(file, path)
  paste0(csv,yaml)
}

#' @export
readFringe <- function(file = NULL, path = NULL){
  path <- path %||% "."
  file <- file_path_sans_ext(file)
  yamlFile <- paste0(file,".yaml")
  csvFile <- paste0(file,".csv")
  d <- read.csv(file.path(path,csvFile),stringsAsFactors = FALSE)
  l <- list()
  if(file.exists(file.path(path, yamlFile)))
    l <- yaml.load_file(file.path(path,yamlFile))
  fringe(data = d,
        name = l$name,
        description = l$description,
        ctypes = l$ctypes,
        cformats = l$cformats,
        cdescriptions = l$cdescriptions)
}


