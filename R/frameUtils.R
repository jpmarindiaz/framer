
availableCtypes <- c("C","N","D","H","T","X","G","I","A")

#' @export
availableCtypes <- function(){
  list(
    "_" = "Null",
    "C" = "Categorical",
    "N" = "Numeric",
    "D" = "Dates",
    "H" = "Hours",
    "T" = "Datetime",
    "X" = "Text",
    "G" = "Geography",
    "I" = "Image",
    "A" = "Audio"
  )
}

#' @export
availableCformats <- function(){
  list(
  "C" = "",
  "N" = "",
  "D" = c("yyyy-mm-dd","unixTimeStamp"),
  "H" = "HH:MM:SS",
  "T" = "yyyy-mm-dd hh:mm:ss",
  "G" = c("latNum","lngNum"),
  "X" = c("plain","html","markdown"),
  "I" = c("imageUrl","file"),
  "A" = "audio"
  )
}

defaultCformats <- list(
  "C" = "",
  "N" = "",
  "D" = "yyyy-mm-dd",
  "H" = "HH:MM:SS",
  "T" = "yyyy-mm-dd hh:mm:ss",
  "X" = "plain",
  "G" = "latNum",
  "I" = "imageUrl",
  "A" = "audio"
)

isImgUrl <- function(x) all(grepl("([^http\\s]+(\\.(?i)(jpg|png|gif|bmp|svg))$)",x))

getDefaultCformats <- function(ctypes){
  l <- lapply(ctypes, function(ctype){
    defaultCformats[[ctype]]
  })
  unlist(l)
}

guessCtype <- function(v){
  v <- v[!is.na(v)]
  if(length(v) == 0)
    return("_")
  if(class(v) %in% c("integer","numeric")){
    ctype <- "N"
    return(ctype)
  }
  dth <- whichDTH(v)
  if(!is.null(dth))
    ctype <- dth
  else{
    v <- as.character(v)
    ctype <- "C"
    if(ctype == "C" && isImgUrl(v)){
      ctype <- "I"
    }

    if(ctype == "C" && isXtype(v))
      ctype <- "X"
  }
  ctype
}

isXtype <- function(v){
  #nwords <- function(x) vapply(strsplit(x, "\\W+"), length, integer(1))
  nwords <- function(x) vapply(strsplit(x, "[[:punct:] [^/]]"), length, integer(1))
  any(nchar(v) > 100) && any(nwords(v)>10)
}


#' @export
guessCtypes <- function(df){
  df <- removeRowAllNA(df)
  l <- lapply(df,guessCtype)
  unname(unlist(l))
}

guessCformats <- function(df){
  gc <- guessCtypes(df)
  idx <- match(gc,names(defaultCformats))

  unname(unlist(defaultCformats[idx]))
}


#' @export
guessFtype <- function(df){
  s <- guessCtypes(df)
  s <- sort(s)
  paste(s,collapse="")
}

forceCtypes <- function(df, ctypes, cformat = NULL){
  if(ncol(df)!= length(ctypes)) stop("number of df cols must be the same as col types length")
  for (i in seq_along(ctypes)){
    if(ctypes[i]=="N"){df[,i]<- as.numeric(df[,i])}
    if(ctypes[i]=="C"){df[,i]<- as.character(df[,i])}
    if(ctypes[i]=="X"){df[,i]<- as.character(df[,i])}
    if(ctypes[i]=="I"){
      if(!isImgUrl(df[,i])) stop ("Not an image Url")
      df[,i]<- as.character(df[,i])
      }
    if(ctypes[i]=="D"){df[,i]<- parseDatetime(df[,i],"D")}
    if(ctypes[i]=="T"){df[,i]<- parseDatetime(df[,i],"T")}
    if(ctypes[i]=="H"){df[,i]<- parseDatetime(df[,i],"H")}
    if(ctypes[i]=="G"){df[,i]<- as.numeric(df[,i])}
  }
  df
}

getColumnNames <- function(frame){
  if(!isFrame(frame)) stop('class is not Frame')
  unlist(Map(function(i){i$name},frame$fields))
}

removeRowAllNA <- function(d){
  idx <- !is.na(d)
  idx <- rowSums(idx) > 0
  if(ncol(d)==1){
    out <- data.frame(d[idx,])
    names(out) <- names(d)
    return(out)
  }
  d[idx,]
}

