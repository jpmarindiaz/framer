
availableCtypes <- c("Ca","Nu","Da","Dy","Mn","Yr","Ho","Dt","Tx","Ge","Im","Au")

#' @export
availableCtypes <- function(){
  list(
    "_" = "Null",
    "Ca" = "Categorical",
    "Nu" = "Numeric",
    "Da" = "Dates",
    "Dy" = "Day",
    "Mn" = "Month",
    "Yr" = "Years",
    "Ho" = "Hours",
    "Dt" = "Datetime",
    "Tx" = "Text",
    "Ge" = "Geography",
    "Im" = "Image",
    "Au" = "Audio"
  )
}

#' @export
availableCformats <- function(){
  list(
  "Ca" = "",
  "Nu" = "",
  "Dt" = c("yyyy-mm-dd","unixTimeStamp"),
  "Ho" = "HH:MM:SS",
  "Dt" = "yyyy-mm-dd hh:mm:ss",
  "Ge" = c("code","name","latNum","lonNum"),
  "Tx" = c("plain","html","markdown"),
  "Im" = c("imageUrl","file"),
  "Au" = "audio"
  )
}

defaultCformats <- list(
  "Ca" = "",
  "Nu" = "",
  "Da" = "yyyy-mm-dd",
  "Ho" = "HH:MM:SS",
  "Dt" = "yyyy-mm-dd hh:mm:ss",
  "Tx" = "plain",
  "Ge" = "latNum",
  "Im" = "imageUrl",
  "Au" = "audio"
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
    ctype <- "Nu"
    # if(all(v %in% 1000:2200)) ctype <- "Yr"
    # if(all(v %in% 1:31)) ctype <- "Dy"
    #if(all(v %in% 1:12)) ctype <- "Mn"
    return(ctype)
  }
  dth <- whichDTH(v)
  if(!is.null(dth))
    ctype <- dth
  else{
    v <- as.character(v)
    ctype <- "Ca"
    if(ctype == "Ca" && isImgUrl(v)){
      ctype <- "Im"
    }
    if(ctype == "Ca" && isTxType(v))
      ctype <- "Tx"
  }
  ctype
}

isTxType <- function(v){
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
  defaultCformats[gc]
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
    if(ctypes[i]=="Nu"){df[,i]<- as.numeric(df[,i])}
    if(ctypes[i]=="Ca"){df[,i]<- as.character(df[,i])}
    if(ctypes[i]=="Tx"){df[,i]<- as.character(df[,i])}
    if(ctypes[i]=="Im"){
      if(!isImgUrl(df[,i])) stop ("Not an image Url")
      df[,i]<- as.character(df[,i])
      }
    if(ctypes[i]=="Dt"){df[,i]<- parseDatetime(df[,i],"Dt")}
    if(ctypes[i]=="Tm"){df[,i]<- parseDatetime(df[,i],"Tm")}
    if(ctypes[i]=="Ho"){df[,i]<- parseDatetime(df[,i],"Ho")}
    if(ctypes[i]=="Ge"){df[,i]<- as.numeric(df[,i])}
  }
  df
}

getColumnNames <- function(fringe){
  if(!isFringe(fringe)) stop('class is not Fringe')
  unlist(Map(function(i){i$name},fringe$fields))
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

#' @export
getDictionary <- function(d){
  d <- fringe(d)
  data.frame(id = getCnames(d), name = getCnames(d),ctype = getCtypes(d),stringsAsFactors = FALSE)
}


