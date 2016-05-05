#' @export
frame<- function(data, ctypes=NULL,cformats=NULL,
                 cdescriptions=NULL,
                   name = NULL, description = NULL,recordName = NULL){
  if(isFrame(data)){
    warning("data is already a Frame")
    return(data)
  }
  # if(nrow(data) >0)
  #rownames(data) <- NULL
  frame <- Frame$new(data,
                    ctypes=ctypes,
                    cformats=cformats,
                    cdescriptions = cdescriptions,
                    name = name %||% deparse(substitute(data)),
                    description = description,
                    recordName = recordName)
  frame
}

#' @export
frameske<- function(ctypes=NULL,cformats=NULL,cnames = NULL,
               name = NULL, description = NULL,
               validators = NULL, sampleData = NULL, useCnames = TRUE){
  if(!is.null(sampleData))
    name <- name %||% deparse(substitute(sampleData))
  frameske <- FrameSke$new(
                 ctypes=ctypes,
                 cformats=cformats,
                 cnames=cnames,
                 name = name,
                 description = description,
                 validators = validators,
                 sampleData = sampleData,
                 useCnames = useCnames)
  frameske
}


#' @export
isFrame <- function(d){
  "Frame" %in% class(d)
}

#' @export
sameFrames <- function(f1,f2){
  all(
    identical(getCnames(f1),getCnames(f2)),
    identical(getCtypes(f1),getCtypes(f2)),
    identical(getCformats(f1),getCformats(f2)),
    identical(f1$d,f2$d)
  )
}

#' @export
getDataframe <- function(frame, withNames = TRUE){
  if(!isFrame(frame)) stop('class is not Frame')
  if(withNames) return(frame$data)
  else return(frame$d)
}

#' @export
getCnames <- function(frame){
  if(!isFrame(frame)) stop('class is not Frame')
  unlist(Map(function(i){i$name},frame$fields))
}

#' @export
getCtypes <- function(frame, cols = NULL){
  if(!isFrame(frame)) stop("Not a Frame")
  cols <- cols %||% getCnames(frame)
  l <- Map(function(i){i[["ctype"]]},frame$fields)
  names(l) <- Map(function(i){i[["name"]]},frame$fields)
  out <- l[cols]
  unname(unlist(out))
}

#' @export
getCformats <- function(frame){
  if(!isFrame(frame)) stop("Not a Frame")
  frame$getCformats()
}


#' @export
getCaCnames <- function(frame, n = 4){
  d <- getDataframe(frame)
  nvals <- sapply(d,function(c)length(unique(c)))
  names(nvals[nvals <= n])
}

#' @export
getFtype <- function(frame){
  if(!isFrame(frame)) stop('class is not Frame')
  frame$ftype
}




#' @export
frameHasFrameSkeleton <- function(frame,frameSke){
  # Check ctypes and cnames
  cframe <- getCnames(frame)
  names(cframe) <- getCtypes(frame)
  cske <- frameSke$cnames
  names(cske) <- frameSke$ctypes
  ctypesCnamesCheck <- identical(cframe,cske)

  # Check validators
  validators <- frameSke$validators
  if(paste(validators,collapse="") != ""){
    validatorsTmp <- lapply(validators,function(v){strsplit(v,":",fixed=TRUE)[[1]]})
    validatorCheck <- lapply(validatorsTmp,function(v){
      cols <- strsplit(v[-1],"|",fixed=TRUE)[[1]]
      type <- v[1]
      colValidate(frame,type = type,cols = cols)
    })
    validatorCheck <- all(unlist(validatorCheck))
  } else{
    validatorCheck <- TRUE
  }
  # Return validations
  ctypesCnamesCheck && validatorCheck
}


#' @export
validValidators <- function(validators){
  #validators <- c("frameColVal_greaterThan0:fdsafs","frameColVal_unique:fdsafds")
  if(length(validators) == 1 && validators == "") return(TRUE)
  v <- strsplit(validators,":")
  v <- Map(function(i){i[[1]]},v)
  framevalf <- paste0("frameVal_",frameValidateFuns())
  colvalf <- paste0("frameColVal_",frameColValidateFuns())
  all(v %in% c(framevalf,colvalf))
}

#' @export
selectColumns <- function(frame,fields){
  if(!all(fields %in% getColumnNames(frame))) stop("Columns not in this frame")
  frame(frame$data[fields],name = frame$name, description = frame$description)
}

#' @export
selectFrameCols <- function(frameIn,cols){
  if(!class(frameIn)[1] %in% c("Frame","data.frame"))
    stop("frame must be either a Frame of a data.frame")
  if(!isFrame(frameIn)) frame <- frame(frameIn)
  else frame <- frameIn
  if(class(cols) %in% c("numeric","integer"))
    cols <- getCnames(frame)[cols]
  if(! all(cols %in% getCnames(frame)))
    stop("cols not in frame")
  d <- getDataframe(frame)
  out <- d[cols]
  if(isFrame(frameIn)) return(frame(out))
  out
}

#' @export
setCnames <- function(t,cnames, idx = NULL){
  if(!isFrame(t))
    stop("frame must be a Frame")
  t$setCnames(cnames,idx = idx)
  t
}



