
#' @export
frameValidateFuns <- function(){
  frameVal_funs <- as.character(lsf.str("package:framer"))
  frameVal_funs <- frameVal_funs[grepl("^frameVal_",frameVal_funs)]
  gsub("frameVal_","",frameVal_funs, fixed = TRUE)
}


#' frameVal_hasFtype
#' @name frameVal_hasFtype
#' @description frameVal_hasFtype
#' @export
frameVal_hasFtype <- function(frame,ftype){
  if(missing(ftype)) stop("need ftype as a parameter")
  identical(ftype,getFtype(frame))
}

#' frameVal_hasAnyFtype
#' @name frameVal_hasAnyFtype
#' @description frameVal_hasAnyFtype
#' @export
frameVal_hasAnyFtype <- function(frame,ftype){
  if(missing(ftype)) stop("need ftype as a parameter")
  any(getFtype(frame) %in% ftype)
}

#' frameVal_hasCtypes
#' @name frameVal_hasCtypes
#' @description frameVal_hasCtypes
#' @export
frameVal_hasCtypes <- function(frame,ctypes){
  if(missing(ctypes)) stop("need ctypes as a parameter")
  identical(getCtypes(frame),ctypes)
}

#' frameVal_hasColnames
#' @name frameVal_hasColnames
#' @description frameVal_hasColnames
#' @export
frameVal_hasColnames <- function(frame,cnames){
  if(missing(cnames)) stop("need cnames as a parameter")
  identical(getCnames(frame),cnames)
}

#' frameVal_colnamesInFrame
#' @name frameVal_colnamesInFrame
#' @description frameVal_colnamesInFrame
#' @export
frameVal_colnamesInFrame <- function(frame,cols){
  if(missing(cols)) stop("need cnames as a parameter")
  cols %in% getCnames(frame)
}


#' @export
frameValidate <- function(t, validation, ...){
  if(!isFrame(t)) stop("must be a frame")
  args <- list(...)
  availableValidations <- frameValidateFuns()
  if(!validation %in% availableValidations)
    stop("no validation with that name")
  fun <- paste0("frameVal_",validation)
  do.call(fun,c(t,list(...)))
}








