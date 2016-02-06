#' @export
frameColValidateFuns <- function(){
  colVal_funs <- as.character(lsf.str("package:framer"))
  colVal_funs <- colVal_funs[grepl("^frameColVal_",colVal_funs)]
  colVal_funs <- gsub("frameColVal_","",colVal_funs, fixed = TRUE)
  colVal_funs
}

#' frameColVal_unique
#' @name frameColVal_unique
#' @description frameColVal_unique
#' @export
frameColVal_unique <- function(frame,cols){
  data <- getDataframe(frame)
  all(!duplicated(data[cols]))
}

#' frameColVal_greaterThan0
#' @name frameColVal_greaterThan0
#' @description frameColVal_greaterThan0
#' WORKS_WITH_CTYPE: N
#' @export
frameColVal_greaterThan0 <- function(frame,cols){
  data <- getDataframe(frame)
  data <- data[cols]
  all(sapply(data,function(i) i>0))
}

#' frameColVal_hasGenderLevelsEs
#' @name frameColVal_hasGenderLevelsEs
#' @description frameColVal_hasGenderLevelsEs
#' @export
frameColVal_hasGenderLevelsEs <- function(frame,cols){
  data <- getDataframe(frame)
  f <- function(i){i %in% c("Masculino","Femenino","")}
  all(sapply(data,f))
}

#' frameColVal_different
#' @name frameColVal_different
#' @description frameColVal_different
#' @export
frameColVal_different <- function(frame,cols){
  data <- getDataframe(frame)
  l <- lapply(cols,function(c){
    length(unique(data[,c])) == length(data[,c])
  })
  all(unlist(l))
}

#' frameColVal_hasCtype
#' @name frameColVal_hasCtype
#' @description frameColVal_hasCtype
#' @export
frameColVal_hasCtype <- function(frame,cols,ctype){
  ctypes <- getCtypes(frame)
  idx <- match(cols,getCnames(frame))
  ctypes <- ctypes[idx]
  frame <- selectFrameCols(frame,cols)
  all(getCtypes(frame) %in% ctype)
}



#' @export
frameColValidate <- function(t, cols = NULL, validation, ...){
  availableValidations <- frameColValidateFuns()
  if(!validation %in% availableValidations)
    stop("no validation with that name")
  if(!isFrame(t)) stop("must be a frame")
  cols <- cols %||% getCnames(t)
  if(class(cols) %in% c("numeric","integer"))
    cols <- getCnames(t)[cols]
  if(!all(cols %in% getCnames(t)))
    stop('cols not in frame')
  args <- list(...)
  fun <- paste0("frameColVal_",validation)
  p <- c(list(t,cols),args)
  do.call(fun,p)
}


