#' @export Frame
#' @exportClass Frame
Frame <- R6Class("Frame",
               public = list(
                 name = NA,
                 description = NA,
                 recordName = NA,
                 ftype = NA,
                 fields = NA,
                 d = NA,
                 initialize = function(d,
                                       ctypes = NULL,
                                       cformats = NULL,
                                       name = NULL,
                                       description = NULL,
                                       recordName = NULL
                 ) {
                   if(missing(d)) stop("Need a dataframe")
                   ctypes <- ctypes %||% guessCtypes(d)
                   cformats <- cformats %||% guessCformats(d)

                   self$name <- name %||% deparse(substitute(d))
                   self$description <- description  %||% ""
                   self$recordName <- recordName %||% "observation"
                   fieldNames <- names(d)
                   fieldList <- lapply(seq_along(fieldNames), function(i,ctypes,cformats){
                     #message(ctypes[i], "_",cformats[i],"_")
                     Column$new(name=fieldNames[i],ctype=ctypes[i],cformat=cformats[i])
                   },ctypes = ctypes, cformats = cformats)
                   self$fields <- fieldList
                   self$ftype <- paste0(sort(self$getCtypes()),collapse = "")
                   d <- removeRowAllNA(d)
                   d <- naToEmpty(d)
                   fd <- forceCtypes(as.data.frame(d), ctypes)
                   names(fd) <- letters[1:ncol(fd)]
                   self$d <- fd
                   self$validate()
                 },
                 validate = function(){
                   if(length(self$getCnames()) > length(unique(self$getCnames())) )
                     stop("cnames must be unique")
                 },
                 getCnames = function(){
                   unlist(Map(function(i){i$name},self$fields))
                 },
                 getCtypes = function(){
                   unlist(Map(function(i){i$ctype},self$fields))
                 },
                 getCformats = function(){
                   unlist(Map(function(i){i$cformat},self$fields))
                 },
                 setCnames = function(cnames, idx = NULL){
                   originalCnames <- self$getCnames()
                   if(length(cnames)!= length(originalCnames) && is.null(idx))
                     stop("cnames must be the same length as original")
                   idx <- idx %||% seq_along(originalCnames)
                   j <- 1
                   originalCnames[idx] <- cnames
                   newNames <- originalCnames
                   lapply(seq_along(newNames), function(i){
                     self$fields[[i]]$name <- newNames[i]
                   })
                   names(self$d) <- self$getCnames()
                   self$validate()
                 },
                 print = function(...) {
                   p <- paste0(
                     "<Frame>",
                     "\nname: ", self$name,
                     "\ndescription: ", self$description,
                     "\ncnames: ", paste(paste0('"',self$getCnames(),'"'),collapse=", "),
                     "\nctypes: ", paste(paste0('"',self$getCtypes(),'"'),collapse=", "),
                     "\ncformats: ", paste(paste0('"',self$getCformats(),'"'),collapse=", "),
                     "\nftype: ", self$ftype,
                     "\nnrows: ", nrow(self$data),
                     "\ndata:\n ", paste(capture.output(head(self$data,4)),collapse="\n")
                   )
                   cat(p)
                   invisible(self)
                 }
               ),
               active = list(
                 data = function(){
                   d <- self$d
                   names(d) <- self$getCnames()
                   row.names(d) <- NULL
                   d
                 }
               )
)

