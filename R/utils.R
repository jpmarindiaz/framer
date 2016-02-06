
dfFactorsToCharacters <- function(d){
  i <- sapply(d, is.factor)
  d[i] <- lapply(d[i], as.character)
  d
}


`%||%` <- function (x, y)
{
  if (is.empty(x))
    return(y)
  else if (is.null(x) || is.na(x))
    return(y)
  else if( class(x)=="character" && nchar(x)==0 )
    return(y)
  else x
}

is.empty <- function(x){
  #   !is.null(x)
  !as.logical(length(x))
}


naToEmpty <- function(df, empty = c(" ")){
  df[is.na(df)] <- ""
  df[df %in% empty] <- ""
  df
}

