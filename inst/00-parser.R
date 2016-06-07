library(devtools)
load_all()


library(fringer)

# http://kevinushey.github.io/blog/2016/02/12/top-down-operator-precedence-parsing-with-r/
#http://eli.thegreenplace.net/2010/01/02/top-down-operator-precedence-parsing

tokens <- c()
index <- 0

tokenize <- function(program) {
  tokens <<- unlist(strsplit(program, "\\s+"))
  index <<- 1
  tokens
}
current <- function() {
  tokens[index]
}
consume <- function() {
  token <- current()
  index <<- index + 1
  token
}


binaryPrecedence <- function(token) {
  #switch(token, "+" = 10, "*" = 20, 0)
  if(token %in% c("==","in","notIn",">","<","<=",">="))
    return(10)
  if(token %in% c("select","allUnique","uniques","nrow"))
    return(20)
  return(0)
}


parseExpressionStart <- function(d=NULL) {
  #force(d)
  token <- consume()
  message("Token Start",token)
  l <- list(
    "select" = function(d) dplyr::select_(),
    "allUnique" = function(d) nrow(d) == nrow(unique(d)),
    "uniques" = function(d) nrow(unique(d)),
    "nrow" = function(d) nrow(d),
    "cnames" = function(d) getCnames(d),
    "ctypes" = function(d) guessCtypes(d)
  )
  if(token %in% names(l))
     return(l[[token]](d))
  return(as.numeric(token))
}

parseExpressionContinuation <- function(node, d = NULL) {
  token <- consume()
  message("TOKEN PARSE EXPRESSION CONT: ",token)
  call(token,node,
       parseExpression(
         precedence = binaryPrecedence(token),
         d = d)
       )
}

parseExpression <- function(precedence = 0,d = NULL) {
  str(d)
  node <- parseExpressionStart(d)
  str(node)
  message("NODE: ",node)
  while (precedence < binaryPrecedence(current()))
    node <- parseExpressionContinuation(node, d = d)
  node
}

# Our entry-point for parsing programs.
parse <- function(program,d) {
  tokens <<- tokenize(program)
  index <<- 1
  parseExpression(d = d)
}

d <- cars


eval(parse("uniques",d))
eval(parse("allUnique",d))
eval(parse("uniques > 1",d))
parse("uniques > 1",d)
parse("uniques",d)
parse("cnames",d)
parse("ctypes",d)






vals <- list(
  nrows = "nrow > 1",
  uniques =  "uniques > 1",
  firstColAllUnique = "select(1) %>% allUnique",
  fixedColNames = "cnames == ['firstName','lastName','*']",
  colNamesIn = "cnames in ['one','two','three'] ",
  colNamesIn = "cnames notIn ['one','two','three'] ",
  hasFtype = "ftype == 'CaCaNu' ",
  hasCaFtype = "Ca in ftype",
  hasCtypes = "ctypes == 'NuCaNu'",
  allNumeric = "allCtypes == 'Nu'",
  nrowGreaterTahn = "nrow > 10",
  twoColsCombinationsAllUnique = "select('n1','n2') %>% allUnique"
)
program <- vals$nrows
asp <- parse(program, d)
eval(asp)



tokenize(vals$nrows)
tokenize(vals$uniques)
tokenize(vals$fixedColNames)
tokenize(vals$twoColsCombinationsAllUnique)
parserOperators <- c("select","allUnique","==","in","notIn",
                     ">","<","<=",">=")


tokens <- c()
index <- 0

# Run it!
program <- "1 + 2 * 3 + 4 * 5 + 6"
ast <- parse(program)
ast
