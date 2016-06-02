library(devtools)
load_all()
document()
install()
test()

# devtools::use_testthat() ## Create package testing files
library(fringer)

str(iris)
fringe(iris)
fringe(mtcars)

## ADD TESTS
t <- fringe(mtcars, cdescriptions = 1:11)
# t$asList()
# t$writeCSV()
# t$writeYAML()
writeFringe(t,"mtcars")

f <- readFringe(file = "mtcars")

sameFringes(t,f)

f1 <- t
f2 <- f
sameFringes(f1,f2)



str(t)
str(f)

self <- t
t$data
selectFringeCols(t,1:3)


f <- fringe(mtcars, cdescriptions = names(mtcars))
fringeValidate(f,"allNumeric")

t <- sampleData("CCN", asFringe = FALSE)


cdescriptions <- c("","","2","daf","f")
fringe(iris, cdescriptions = cdescriptions)



d <- sampleData("DXXNNNN")
guessCtypes(d)
getCaCnames(fringe(d),n = 15)


t <- sampleData("CCN", asFringe = TRUE)
cnames <- c("res","sec")
setCnames(t,c("first","second"))
setCnames(t,c("first","second","third"))





## Add
# SELECTNOT COLUMNS
setdiff(names(all),c("cv_dpto","depto","mupio"))










