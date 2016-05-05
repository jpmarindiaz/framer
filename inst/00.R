library(devtools)
load_all()
document()
install()
test()

# devtools::use_testthat() ## Create package testing files
library(framer)

str(iris)
frame(iris)
frame(mtcars)

## ADD TESTS
t <- frame(mtcars)
# t$asList()
# t$writeCSV()
# t$writeYAML()
writeFrame(t,"inst/data/frame/mtcars")

writeFrame(t)
f <- readFrame(file = "mtcars")

sameFrames(t,f)

f1 <- t
f2 <- f
sameFrames(f1,f2)



str(t)
str(f)

self <- t
t$data
selectFrameCols(t,1:3)


f <- frame(mtcars, cdescriptions = names(mtcars))
frameValidate(f,"allNumeric")

t <- sampleData("CCN", asFrame = FALSE)


cdescriptions <- c("","","2","daf","f")
frame(iris, cdescriptions = cdescriptions)



d <- sampleData("DXXNNNN")
guessCtypes(d)
getCaCnames(frame(d),n = 15)


t <- sampleData("CCN", asFrame = TRUE)
cnames <- c("res","sec")
setCnames(t,c("first","second"))
setCnames(t,c("first","second","third"))





## Add
# SELECTNOT COLUMNS
setdiff(names(all),c("cv_dpto","depto","mupio"))










