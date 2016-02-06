library(devtools)
load_all()
document()
install()
# devtools::use_testthat() ## Create package testing files
#library(framer)


##
t <- frame(mtcars)
selectFrameCols(t,1:3)



d <- sampleData("DXXNNNN")
guessCtypes(d)
getCaCnames(frame(d),n = 15)


t <- sampleData("CCN", asFrame = TRUE)
cnames <- c("res","sec")
setCnames(t,c("first","second"))
setCnames(t,c("first","second","third"))
















