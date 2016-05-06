context("frames")

test_that("Create Frame", {
  #test empty framees
  void = data.frame(col1 = character(0), col2 = character(0))
  expect_equal(guessCtype(void[1]),"_")
  expect_equal(guessCtypes(void),c("_","_"))
  tv <- frame(void)
  expect_equal(tv$data,void)

  # test frames with only one column
  d <- data.frame(LET = letters, stringsAsFactors = FALSE)
  frame <- frame(d)
  expect_equal(frame$data,d)
  frame <- frame(iris)
  expect_equal("iris",frame$name)
  iris2 <- dfFactorsToCharacters(iris)
  expect_equal(frame$data, iris2)
  df <- sampleData('CN', asFrame = TRUE)
  expect_equal(getCtypes(df),c('Ca','Nu'))
  expect_equal(getCnames(df),c('a','number'))
  #expect_equal(getCformats(df),c('','')) ## OJO FORMATS

  t <- sampleData("CCN", asFrame = TRUE)
  cnames <- c("res","sec")
  t$setCnames(cnames, idx = c(3,1))
  expect_equal(getCnames(t),c('sec','category2','res'))
  cnames = c("a","v","vd")
  t$setCnames(cnames)
  expect_equal(getCnames(setCnames(t,cnames)),getCnames(t))
  expect_error(t$setCnames(c("res","res")))
  expect_error(setCnames(t,c("first","second")))

  t <- sampleData("CCN", asFrame = TRUE)
  cdescriptions <- c("res","sec")
  t$setCdescriptions(cdescriptions, idx = c(3,1))
  expect_equal(getCdescriptions(t),c('sec','','res'))
  cdescriptions = c("a","v","vd")
  t$setCdescriptions(cdescriptions)
  expect_equal(getCdescriptions(setCdescriptions(t,cdescriptions)),getCdescriptions(t))
  expect_error(t$setCdescriptions(c("res","res")))
  expect_error(setCdescriptions(t,c("first","second")))
})



test_that("Sample Data", {
  t <- sampleData("CN", asFrame = TRUE)
  expect_equal(getFtype(t),"Ca-Nu")
  expect_error(sampleData("XXXXXX", asFrame = TRUE))
})


test_that("frameValidations", {
  t <- sampleData("CN",asFrame = TRUE)
  expect_true(frameValidate(t,"hasCtypes",c("Ca","Nu")))
  expect_true(frameValidate(t,"hasFtype","Ca-Nu"))
  expect_false(frameValidate(t,"hasAnyFtype",c("Ca-Ca-Nu","Nu-Nu","Nu-Im")))
  expect_true(frameValidate(t,"hasColnames",c("a","number")))

  t <- sampleData("CN",asFrame = TRUE)
  frameValidate(t,"colnamesInFrame","a")
  expect_false(frameColValidate(t,1,"unique"))
  expect_false(frameColValidate(t,"a","unique"))
  expect_true(frameColValidate(t,"number","unique"))
})

test_that("frameIO",{
  t <- frame(mtcars)
  writeFrame(t)
  f <- readFrame(file = "mtcars")
  unlink("mtcars.csv")
  unlink("mtcars.yaml")
  expect_true(sameFrames(t,f))
  # write
  filename <- "sampleFrame"
  tmpDir <- tempdir()
  writeFrame(t,file = filename, path = tmpDir)
  f2 <- readFrame(file = filename, path=tmpDir)
  expect_true(sameFrames(t,f2))
  expect_true(file.exists(file.path(tmpDir,paste0(filename,".csv"))))
  expect_true(file.exists(file.path(tmpDir,paste0(filename,".yaml"))))
  unlink(file.path(tmpDir,paste0(filename,".csv")))
  unlink(file.path(tmpDir,paste0(filename,".yaml")))
})



