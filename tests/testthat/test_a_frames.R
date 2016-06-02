context("fringes")

test_that("Create Fringe", {
  #test empty fringees
  void = data.frame(col1 = character(0), col2 = character(0))
  expect_equal(guessCtype(void[1]),"_")
  expect_equal(guessCtypes(void),c("_","_"))
  tv <- fringe(void)
  expect_equal(tv$data,void)

  # test fringes with only one column
  d <- data.frame(LET = letters, stringsAsFactors = FALSE)
  fringe <- fringe(d)
  expect_equal(fringe$data,d)
  fringe <- fringe(iris)
  expect_equal("iris",fringe$name)
  iris2 <- dfFactorsToCharacters(iris)
  expect_equal(fringe$data, iris2)
  df <- sampleData('CN', asFringe = TRUE)
  expect_equal(getCtypes(df),c('Ca','Nu'))
  expect_equal(getCnames(df),c('a','number'))
  #expect_equal(getCformats(df),c('','')) ## OJO FORMATS

  t <- sampleData("CCN", asFringe = TRUE)
  cnames <- c("res","sec")
  t$setCnames(cnames, idx = c(3,1))
  expect_equal(getCnames(t),c('sec','category2','res'))
  cnames = c("a","v","vd")
  t$setCnames(cnames)
  expect_equal(getCnames(setCnames(t,cnames)),getCnames(t))
  expect_error(t$setCnames(c("res","res")))
  expect_error(setCnames(t,c("first","second")))

  t <- sampleData("CCN", asFringe = TRUE)
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
  t <- sampleData("CN", asFringe = TRUE)
  expect_equal(getFtype(t),"Ca-Nu")
  expect_error(sampleData("XXXXXX", asFringe = TRUE))
})


test_that("fringeValidations", {
  t <- sampleData("CN",asFringe = TRUE)
  expect_true(fringeValidate(t,"hasCtypes",c("Ca","Nu")))
  expect_true(fringeValidate(t,"hasFtype","Ca-Nu"))
  expect_false(fringeValidate(t,"hasAnyFtype",c("Ca-Ca-Nu","Nu-Nu","Nu-Im")))
  expect_true(fringeValidate(t,"hasColnames",c("a","number")))

  t <- sampleData("CN",asFringe = TRUE)
  fringeValidate(t,"colnamesInFringe","a")
  expect_false(fringeColValidate(t,1,"unique"))
  expect_false(fringeColValidate(t,"a","unique"))
  expect_true(fringeColValidate(t,"number","unique"))
})

test_that("fringeIO",{
  t <- fringe(mtcars)
  writeFringe(t)
  f <- readFringe(file = "mtcars")
  unlink("mtcars.csv")
  unlink("mtcars.yaml")
  expect_true(sameFringes(t,f))
  # write
  filename <- "sampleFringe"
  tmpDir <- tempdir()
  writeFringe(t,file = filename, path = tmpDir)
  f2 <- readFringe(file = filename, path=tmpDir)
  expect_true(sameFringes(t,f2))
  expect_true(file.exists(file.path(tmpDir,paste0(filename,".csv"))))
  expect_true(file.exists(file.path(tmpDir,paste0(filename,".yaml"))))
  unlink(file.path(tmpDir,paste0(filename,".csv")))
  unlink(file.path(tmpDir,paste0(filename,".yaml")))
})



