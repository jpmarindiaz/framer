library(framer)
context("validations")

test_that("Frame validations", {
  t <- sampleData("CN", asFrame = TRUE)
  expect_true(frameValidate(t,"hasFtype","Ca-Nu"))
  expect_false(frameValidate(t,"hasCtypes",c("Ca","Nu","Nu")))
  expect_true(frameValidate(t,"hasColnames",c("a","number")))

})

test_that("Col validations", {

  t <- sampleData("DXXNNNN", asFrame = FALSE)
  frame <- frame(t)
  cols <- c("pagePathLevel1","fullReferrer","pageviews")
  ctype <- "Tx"
  expect_false(frameColValidate(frame,cols,"hasCtype",ctype))
  expect_true(frameColValidate(frame,c("pageviews","avgTimeOnPage"),"hasCtype","Nu"))

  availableSampleData()
  t <- sampleData("CN",asFrame = TRUE)
  expect_true(frameColValidate(t,2,"unique"))
  expect_false(frameColValidate(t,"a","unique"))

  # customs validator fun
  #   f <- function(datas,cols, val) {as.logical(val)}
  #   a <- colValidate(t,type = "custom",1,f, 0)
  #   b <- colValidate(t,"custom",1,f, val=1)
  #   expect_false(a)
  #   expect_true(b)
})

