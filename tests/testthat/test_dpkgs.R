# library(framer)
# context("Frame properties")
# test_that("properties", {
#   df <- data.frame(x = runif(5), y = runif(5))
#   c <- framepkg(df)
#   expect_is(c,"Framepkg")
#
#   nms <- c("N","N")
#   #names(nms) <- names(df)
#   expect_equal(guessCtypes(df),nms)
#
#   cols <- "dist"
#   expect_equal(guessCtypes(df),getCtypes(c))
#   expect_equal(letters[1:2],names(getDataframe(c, withNames=FALSE)))
# })
