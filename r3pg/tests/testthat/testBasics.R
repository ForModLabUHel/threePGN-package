context("Basic tets")
library(threePGN)

test_that("Model runs", {
  firstRun <- r3pgn(siteData = data_site[1:3,], climate = data_climate, parameters = data_param[,2], outputs = c(1:5, 10:12, 26:27))
  expect_is(firstRun, "r3pgOut")

})





