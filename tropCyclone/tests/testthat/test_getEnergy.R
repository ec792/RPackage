
test_that( "getEnergy is right", {
  
  id <- "AL081889"
  subset <- hurdat[hurdat$id == "AL081889", ]
  #note there is only 6 hour data
  expected <- sum(subset$maxWind^2) / 10^4
  expect_equal(
    getEnergy("AL081889"),
    expected
  )
  
})
  