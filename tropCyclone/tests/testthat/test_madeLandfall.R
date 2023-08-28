
test_that("madeLandfall is ok",{
  
  #id of Hurricane Katrina (2005) is
  id <- "AL122005"
  expect_equal(
    madeLandfall(id),
    TRUE
  )
  
})
