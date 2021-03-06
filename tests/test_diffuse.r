

# unit testing for my functions

rm(list=ls())

library(testthat)

library(devtools)
devtools::load_all()
library(kom)

data(hortVillage)

popdata <- hortVillage

popdata$people$mm_pid <- popdata$people$m_pid[match(popdata$people$m_pid, popdata$people$pid)]
popdata$people$mf_pid <- popdata$people$f_pid[match(popdata$people$m_pid, popdata$people$pid)]
popdata$people$fm_pid <- popdata$people$m_pid[match(popdata$people$f_pid, popdata$people$pid)]
popdata$people$ff_pid <- popdata$people$f_pid[match(popdata$people$f_pid, popdata$people$pid)]

popdata$people$male[is.na(popdata$people$male)] <- 1
popdata$people$age <- 0 - popdata$people$dob

popdata$people$wealth <- rnorm(nrow(popdata$people), 0, 10) 
# distribution can be varied

popdata$households$ray_coord <- haversine(popdata$households$lat, 
  popdata$villages$lat[match(popdata$households$village, popdata$villages$village)], 
  popdata$households$long, popdata$villages$long[match(popdata$households$village, popdata$villages$village)])
popdata$households$x_coord <- cartesian_map(popdata$households$lat, 
  popdata$villages$lat[match(popdata$households$village, popdata$villages$village)], 
  popdata$households$long, popdata$villages$long[match(popdata$households$village, popdata$villages$village)])$x
popdata$households$y_coord <- cartesian_map(popdata$households$lat, 
  popdata$villages$lat[match(popdata$households$village, popdata$villages$village)], 
  popdata$households$long, popdata$villages$long[match(popdata$households$village, popdata$villages$village)])$y

parameter_list <- list(
  baseline_probability = 1e-05,
  kin_network_effect = 0,
  neighbor_effect = 0,
  wealth_effect = 0
)

# it should chcek at that all variables exist at first

# normal sim
test_that("normal diffusion simulation", {
  x <- diffuse( parameter_list, popdata )
  expect_true(nrow(x) > 0)
})

test_that("just one day, with yearly obs rate", {
  x <- diffuse( parameter_list, n_years = 1/365, census_period = 365, popdata )
  expect_equal( length(unique(x$date)), 1 )
})

test_that( "just one day!", {
  x <- diffuse( parameter_list, popdata, census_period=1, n_years=1/365, cognition="additive" )
  expect_equal( length(unique(x$date)), 1 )
})

test_that( "daily censuses for 100 days", {
  history <- diffuse( parameter_list, census_period=1, n_year=100/365, popdata, cognition="additive" )
  expect_equal( length(unique(history$date)), 100 )
})



# exactly 2 censuses

test_that( "exactly 2 censuses, 10 days apart", {
  history <- diffuse( parameter_list, n_years=10/365, census_period=10, popdata )
  expect_equal( length(unique(history$date)), 2 )
})

test_that( "exactly 2 censuses, 50 days apart", {
  history <- diffuse( parameter_list, n_years=50/365, census_period=50, popdata )
  expect_equal( length(unique(history$date)), 2 )
})

test_that( "exactly 2 censuses, 1 year apart", {
  history <- diffuse( parameter_list, n_years=1, census_period=365, popdata )
  expect_equal( length(unique(history$date)), 2 )
})

test_that( "exactly 5 censuses in 5 days", {
  history <- diffuse( parameter_list, n_years=5/365, census_period=1, popdata )
  expect_equal( length(unique(history$date)), 5 )
})

test_that( "exactly 5 censuses in 20 days", {
  history <- diffuse( parameter_list, n_years=20/365, census_period=5, popdata )
  expect_equal( length(unique(history$date)), 5 )
})

test_that( "exactly 10 censuses in 90 days", {
  history <- diffuse( parameter_list, n_years=90/365, census_period=10, popdata )
  expect_equal( length(unique(history$date)), 10 )
})

test_that("not a valid cognitive model", {
  expect_error(diffuse( parameter_list, popdata, cognition="additive2"))
})



test_that( "neighbor_has works", {

  popdata <- hortVillage

  popdata$people$mm_pid <- popdata$people$m_pid[match(popdata$people$m_pid, popdata$people$pid)]
  popdata$people$mf_pid <- popdata$people$f_pid[match(popdata$people$m_pid, popdata$people$pid)]
  popdata$people$fm_pid <- popdata$people$m_pid[match(popdata$people$f_pid, popdata$people$pid)]
  popdata$people$ff_pid <- popdata$people$f_pid[match(popdata$people$f_pid, popdata$people$pid)]

  popdata$people$male[is.na(popdata$people$male)] <- 1
  popdata$people$age <- 0 - popdata$people$dob

  popdata$people$wealth <- rnorm(nrow(popdata$people), 0, 10) 
  # distribution can be varied

  popdata$villages$log_dist_town <- log(popdata$villages$dist_town)

  popdata$households$ray_coord <- haversine(popdata$households$lat, 
    popdata$villages$lat[match(popdata$households$village, popdata$villages$village)], 
    popdata$households$long, popdata$villages$long[match(popdata$households$village, popdata$villages$village)])
  popdata$households$x_coord <- cartesian_map(popdata$households$lat, 
    popdata$villages$lat[match(popdata$households$village, popdata$villages$village)], 
    popdata$households$long, popdata$villages$long[match(popdata$households$village, popdata$villages$village)])$x
  popdata$households$y_coord <- cartesian_map(popdata$households$lat, 
    popdata$villages$lat[match(popdata$households$village, popdata$villages$village)], 
    popdata$households$long, popdata$villages$long[match(popdata$households$village, popdata$villages$village)])$y

  # simulation

  parameter_list <- list(
    baseline_probability = 1e-05,
    kin_network_effect = 3,
    neighbor_effect = 3,
    wealth_effect = 0
  )

  # neighbor_radius is giving warnings but thats okay...at least it is running!

  history <- diffuse( parameter_list, popdata, cognition="additive" )

  expect_true( nrow(history) > 0 )

})





test_that( "this works", {

  library(kom)

  data(hortVillage)

  hortVillage$people$mm_pid <- hortVillage$people$m_pid[match(hortVillage$people$m_pid, hortVillage$people$pid)]
  hortVillage$people$mf_pid <- hortVillage$people$f_pid[match(hortVillage$people$m_pid, hortVillage$people$pid)]
  hortVillage$people$fm_pid <- hortVillage$people$m_pid[match(hortVillage$people$f_pid, hortVillage$people$pid)]
  hortVillage$people$ff_pid <- hortVillage$people$f_pid[match(hortVillage$people$f_pid, hortVillage$people$pid)]

  hortVillage$people$male[is.na(hortVillage$people$male)] <- 1
  hortVillage$people$age <- 0 - hortVillage$people$dob

  hortVillage$people$wealth <- rnorm(nrow(hortVillage$people), 0, 10) 
  # distribution can be varied

  hortVillage$households$ray_coord <- haversine(hortVillage$households$lat, 
    hortVillage$villages$lat[match(hortVillage$households$village, hortVillage$villages$village)], 
    hortVillage$households$long, hortVillage$villages$long[match(hortVillage$households$village, hortVillage$villages$village)])
  hortVillage$households$x_coord <- cartesian_map(hortVillage$households$lat, 
    hortVillage$villages$lat[match(hortVillage$households$village, hortVillage$villages$village)], 
    hortVillage$households$long, hortVillage$villages$long[match(hortVillage$households$village, hortVillage$villages$village)])$x
  hortVillage$households$y_coord <- cartesian_map(hortVillage$households$lat, 
    hortVillage$villages$lat[match(hortVillage$households$village, hortVillage$villages$village)], 
    hortVillage$households$long, hortVillage$villages$long[match(hortVillage$households$village, hortVillage$villages$village)])$y

  parameter_list <- list(
    baseline_probability = 1e-05,
    kin_network_effect = 0,
    neighbor_effect = 0,
    wealth_effect = 5
  )

  history <- diffuse( parameter_list, hortVillage, cognition="additive" )

  expect_true( nrow(history) > 0 )

})
