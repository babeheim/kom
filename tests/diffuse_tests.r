

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

popdata$villages$dist_town <- 1000*popdata$villages$dist_town
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




parameter_list <- list(
  baseline_probability = 1e-05,
  kin_network_effect = 0,
  town_distance_effect = 0,
  neighbor_effect = 0,
  wealth_effect = 0
)

# it should chcek at that all variables exist at first

# normal sim
test_that("normal diffusion simulation", {
  x <- diffuse( parameter_list, popdata, quiet=FALSE )
  expect_true(nrow(x) > 0)
})

test_that("just one day, with yearly obs rate", {
  x <- diffuse( parameter_list, n_years = 1/365, census_period = 365, popdata )
  expect_equal( length(unique(x$date)), 1 )
})

test_that( "just one day!", {
  x <- diffuse( parameter_list, popdata, census_period=1, n_years=1/365, cognition="additive" , quiet=TRUE )
  expect_equal( length(unique(x$date)), 1 )
})

test_that( "daily censuses for 100 days", {
  history <- diffuse( parameter_list, census_period=1, n_year=100/365, popdata, cognition="additive" , quiet=TRUE )
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


# inputs: 
## pid - character, 
## pop_reg$household - character
## pop_reg$village - character
## pop_reg$pid - character
## house_reg$household - character
## house_reg$village - character

## dist_radius - numeric, > -0

## house_reg$x_coord - numeric 
## house_reg$y_coord - numeric

## fail if any are not above, convert to character
# also fail if trying to pass in NA values

## pass it a variety of locations on the globe...
## no! dont use haversine, just make it cartesian inputs


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
    town_distance_effect = 0,
    neighbor_effect = 3,
    wealth_effect = 0
  )

  # neighbor_radius is giving warnings but thats okay...at least it is running!

  history <- diffuse( parameter_list, popdata, cognition="additive" )

  expect_true( nrow(history) > 0 )

})




