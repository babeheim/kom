

rm(list=ls())

library(testthat)

library(devtools)
devtools::load_all()
library(kom)

data(hortVillage)


## create_risk_table tests

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

  history <- diffuse( parameter_list, popdata, cognition="additive" )

  risk <- create_risk_table( popdata, history, neighbor_radius=500 )

  expect_true( any(risk$neighbor_has > 0) )

})

