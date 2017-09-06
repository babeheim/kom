
# tests for calc_adoption_risk

rm(list=ls())

library(testthat)

library(devtools)
devtools::load_all()
library(kom)

data(hortVillage)

preg <- hortVillage$people # people
hreg <- hortVillage$households # households
vreg <- hortVillage$villages # villages

# adding these columns is necessary
preg$mm_pid <- preg$m_pid[match(preg$m_pid, preg$pid)]
preg$mf_pid <- preg$f_pid[match(preg$m_pid, preg$pid)]
preg$fm_pid <- preg$m_pid[match(preg$f_pid, preg$pid)]
preg$ff_pid <- preg$f_pid[match(preg$f_pid, preg$pid)]

hreg$ray_dist <- haversine(hreg$lat, vreg$lat[match(hreg$village, vreg$village)], hreg$long, vreg$long[match(hreg$village, vreg$village)], units="m")
hreg$x_coord <- cartesian_map(hreg$lat, vreg$lat[match(hreg$village, vreg$village)], hreg$long, vreg$long[match(hreg$village, vreg$village)], units="m")$x
hreg$y_coord <- cartesian_map(hreg$lat, vreg$lat[match(hreg$village, vreg$village)], hreg$long, vreg$long[match(hreg$village, vreg$village)], units="m")$y


preg$pr_adopt <- NA
preg$age <- 0 - preg$dob

preg$wealth <- rnorm(nrow(preg), 0, 10)




init_list <- list(
  n_seed_individuals = 5,
  seed_network_location = "random",
  simulation_duration_years = 1,
  phi_radius = 500,
  observation_rate = 30
)

parameter_list <- list(  
  baseline_probability = 1e-05,
  kin_network_effect = 0,
  town_distance_effect = 0,
  neighbor_effect = 10,
  wealth_effect = 0
)

######### Simulator Initializaiton #########

# initialize diffusion trait

preg$phi <- 0
can <- which( is.na(preg$dod) & preg$male==1 
  & preg$age >= 16*365 & preg$phi==0 )
if(length(can)==0) stop("no one is available to adopt the trait")
if(init_list$n_seed_individuals >= length(can)) stop("there are fewer people available than seed amount")
tar <- sample( can, init_list$n_seed_individuals )
preg$phi[tar] <- 1

####

day <- 1
days_max <- ceiling(365*init_list$simulation_duration_years)
should_simulate_day <- day <= days_max

#########

output <- list()

####### Simulator Daily Loop #######

can <- which( is.na(preg$dod) & preg$male==1 
  & preg$age >= 16*365 & preg$phi==0 )

risk <- preg[can,]

vreg$log_dist_town <- log(vreg$dist_town)

risk$log_dist_town <- vreg$log_dist_town[match(risk$village, vreg$village)]
risk$kin_has <- 0
risk$neighbor_has <- 0


for(i in 1:nrow(risk)){

  # identify kin phi status
  my_kin <- kin_retriever( risk$pid[i], preg )
  my_kin_rows <- which( preg$pid %in% my_kin )
  if( length( my_kin_rows ) > 0 ) risk$kin_has[i] <- any( 
    preg$phi[my_kin_rows]==1 )
  
  # identify neighbor phi status
  my_neighbors <- neighbor_finder( risk$pid[i], 
    preg, hreg, dist_radius = init_list$phi_radius, units="m")
  if( length( my_neighbors )>0 ) risk$neighbor_has[i] <- sum( 
    preg$phi[preg$pid %in% my_neighbors]==1 )

}


parameter_list$baseline_probability <- 1e-05
parameter_list$kin_network_effect <- 0
parameter_list$town_distance_effect <- 0
parameter_list$neighbor_effect <- 0
parameter_list$wealth_effect <- 0
test_that("returns valid values", {
  x <- calc_adoption_risk( parameter_list, risk, "additive" )
  expect_true( all(is.numeric(x)) )
})

parameter_list$baseline_probability <- 1e-05
parameter_list$kin_network_effect <- 10
parameter_list$town_distance_effect <- 0
parameter_list$neighbor_effect <- 0
parameter_list$wealth_effect <- 0
test_that("returns valid values", {
  x <- calc_adoption_risk( parameter_list, risk, "additive" )
  expect_true( all(is.numeric(x)) )
})

parameter_list$baseline_probability <- 1e-05
parameter_list$kin_network_effect <- 0
parameter_list$town_distance_effect <- 1.1
parameter_list$neighbor_effect <- 0
parameter_list$wealth_effect <- 0
test_that("returns valid values", {
  x <- calc_adoption_risk( parameter_list, risk, "additive" )
  expect_true( all(is.numeric(x)) )
})

parameter_list$baseline_probability <- 1e-05
parameter_list$kin_network_effect <- 0
parameter_list$town_distance_effect <- 0
parameter_list$neighbor_effect <- 10
parameter_list$wealth_effect <- 0
test_that("returns valid values", {
  x <- calc_adoption_risk( parameter_list, risk, "additive" )
  expect_true( all(is.numeric(x)) )
})

parameter_list$baseline_probability <- 1e-05
parameter_list$kin_network_effect <- 0
parameter_list$town_distance_effect <- 0
parameter_list$neighbor_effect <- 0
parameter_list$wealth_effect <- 10
test_that("returns valid values", {
  x <- calc_adoption_risk( parameter_list, risk, "additive" )
  expect_true( all(is.numeric(x)) )
})

parameter_list$baseline_probability <- 1e-02
parameter_list$kin_network_effect <- 0
parameter_list$town_distance_effect <- 0
parameter_list$neighbor_effect <- 0
parameter_list$wealth_effect <- 0
calc_adoption_risk( parameter_list, risk, "additive" )

parameter_list$kin_network_effect <- 10
parameter_list$town_distance_effect <- 0
parameter_list$neighbor_effect <- 0
parameter_list$wealth_effect <- 0
test_that("returns valid values", {
  x <- calc_adoption_risk( parameter_list, risk, "additive" )
  expect_true( all(is.numeric(x)) )
})

parameter_list$kin_network_effect <- 0
parameter_list$town_distance_effect <- 1.1
parameter_list$neighbor_effect <- 0
parameter_list$wealth_effect <- 0
test_that("returns valid values", {
  x <- calc_adoption_risk( parameter_list, risk, "additive" )
  expect_true( all(is.numeric(x)) )
})

parameter_list$kin_network_effect <- 0
parameter_list$town_distance_effect <- 0
parameter_list$neighbor_effect <- 10
parameter_list$wealth_effect <- 0
test_that("returns valid values", {
  x <- calc_adoption_risk( parameter_list, risk, "additive" )
  expect_true( all(is.numeric(x)) )
})

parameter_list$kin_network_effect <- 0
parameter_list$town_distance_effect <- 0
parameter_list$neighbor_effect <- 0
parameter_list$wealth_effect <- 10
test_that("returns valid values", {
  x <- calc_adoption_risk( parameter_list, risk, "additive" )
  expect_true( all(is.numeric(x)) )
})



## 

parameter_list$baseline_probability <- 1e-05
parameter_list$kin_network_effect <- 0
parameter_list$town_distance_effect <- 0
parameter_list$neighbor_effect <- 0
parameter_list$wealth_effect <- 0
test_that("returns valid values", {
  x <- calc_adoption_risk( parameter_list, risk, "multiplicative" )
  expect_true( all(is.numeric(x)) )
})

parameter_list$baseline_probability <- 1e-05
parameter_list$kin_network_effect <- 10
parameter_list$town_distance_effect <- 0
parameter_list$neighbor_effect <- 0
parameter_list$wealth_effect <- 0
test_that("returns valid values", {
  x <- calc_adoption_risk( parameter_list, risk, "multiplicative" )
  expect_true( all(is.numeric(x)) )
})

parameter_list$baseline_probability <- 1e-05
parameter_list$kin_network_effect <- 0
parameter_list$town_distance_effect <- 1.1
parameter_list$neighbor_effect <- 0
parameter_list$wealth_effect <- 0
test_that("returns valid values", {
  x <- calc_adoption_risk( parameter_list, risk, "multiplicative" )
  expect_true( all(is.numeric(x)) )
})

parameter_list$baseline_probability <- 1e-05
parameter_list$kin_network_effect <- 0
parameter_list$town_distance_effect <- 0
parameter_list$neighbor_effect <- 10
parameter_list$wealth_effect <- 0
test_that("returns valid values", {
  x <- calc_adoption_risk( parameter_list, risk, "multiplicative" )
  expect_true( all(is.numeric(x)) )
})


parameter_list$baseline_probability <- 1e-05
parameter_list$kin_network_effect <- 0
parameter_list$town_distance_effect <- 0
parameter_list$neighbor_effect <- 0
parameter_list$wealth_effect <- 10
test_that("returns valid values", {
  x <- calc_adoption_risk( parameter_list, risk, "multiplicative" )
  expect_true( all(is.numeric(x)) )
})

