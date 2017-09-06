

# unit testing for my functions


library(testthat)

# create_risk_table has problems, dammit!

# here's where it goes wrong

rm(list=ls())

# load in the kom diffusion library
sapply( list.files('./R', full.names=TRUE), source)

# pre-compile the stan models before parallelizing
fit_add <- stan_model( file="./stan/additive_model.stan" )
# fit_multi <- stan_model( file="./stan/multiplicative_model.stan" )

## 

test_that( "neighbor_has works", {

  popdata <- list()
  popdata$preg <- read.csv("./data/population_register.csv", stringsAsFactors=FALSE)
  popdata$hreg <- read.csv("./data/household_register.csv", stringsAsFactors=FALSE)
  popdata$vreg <- read.csv("./data/village_register.csv", stringsAsFactors=FALSE)

  popdata$preg$mm_pid <- popdata$preg$m_pid[match(popdata$preg$m_pid, popdata$preg$pid)]
  popdata$preg$mf_pid <- popdata$preg$f_pid[match(popdata$preg$m_pid, popdata$preg$pid)]
  popdata$preg$fm_pid <- popdata$preg$m_pid[match(popdata$preg$f_pid, popdata$preg$pid)]
  popdata$preg$ff_pid <- popdata$preg$f_pid[match(popdata$preg$f_pid, popdata$preg$pid)]

  keep <- which(popdata$preg$village=="7") # oh...
  popdata$preg <- popdata$preg[keep,]
  keep <- which(popdata$hreg$village=="7")
  popdata$hreg <- popdata$hreg[keep,]

  popdata$preg$male[is.na(popdata$preg$male)] <- 1
  popdata$preg$age <- 0 - popdata$preg$dob

  popdata$preg$wealth <- rnorm(nrow(popdata$preg), 0, 10) 
  # distribution can be varied

  popdata$vreg$dist_town <- 1000*popdata$vreg$dist_town
  popdata$vreg$log_dist_town <- log(popdata$vreg$dist_town)

  popdata$hreg$ray_coord <- haversine(popdata$hreg$lat, 
    popdata$vreg$lat[match(popdata$hreg$village, popdata$vreg$village)], 
    popdata$hreg$long, popdata$vreg$long[match(popdata$hreg$village, popdata$vreg$village)])
  popdata$hreg$x_coord <- cartesian_map(popdata$hreg$lat, 
    popdata$vreg$lat[match(popdata$hreg$village, popdata$vreg$village)], 
    popdata$hreg$long, popdata$vreg$long[match(popdata$hreg$village, popdata$vreg$village)])$x
  popdata$hreg$y_coord <- cartesian_map(popdata$hreg$lat, 
    popdata$vreg$lat[match(popdata$hreg$village, popdata$vreg$village)], 
    popdata$hreg$long, popdata$vreg$long[match(popdata$hreg$village, popdata$vreg$village)])$y

  # simulation

  init_list <- list(
    n_seed_individuals = 5,
    seed_network_location = "random",
    simulation_duration_years = 4,
    phi_radius = 1000,
    baseline_probability = 1e-05,
    kin_network_effect = 3,
    town_distance_effect = 0,
    neighbor_effect = 3,
    wealth_effect = 0,
    observation_rate = 365,
    event_logging = FALSE
  )

  history <- diffuse( init_list, popdata, cognition="additive" )

  risk <- create_risk_table( init_list, popdata, history )

  expect_true( any(risk$neighbor_has > 0) )

})




# tests for calc_adoption_risk

rm(list=ls())

sapply( list.files('./R', full.names=TRUE), source )

# We want to pass in a bucnh fo different scenarios, 
# ensure they are being calculated correctly and

## setup

popdata <- list()
popdata$preg <- read.csv("./data/population_register.csv", stringsAsFactors=FALSE)
popdata$hreg <- read.csv("./data/household_register.csv", stringsAsFactors=FALSE)
popdata$vreg <- read.csv("./data/village_register.csv", stringsAsFactors=FALSE)

popdata$preg$mm_pid <- popdata$preg$m_pid[match(popdata$preg$m_pid, popdata$preg$pid)]
popdata$preg$mf_pid <- popdata$preg$f_pid[match(popdata$preg$m_pid, popdata$preg$pid)]
popdata$preg$fm_pid <- popdata$preg$m_pid[match(popdata$preg$f_pid, popdata$preg$pid)]
popdata$preg$ff_pid <- popdata$preg$f_pid[match(popdata$preg$f_pid, popdata$preg$pid)]

keep <- which(popdata$preg$village=="7") # oh...
popdata$preg <- popdata$preg[keep,]
keep <- which(popdata$hreg$village=="7")
popdata$hreg <- popdata$hreg[keep,]

popdata$preg$male[is.na(popdata$preg$male)] <- 1
popdata$preg$age <- 0 - popdata$preg$dob

popdata$preg$wealth <- rnorm(nrow(popdata$preg), 0, 10) 
# distribution can be varied

popdata$vreg$dist_town <- 1000*popdata$vreg$dist_town
popdata$vreg$log_dist_town <- log(popdata$vreg$dist_town)

popdata$hreg$ray_coord <- haversine(popdata$hreg$lat, 
  popdata$vreg$lat[match(popdata$hreg$village, popdata$vreg$village)], 
  popdata$hreg$long, popdata$vreg$long[match(popdata$hreg$village, popdata$vreg$village)])
popdata$hreg$x_coord <- cartesian_map(popdata$hreg$lat, 
  popdata$vreg$lat[match(popdata$hreg$village, popdata$vreg$village)], 
  popdata$hreg$long, popdata$vreg$long[match(popdata$hreg$village, popdata$vreg$village)])$x
popdata$hreg$y_coord <- cartesian_map(popdata$hreg$lat, 
  popdata$vreg$lat[match(popdata$hreg$village, popdata$vreg$village)], 
  popdata$hreg$long, popdata$vreg$long[match(popdata$hreg$village, popdata$vreg$village)])$y

init_list <- list(
  n_seed_individuals = 5,
  seed_network_location = "random",
  simulation_duration_years = 1,
  phi_radius = 500,
  baseline_probability = 1e-05,
  kin_network_effect = 0,
  town_distance_effect = 0,
  neighbor_effect = 10,
  wealth_effect = 0,
  observation_rate = 30
)

pop_data <- popdata

preg <- pop_data$preg # people
hreg <- pop_data$hreg # households
vreg <- pop_data$vreg # villages

preg$pr_adopt <- NA

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

risk$log_dist_town <- vreg$log_dist_town[risk$village]
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
    preg, hreg, dist_radius = init_list$phi_radius)
  if( length( my_neighbors )>0 ) risk$neighbor_has[i] <- sum( 
    preg$phi[preg$pid %in% my_neighbors]==1 )

}


init_list$baseline_probability <- 1e-05
init_list$kin_network_effect <- 0
init_list$town_distance_effect <- 0
init_list$neighbor_effect <- 0
init_list$wealth_effect <- 0
calc_adoption_risk( init_list, risk, "additive" )

init_list$baseline_probability <- 1e-05
init_list$kin_network_effect <- 10
init_list$town_distance_effect <- 0
init_list$neighbor_effect <- 0
init_list$wealth_effect <- 0
calc_adoption_risk( init_list, risk, "additive" )

init_list$baseline_probability <- 1e-05
init_list$kin_network_effect <- 0
init_list$town_distance_effect <- 1.1
init_list$neighbor_effect <- 0
init_list$wealth_effect <- 0
calc_adoption_risk( init_list, risk, "additive" )

init_list$baseline_probability <- 1e-05
init_list$kin_network_effect <- 0
init_list$town_distance_effect <- 0
init_list$neighbor_effect <- 10
init_list$wealth_effect <- 0
calc_adoption_risk( init_list, risk, "additive" )

init_list$baseline_probability <- 1e-05
init_list$kin_network_effect <- 0
init_list$town_distance_effect <- 0
init_list$neighbor_effect <- 0
init_list$wealth_effect <- 10
calc_adoption_risk( init_list, risk, "additive" )



init_list$baseline_probability <- 1e-02
init_list$kin_network_effect <- 0
init_list$town_distance_effect <- 0
init_list$neighbor_effect <- 0
init_list$wealth_effect <- 0
calc_adoption_risk( init_list, risk, "additive" )

init_list$kin_network_effect <- 10
init_list$town_distance_effect <- 0
init_list$neighbor_effect <- 0
init_list$wealth_effect <- 0
calc_adoption_risk( init_list, risk, "additive" )

init_list$kin_network_effect <- 0
init_list$town_distance_effect <- 1.1
init_list$neighbor_effect <- 0
init_list$wealth_effect <- 0
calc_adoption_risk( init_list, risk, "additive" )

init_list$kin_network_effect <- 0
init_list$town_distance_effect <- 0
init_list$neighbor_effect <- 10
init_list$wealth_effect <- 0
calc_adoption_risk( init_list, risk, "additive" )

init_list$kin_network_effect <- 0
init_list$town_distance_effect <- 0
init_list$neighbor_effect <- 0
init_list$wealth_effect <- 10
calc_adoption_risk( init_list, risk, "additive" )



## 

init_list$baseline_probability <- 1e-05
init_list$kin_network_effect <- 0
init_list$town_distance_effect <- 0
init_list$neighbor_effect <- 0
init_list$wealth_effect <- 0
calc_adoption_risk( init_list, risk, "multiplicative" )

init_list$baseline_probability <- 1e-05
init_list$kin_network_effect <- 10
init_list$town_distance_effect <- 0
init_list$neighbor_effect <- 0
init_list$wealth_effect <- 0
calc_adoption_risk( init_list, risk, "multiplicative" )

init_list$baseline_probability <- 1e-05
init_list$kin_network_effect <- 0
init_list$town_distance_effect <- 1.1
init_list$neighbor_effect <- 0
init_list$wealth_effect <- 0
calc_adoption_risk( init_list, risk, "multiplicative" )

init_list$baseline_probability <- 1e-05
init_list$kin_network_effect <- 0
init_list$town_distance_effect <- 0
init_list$neighbor_effect <- 10
init_list$wealth_effect <- 0
calc_adoption_risk( init_list, risk, "multiplicative" )

init_list$baseline_probability <- 1e-05
init_list$kin_network_effect <- 0
init_list$town_distance_effect <- 0
init_list$neighbor_effect <- 0
init_list$wealth_effect <- 10
calc_adoption_risk( init_list, risk, "multiplicative" )






# tests for 'diffuse' function

rm(list=ls())

sapply( list.files('./R', full.names=TRUE), source )

popdata <- list()
popdata$preg <- read.csv("./data/population_register.csv", stringsAsFactors=FALSE)
popdata$hreg <- read.csv("./data/household_register.csv", stringsAsFactors=FALSE)
popdata$vreg <- read.csv("./data/village_register.csv", stringsAsFactors=FALSE)

popdata$preg$mm_pid <- popdata$preg$m_pid[match(popdata$preg$m_pid, popdata$preg$pid)]
popdata$preg$mf_pid <- popdata$preg$f_pid[match(popdata$preg$m_pid, popdata$preg$pid)]
popdata$preg$fm_pid <- popdata$preg$m_pid[match(popdata$preg$f_pid, popdata$preg$pid)]
popdata$preg$ff_pid <- popdata$preg$f_pid[match(popdata$preg$f_pid, popdata$preg$pid)]

keep <- which(popdata$preg$village=="7") # oh...
popdata$preg <- popdata$preg[keep,]
keep <- which(popdata$hreg$village=="7")
popdata$hreg <- popdata$hreg[keep,]

popdata$preg$male[is.na(popdata$preg$male)] <- 1
popdata$preg$age <- 0 - popdata$preg$dob

popdata$preg$wealth <- rnorm(nrow(popdata$preg), 0, 10) 
# distribution can be varied

popdata$vreg$dist_town <- 1000*popdata$vreg$dist_town
popdata$vreg$log_dist_town <- log(popdata$vreg$dist_town)

popdata$hreg$ray_coord <- haversine(popdata$hreg$lat, 
  popdata$vreg$lat[match(popdata$hreg$village, popdata$vreg$village)], 
  popdata$hreg$long, popdata$vreg$long[match(popdata$hreg$village, popdata$vreg$village)])
popdata$hreg$x_coord <- cartesian_map(popdata$hreg$lat, 
  popdata$vreg$lat[match(popdata$hreg$village, popdata$vreg$village)], 
  popdata$hreg$long, popdata$vreg$long[match(popdata$hreg$village, popdata$vreg$village)])$x
popdata$hreg$y_coord <- cartesian_map(popdata$hreg$lat, 
  popdata$vreg$lat[match(popdata$hreg$village, popdata$vreg$village)], 
  popdata$hreg$long, popdata$vreg$long[match(popdata$hreg$village, popdata$vreg$village)])$y




init_list <- list(
  n_seed_individuals = 5,
  seed_network_location = "random",
  simulation_duration_years = 1,
  phi_radius = 500,
  baseline_probability = 1e-05,
  kin_network_effect = 0,
  town_distance_effect = 0,
  neighbor_effect = 0,
  wealth_effect = 0,
  observation_rate = 30
)

# it should chcek at that all variables exist at first

# normal sim
test_that("normal diffusion simulation", {
  x <- diffuse( init_list, popdata )
  expect_true(nrow(x) > 0)
})

test_that("just one day, with yearly obs rate", {
  init_list$observation_rate <- 365
  init_list$simulation_duration_years <- 1/365
  x <- diffuse( init_list, popdata )
  expect_equal( length(unique(x$date)), 1 )
})

test_that( "just one day!", {
  init_list$observation_rate <- 1
  init_list$simulation_duration_years <- 1/365
  x <- diffuse( init_list, popdata, cognition="additive" , quiet=TRUE )
  expect_equal( length(unique(x$date)), 1 )
})

test_that( "daily censuses for 100 days", {
  init_list$observation_rate <- 1
  init_list$simulation_duration_years <- 100/365
  history <- diffuse( init_list, popdata, cognition="additive" , quiet=TRUE )
  expect_equal( length(unique(history$date)), 100 )
})



# exactly 2 censuses

test_that( "exactly 2 censuses, 10 days apart", {
  init_list$observation_rate <- 10
  init_list$simulation_duration_years <- 10/365
  history <- diffuse( init_list, popdata )
  expect_equal( length(unique(history$date)), 2 )
})

test_that( "exactly 2 censuses, 50 days apart", {
  init_list$observation_rate <- 50
  init_list$simulation_duration_years <- 50/365
  history <- diffuse( init_list, popdata )
  expect_equal( length(unique(history$date)), 2 )
})

test_that( "exactly 2 censuses, 1 year apart", {
  init_list$observation_rate <- 365
  init_list$simulation_duration_years <- 365/365
  history <- diffuse( init_list, popdata )
  expect_equal( length(unique(history$date)), 2 )
})

test_that( "exactly 5 censuses in 5 days", {
  init_list$observation_rate <- 1
  init_list$simulation_duration_years <- 5/365
  history <- diffuse( init_list, popdata )
  expect_equal( length(unique(history$date)), 5 )
})

test_that( "exactly 5 censuses in 20 days", {
  init_list$observation_rate <- 5
  init_list$simulation_duration_years <- 20/365
  history <- diffuse( init_list, popdata )
  expect_equal( length(unique(history$date)), 5 )
})

test_that( "exactly 10 censuses in 90 days", {
  init_list$observation_rate <- 10
  init_list$simulation_duration_years <- 90/365
  history <- diffuse( init_list, popdata )
  expect_equal( length(unique(history$date)), 10 )
})

test_that("not a valid cognitive model", {
  expect_error(diffuse( init_list, popdata, cognition="additive2"))
})

test_that("too many seed people", {
  init_list$n_seed_individuals = nrow(popdata$preg) + 1
  expect_error(diffuse( init_list, popdata, cognition="additive" ))
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





# more neighbor_finder tests

test_that( "this combo works", {

  # load in the kom diffusion library
  sapply( list.files('./R', full.names=TRUE), source)

  ## 

  popdata <- list()
  popdata$preg <- read.csv("./data/population_register.csv", stringsAsFactors=FALSE)
  popdata$hreg <- read.csv("./data/household_register.csv", stringsAsFactors=FALSE)
  popdata$vreg <- read.csv("./data/village_register.csv", stringsAsFactors=FALSE)

  popdata$preg$mm_pid <- popdata$preg$m_pid[match(popdata$preg$m_pid, popdata$preg$pid)]
  popdata$preg$mf_pid <- popdata$preg$f_pid[match(popdata$preg$m_pid, popdata$preg$pid)]
  popdata$preg$fm_pid <- popdata$preg$m_pid[match(popdata$preg$f_pid, popdata$preg$pid)]
  popdata$preg$ff_pid <- popdata$preg$f_pid[match(popdata$preg$f_pid, popdata$preg$pid)]

  keep <- which(popdata$preg$village=="7") # oh...
  popdata$preg <- popdata$preg[keep,]
  keep <- which(popdata$hreg$village=="7")
  popdata$hreg <- popdata$hreg[keep,]

  popdata$preg$male[is.na(popdata$preg$male)] <- 1
  popdata$preg$age <- 0 - popdata$preg$dob

  popdata$preg$wealth <- rnorm(nrow(popdata$preg), 0, 10) 
  # distribution can be varied

  popdata$vreg$dist_town <- 1000*popdata$vreg$dist_town
  popdata$vreg$log_dist_town <- log(popdata$vreg$dist_town)

  popdata$hreg$ray_coord <- haversine(popdata$hreg$lat, 
    popdata$vreg$lat[match(popdata$hreg$village, popdata$vreg$village)], 
    popdata$hreg$long, popdata$vreg$long[match(popdata$hreg$village, popdata$vreg$village)])
  popdata$hreg$x_coord <- cartesian_map(popdata$hreg$lat, 
    popdata$vreg$lat[match(popdata$hreg$village, popdata$vreg$village)], 
    popdata$hreg$long, popdata$vreg$long[match(popdata$hreg$village, popdata$vreg$village)])$x
  popdata$hreg$y_coord <- cartesian_map(popdata$hreg$lat, 
    popdata$vreg$lat[match(popdata$hreg$village, popdata$vreg$village)], 
    popdata$hreg$long, popdata$vreg$long[match(popdata$hreg$village, popdata$vreg$village)])$y

  # simulation

  init_list <- list(
    n_seed_individuals = 5,
    seed_network_location = "random",
    simulation_duration_years = 4,
    phi_radius = 1000,
    baseline_probability = 1e-05,
    kin_network_effect = 30,
    town_distance_effect = 0,
    neighbor_effect = 3,
    wealth_effect = 0,
    observation_rate = 365,
    event_logging = FALSE
  )

  # bug with the above parameters

  history <- diffuse( init_list, popdata, cognition="additive" )

  expect_true( nrow(history) > 0 )

})




