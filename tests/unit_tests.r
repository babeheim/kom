

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







rm(list=ls())

library(testthat)

sapply( list.files('./R', full.names=TRUE), source )

preg <- read.csv('./data/population_register.csv', as.is=TRUE)
hreg <- read.csv('./data/household_register.csv', as.is=TRUE)
vreg <- read.csv('./data/village_register.csv', as.is=TRUE)

hreg$ray_dist <- haversine(hreg$lat, vreg$lat[match(hreg$village, vreg$village)], hreg$long, vreg$long[match(hreg$village, vreg$village)])
hreg$x_coord <- cartesian_map(hreg$lat, vreg$lat[match(hreg$village, vreg$village)], hreg$long, vreg$long[match(hreg$village, vreg$village)])$x
hreg$y_coord <- cartesian_map(hreg$lat, vreg$lat[match(hreg$village, vreg$village)], hreg$long, vreg$long[match(hreg$village, vreg$village)])$y

# errors in the dataset

n <- 1000

hreg[is.na(hreg$x_coord),]

drop <- which(is.na(hreg$lat) | is.na(hreg$long) | duplicated(hreg[,c("lat", "long")]))

bad_houses <- hreg$household[drop]

hreg <- hreg[-drop,]
preg <- preg[-which(preg$household %in% bad_houses),]


# # pid not in pop_reg
# my_pid <- "12344545"
# neighbor_finder(my_pid, preg, hreg, dist_radius=5000)


# # pid in pop_reg, but household is NA
# my_pid <- sample(preg$pid, 1)
# preg1 <- preg
# preg1$household[preg1$pid==my_pid] <- NA
# neighbor_finder(my_pid, preg1, hreg, dist_radius=5000)

# # pid in pop_reg, but household is not in house_reg
# my_pid <- sample(preg$pid, 1)
# preg1 <- preg
# preg1$household[preg1$pid==my_pid] <- "asdf"
# neighbor_finder(my_pid, preg1, hreg, dist_radius=5000)


# # random NA's in pid
# my_pid <- NA
# neighbor_finder(my_pid, preg, hreg, dist_radius=5000)


# print("tests for kin_retriever")

# # kin
# test <- "kin"
# x <- replicate(n, {
#   my_pid <- sample(preg$pid, 1)
#   out <- kin_retriever(my_pid, preg)
#   all(length(out)>=0)
# })
# if(all(x)) { print(paste0("pass: ", test)) 
#   } else print(paste0("fail: ", test))


# test_that("kin", {
  
#   x <- replicate(n, {
#     my_pid <- sample(preg$pid, 1)
#     out <- kin_retriever(my_pid, preg)
#     all(length(out)>=0)
#   })
#   expect_true(all(x))
#   expect_equal(str_length("ab"), 2)
#   expect_equal(str_length("abc"), 3)
# })




test_that("random NA's in pop_reg$household", {
  x <- replicate(n, {
    my_pid <- sample(preg$pid, 1)
    preg1 <- preg
    kill_entries <- sample(which(!preg1$pid %in% my_pid), 5)
    preg1$household[kill_entries] <- NA
    output1 <- neighbor_finder(my_pid, preg1, hreg, dist_radius=5000)
    preg2 <- preg1[-which(is.na(preg1$household)),]
    output2 <- neighbor_finder(my_pid, preg2, hreg, dist_radius=5000)
    identical_new(output1, output2)
  })
  expect_true(all(x))
})

test_that("popreg has only one entry", {
  x <- replicate(n, {
    my_pid <- sample(preg$pid, 1)
    preg1 <- preg
    kill_entries <- sample(which(!preg1$pid %in% my_pid))
    preg1$household[kill_entries] <- NA
    output1 <- neighbor_finder(my_pid, preg1, hreg, dist_radius=5000)
    preg2 <- preg1[-which(is.na(preg1$household)),]
    output2 <- neighbor_finder(my_pid, preg2, hreg, dist_radius=5000)
    identical_new(output1, output2)
  })
  expect_true(all(x))
})

test_that("random NA's in pop_reg$village and pop_reg$household", {
  x <- replicate(n, {
    my_pid <- sample(preg$pid, 1)
    my_village <- preg$village[which(preg$pid==my_pid)]
    preg1 <- preg
    kill_entries <- sample(which(!preg1$pid %in% my_pid), 5)
    preg1$village[kill_entries] <- NA
    preg1$household[kill_entries] <- NA
    output1 <- neighbor_finder(my_pid, preg1, hreg, dist_radius=5000)
    preg2 <- preg1[-which(is.na(preg1$village)),]
    output2 <- neighbor_finder(my_pid, preg2, hreg, dist_radius=5000)
    identical_new(output1, output2)
  })
  expect_true(all(x))
})

test_that("only person from village is ego", {
  x <- replicate(n, {
    my_pid <- sample(preg$pid, 1)
    my_village <- preg$village[preg$pid==my_pid]
    village_folk <- which(preg$pid != my_pid & preg$village == my_village)
    preg1 <- preg[-village_folk,]
    output1 <- neighbor_finder(my_pid, preg1, hreg, dist_radius=5000)
    length(output1)==0
  })
  expect_true(all(x))
})

test_that("random NA's in house_reg$household", {
  x <- replicate(n, {
    my_pid <- sample(preg$pid, 1)
    my_household <- preg$household[preg$pid==my_pid]
    hreg1 <- hreg
    kill_entries <- sample(which(!hreg1$household %in% my_household), 5)
    hreg1$household[kill_entries] <- NA
    output1 <- neighbor_finder(my_pid, preg, hreg1, dist_radius=5000)
    hreg2 <- hreg1[-which(is.na(hreg1$household)),]
    output2 <- neighbor_finder(my_pid, preg, hreg2, dist_radius=5000)
    identical_new(output1, output2)
  })
  expect_true(all(x))
})

test_that( "random NA's in house_reg$village", {
  x <- replicate(n, {
    my_pid <- sample(preg$pid, 1)
    my_household <- preg$household[preg$pid==my_pid]
    hreg1 <- hreg
    kill_entries <- sample(which(!hreg1$household %in% my_household), 5)
    hreg1$village[kill_entries] <- NA
    output1 <- neighbor_finder(my_pid, preg, hreg1, dist_radius=5000)
    hreg2 <- hreg1[-which(is.na(hreg1$village)),]
    output2 <- neighbor_finder(my_pid, preg, hreg2, dist_radius=5000)
    identical_new(output1, output2)
  })
  expect_true( all(x) )
})

test_that( "all neighbors actually live in this community" , {
  x <- replicate(n,{
    my_pid <- sample(preg$pid, 1)
    my_village <- preg$village[preg$pid==my_pid]
    test <- neighbor_finder(my_pid, preg, hreg, dist_radius=5000)
    all(test %in% preg$pid[preg$village==my_village])
  })
  expect_true( all(x) )
})

test_that( "too-short distance returns character(0)" , {
  x <- replicate(n,{
    my_pid <- sample(preg$pid, 1)
    my_village <- preg$village[preg$pid==my_pid]
    test <- neighbor_finder(my_pid, preg, hreg, dist_radius=1)
    length(test)==0
  })
  expect_true( all(x) )
})

test_that( "wide radius is all but focal household" , {
  x <- replicate(n,{
    my_pid <- sample(preg$pid, 1)
    my_household <- preg$household[preg$pid==my_pid]
    my_household_residents <- preg$pid[preg$household==my_household]
    my_village <- preg$village[preg$pid==my_pid]
    all_residents <- preg$pid[preg$village==my_village]
    test <- neighbor_finder(my_pid, preg, hreg, dist_radius=100000)
    identical_new( c(my_household_residents,test), all_residents ) 
  }) 
  expect_true( all(x) )
})

test_that( "random pid random distance", {
  x <- replicate(n, {
    my_pid <- sample(preg$pid, 1)
    my_radius <- rexp(1 ,1/3000 )
    output <- neighbor_finder(my_pid, preg, hreg, dist_radius=my_radius)
    length(output)>=0
  })
  expect_true( all(x) )
})

test_that( "there is no one else in the village", {
  x <- replicate(n,{
    my_pid <- sample(preg$pid, 1)
    my_village <- preg$village[preg$pid==my_pid]
    all_residents <- preg$pid[preg$village==my_village & preg$pid!=my_pid]
    preg1 <- preg[-which(preg$pid %in% all_residents),]
    output <- neighbor_finder(my_pid, preg1, hreg, dist_radius=100000)
    length(output)==0
  }) 
  expect_true( all(x) )
})

test_that( "everyone in the village lives in the same household", {
  x <- replicate(n,{
    my_pid <- sample(preg$pid, 1)
    my_village <- preg$village[preg$pid==my_pid]
    my_household <- preg$household[preg$pid==my_pid]
    all_residents <- preg$pid[preg$village==my_village & preg$pid!=my_pid]
    preg1 <- preg
    preg1$household[which(preg1$pid %in% all_residents)] <- my_household
    output <- neighbor_finder(my_pid, preg1, hreg, dist_radius=100000)
    length(output)==0
  })
  expect_true( all(x) )
})
